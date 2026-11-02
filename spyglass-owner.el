;;; spyglass-owner.el --- Share buffers with spyglass peers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The owner's side: a share is a buffer offered to peers; each mirror
;; a peer opens on it is a subscription.  The owner is authoritative:
;; it tells each viewer which ranges of its copy to replace, and with
;; what text and properties.
;;
;; Capture.  A subscription keeps the ranges of the buffer its viewer
;; has not seen yet, as intervals (B BL VL): BL characters at B in the
;; buffer replace VL characters in the viewer's copy.  New changes are
;; composed into them (`spyglass--intervals-compose'), so that however
;; slow a viewer is, what the owner holds for it stays bounded: at most
;; `spyglass-max-intervals' ranges, whose text is read from the buffer
;; when sent, never queued.
;;
;; Changes are found in one of two ways, chosen per share:
;;
;; - `shadow': a hidden copy of the buffer, as last captured, with its
;;   properties and mirrored overlays.  Markers spread through the
;;   buffer (sentinels) split it into segments; insertions and deletions
;;   move them, even when made with `inhibit-modification-hooks', so a
;;   segment whose length changed is known to have changed.  Hints
;;   (`after-change-functions', `spyglass-notify', jit-lock, adapters)
;;   name where rewrites of equal length may hide.  Each changed segment
;;   is compared with the shadow, ends first, then line by line, to
;;   send only what differs.  Unhinted changes made without hooks are
;;   found by comparing every segment, at most every
;;   `spyglass-poll-interval'; a slow sweep compares the rest, so that
;;   anything missed is eventually sent.  Used up to
;;   `spyglass-shadow-limit' characters.
;; - `direct': for larger buffers, `after-change-functions' alone.  A
;;   change of text made without hooks is noticed through
;;   `buffer-chars-modified-tick' and resent whole; overlays are not
;;   mirrored.
;;
;; Flow control.  Frames a viewer has not acknowledged count against
;; `spyglass-window'; past it the owner sends nothing more and lets
;; changes coalesce.  Those frames are kept, so that a viewer coming
;; back after a broken stream resumes from the last one it applied.

;;; Code:

(require 'cl-lib)
(require 'spyglass-wire)
(require 'spyglass-conn)

(declare-function jit-lock-fontify-now "jit-lock" (&optional start end))
(declare-function comint-send-input "comint" (&optional no-newline artificial))
(declare-function comint-interrupt-subjob "comint" ())
(declare-function comint-send-eof "comint" ())
(declare-function comint-kill-input "comint" ())
(declare-function comint-previous-input "comint" (arg))
(declare-function comint-next-input "comint" (arg))
(defvar jit-lock-mode)
(defvar jit-lock-functions)

;;;; Options

(defcustom spyglass-flush-delay 0.016
  "Seconds changes gather before being sent, about a frame of redisplay."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-poll-interval 0.05
  "Seconds between checks for changes made without modification hooks."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-sweep-interval 0.25
  "Seconds between steps of the sweep comparing a share with its shadow."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-sweep-chars 32768
  "Characters compared at each step of the sweep."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-shadow-limit (* 16 1024 1024)
  "Largest buffer, in characters, captured with a shadow copy.
Larger buffers are captured through modification hooks alone."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-frame-chars 65536
  "Most characters of text sent in one frame."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-window (* 4 1024 1024)
  "Bytes sent to a viewer and not yet acknowledged, past which sending waits.
It bounds throughput to a window per round trip: 4 MB carry 50 MB/s
over SSH on a local network.  Larger windows let writes wait more
often, and while a write waits Emacs reads no keyboard input."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-tick-budget 0.004
  "Seconds of sending per flush before yielding to the command loop.
Every viewer gets at least a frame per flush; what remains is sent by
the next flush, at once."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-max-intervals 64
  "Most ranges kept pending for a viewer; nearer ones merge past it."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-resume-timeout 300.0
  "Seconds a subscription waits for its viewer to come back."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-text-properties '(face invisible display line-prefix wrap-prefix)
  "Text properties mirrored, among `spyglass-wire-text-properties'.
`font-lock-face' is mirrored as `face' where it shows."
  :group 'spyglass
  :type '(set (const face) (const invisible) (const display)
              (const line-prefix) (const wrap-prefix) (const help-echo)))

(defcustom spyglass-overlay-properties
  '(face invisible display before-string after-string priority)
  "Overlay properties mirrored, among `spyglass-wire-overlay-properties'.
Overlays with none of these but `priority' are not mirrored."
  :group 'spyglass
  :type '(set (const face) (const invisible) (const display)
              (const before-string) (const after-string)
              (const help-echo) (const priority)))

(defcustom spyglass-ignored-overlay-faces
  '(region secondary-selection hl-line show-paren-match show-paren-mismatch
           show-paren-match-expression isearch lazy-highlight)
  "Faces of overlays that belong to the owner's windows, not to its text.
Overlays with one of these faces, or a `window' property, are not
mirrored."
  :group 'spyglass
  :type '(repeat face))

(defvar spyglass-share-functions nil
  "Abnormal hook run with a share just made.
Adapters set the buffer's input, point and notification there.")

(defvar spyglass-share-state-functions nil
  "Abnormal hook run with (SHARE EVENT DATA) on a share's events.
EVENT is `subscribed' or `unsubscribed' with the viewer's key, or
`unshared'.")

(defvar-local spyglass-input-function nil
  "Function applying a viewer's input in this buffer, or nil for the default.
Called in the buffer with (KIND VALUE): KIND `string' and VALUE text
to type, or KIND `key' and VALUE a key in `key-description' form,
such as \"RET\" or \"<up>\".  See `spyglass-default-input'.")

(defvar-local spyglass-submit-function nil
  "Command a viewer's RET runs in this comint buffer, or nil.
Nil means `comint-send-input'.  A front end such as agent-shell sets
its own submit command here.")

(defvar-local spyglass-point-function nil
  "Function returning the position viewers follow, or nil for point.
Terminal adapters return the terminal's cursor.")

(defvar-local spyglass--share nil
  "The share of this buffer, or nil.")
(put 'spyglass--share 'permanent-local t)

;;;; Samples

(defun spyglass--ring (&optional size)
  "Return an empty ring of SIZE (4096 by default) numbers."
  (vector (make-vector (or size 4096) 0.0) 0 0))

(defun spyglass--ring-add (ring value)
  "Add VALUE to RING."
  (let ((v (aref ring 0)) (i (aref ring 1)))
    (aset v i value)
    (aset ring 1 (% (1+ i) (length v)))
    (aset ring 2 (min (length v) (1+ (aref ring 2))))))

(defun spyglass-ring-summary (ring)
  "Return the count, median, 99th percentile and maximum of RING, as a plist."
  (let* ((n (aref ring 2))
         (sorted (sort (cl-subseq (aref ring 0) 0 n) #'<)))
    (if (zerop n)
        (list :n 0)
      (list :n n
            :p50 (aref sorted (/ n 2))
            :p99 (aref sorted (min (1- n) (floor (* n 0.99))))
            :max (aref sorted (1- n))))))

;;;; Records

(cl-defstruct (spyglass-share (:constructor spyglass--make-share)
                              (:copier nil))
  "A buffer shared with peers."
  (id nil :documentation "Identifier peers open it by.")
  (buffer nil :documentation "The buffer.")
  (read-only nil :documentation "Non-nil: viewers' input is refused.")
  (input nil :documentation "Input function overriding the buffer's.")
  (capture 'auto :documentation "auto, shadow or direct.")
  (subs nil :documentation "Subscriptions, live or waiting for their viewer.")
  (mode nil :documentation "shadow or direct while subscribed, else nil.")
  (shadow nil :documentation "Copy of the buffer as last captured.")
  (sentinels nil :documentation "Vector of markers splitting the buffer.")
  (synced nil :documentation "Vector of the sentinels' positions in the shadow.")
  (spread-size 0 :documentation "Buffer size when sentinels were spread.")
  (hints nil :documentation "Hinted regions, as conses of markers, newest first.")
  (hint-all nil :documentation "Non-nil: compare everything at the next capture.")
  (props-hinted nil :documentation "Non-nil: a hint came without hooks since the last capture.")
  (pending nil :documentation "Direct mode: intervals changed since the last capture.")
  (resync nil :documentation "Direct mode: resend the whole buffer.")
  (size 0 :documentation "Buffer size at the last capture.")
  (chars-tick 0 :documentation "`buffer-chars-modified-tick' at the last capture.")
  (modiff 0 :documentation "`buffer-modified-tick' at the last capture.")
  (hook-chars-tick 0 :documentation "Chars tick after the last change known of.")
  (hook-modiff 0 :documentation "Modification tick after the last change known of.")
  (change-time nil :documentation "When the first change since the last capture came.")
  (ovs nil :documentation "Hash table of mirrored overlays and their records.")
  (next-ov 0 :documentation "Last overlay identifier.")
  (scan-time 0.0 :documentation "When the last comparison of the whole buffer ended.")
  (scan-cost 0.0 :documentation "Seconds the last comparison of the whole buffer took.")
  (sweep-pos 1 :documentation "Where the sweep continues.")
  (sweeps 0 :documentation "Sweeps of the whole buffer completed.")
  (sweep-time 0.0 :documentation "When the sweep last ran.")
  (font-lock-face :unknown :documentation "Whether `font-lock-face' showed, as last computed.")
  (point nil :documentation "Position followed, as last computed.")
  (window-start nil :documentation "Window start of the owner, as last computed.")
  (meta nil :documentation "Description sent to viewers, as last computed.")
  (meta-time 0.0 :documentation "When meta was computed.")
  (styles nil :documentation "Hash table of raw property sets and their sanitized form.")
  (jit nil :documentation "Non-nil when registered with jit-lock.")
  (needs-window 'auto :documentation "t, nil, or auto for the buffer's `spyglass-needs-window'.")
  (parked nil :documentation "The frame the buffer is parked in, or nil.")
  (park-size nil :documentation "Cells (COLS . ROWS) of the parking window.")
  (target nil :documentation "Size its viewers gave the terminal, or nil for its own.")
  (grid nil :documentation "The terminal's size as last decided, (COLS . ROWS).")
  (owner-active 0.0 :documentation "When the owner last used or selected the buffer.")
  (counters nil :documentation "Plist of counters and samples."))

(cl-defstruct (spyglass--sub (:constructor spyglass--make-sub) (:copier nil))
  "A viewer's subscription to a share."
  key id connection share epoch
  (seq 0) (acked 0) (inflight 0) unacked
  intervals ct reset
  styles (next-style 0) faces ov-dirty
  point window-start meta orphaned
  (size nil :documentation "Cells (COLS . ROWS) of the viewer's window, or nil.")
  (active 0.0 :documentation "When the viewer last typed or selected its mirror."))

(defvar spyglass--sub-functions nil
  "Functions called with (SUB EVENT) on a subscription's events.
EVENT is `opened', `resumed', `orphaned', `freed' or `input'.  For
spyglass-window.el, which sizes and parks shared buffers.")

(defvar spyglass--poll-functions nil
  "Functions called with each subscribed share when polling.
Called in the share's buffer.")

(defvar spyglass--meta-functions nil
  "Functions called with a share, returning a plist added to its meta.")

(defvar spyglass--shares nil
  "Shares of this Emacs, newest first.")

(defvar spyglass--subs (make-hash-table :test #'equal)
  "Hash table of subscription keys, PEER-ID/SUB-ID, and subscriptions.")

(defvar spyglass--flush-timer nil
  "Timer sending gathered changes, or nil.")

(defvar spyglass--poll-timer nil
  "Timer polling subscribed shares, or nil.")

(defun spyglass--share-stat (share key &optional amount)
  "Add AMOUNT (1 by default) to counter KEY of SHARE."
  (let ((stats (spyglass-share-counters share)))
    (setf (spyglass-share-counters share)
          (plist-put stats key (+ (or (plist-get stats key) 0) (or amount 1))))))

(defun spyglass--share-sample (share key value)
  "Add VALUE to the ring of samples KEY of SHARE."
  (let ((ring (plist-get (spyglass-share-counters share) key)))
    (unless ring
      (setq ring (spyglass--ring))
      (setf (spyglass-share-counters share)
            (plist-put (spyglass-share-counters share) key ring)))
    (spyglass--ring-add ring value)))

(defun spyglass-share-stats (share)
  "Return the counters and summarized samples of SHARE, as a plist.
Samples are `:tick' (seconds each capture and send took on the
main thread) and `:latency' (seconds from a change to its
acknowledgment by a viewer)."
  (let (out)
    (cl-loop for (key value) on (spyglass-share-counters share) by #'cddr
             do (setq out (plist-put out key (if (vectorp value)
                                                 (spyglass-ring-summary value)
                                               value))))
    out))

(defun spyglass-share-reset-stats (share)
  "Reset the counters and samples of SHARE."
  (setf (spyglass-share-counters share) nil))

;;;; Intervals

(defun spyglass--intervals-compose (intervals changes)
  "Return INTERVALS updated with CHANGES.
INTERVALS are (B BL VL) in the buffer as it was, ascending and
disjoint: BL characters at B replace VL characters of the viewer's
copy.  CHANGES are (OB OL NL ...) in the same buffer, ascending and
disjoint: OL characters at OB became NL characters.  The result is in
the buffer as it is now; ranges that overlap or touch merge."
  (let ((i intervals) (c changes) (shift 0) out
        gs ge (gdv 0) (gdn 0))
    (while (or i c)
      (let* ((take-i (and i (or (null c) (<= (car (car i)) (car (car c))))))
             (item (if take-i (pop i) (pop c)))
             (s (car item))
             (len (cadr item)))
        (when (and gs (> s ge))
          (push (list (+ gs shift) (+ (- ge gs) gdn) (- (- ge gs) gdv)) out)
          (setq shift (+ shift gdn) gs nil gdv 0 gdn 0))
        (if gs
            (setq ge (max ge (+ s len)))
          (setq gs s ge (+ s len)))
        (if take-i
            (setq gdv (+ gdv (- len (nth 2 item))))
          (setq gdn (+ gdn (- (nth 2 item) len))))))
    (when gs
      (push (list (+ gs shift) (+ (- ge gs) gdn) (- (- ge gs) gdv)) out))
    (nreverse out)))

(defun spyglass--intervals-cap (intervals max)
  "Return INTERVALS with neighbors merged until at most MAX remain."
  (while (> (length intervals) (max 1 max))
    (let ((best nil) (best-gap most-positive-fixnum) (prev nil))
      (dolist (iv intervals)
        (when prev
          (let ((gap (- (car iv) (+ (car prev) (cadr prev)))))
            (when (< gap best-gap) (setq best-gap gap best prev))))
        (setq prev iv))
      (let* ((cell (memq best intervals))
             (a (car cell)) (b (cadr cell)))
        (setcar cell (list (car a) (- (+ (car b) (cadr b)) (car a))
                           (+ (nth 2 a) best-gap (nth 2 b))))
        (setcdr cell (cddr cell)))))
  intervals)

(defun spyglass--intervals-delta (intervals)
  "Return how much longer the buffer is than the viewer's copy, per INTERVALS."
  (let ((d 0))
    (dolist (iv intervals) (setq d (+ d (- (cadr iv) (nth 2 iv)))))
    d))

(defun spyglass--intervals-to-changes (intervals)
  "Return INTERVALS, in the buffer as it is, as changes in the buffer as it was."
  (let ((shift 0) out)
    (dolist (iv intervals)
      (push (list (- (car iv) shift) (nth 2 iv) (cadr iv)) out)
      (setq shift (+ shift (- (cadr iv) (nth 2 iv)))))
    (nreverse out)))

;;;; Hooks in shared buffers

(defun spyglass--active-p (share)
  "Return non-nil when SHARE has subscriptions to capture for."
  (spyglass-share-mode share))

(defun spyglass--schedule-flush (&optional now)
  "Send gathered changes after `spyglass-flush-delay', or as soon as idle if NOW."
  (when (and now spyglass--flush-timer
             (not (eq (timer--function spyglass--flush-timer) #'spyglass--flush-now)))
    (cancel-timer spyglass--flush-timer)
    (setq spyglass--flush-timer nil))
  (unless spyglass--flush-timer
    (setq spyglass--flush-timer
          (if now
              (run-with-timer 0 nil #'spyglass--flush-now)
            (run-with-timer spyglass-flush-delay nil #'spyglass--flush)))))

(defun spyglass--flush-now ()
  "Send what the last flush left for lack of time."
  (spyglass--flush))

(defun spyglass--after-change (beg end oldlen)
  "Note the change of OLDLEN characters at BEG into BEG..END."
  (let ((share spyglass--share))
    (when (and share (spyglass-share-mode share))
      (if (eq (spyglass-share-mode share) 'shadow)
          (spyglass--hint share beg end)
        (setf (spyglass-share-pending share)
              (spyglass--intervals-cap
               (spyglass--intervals-compose (spyglass-share-pending share)
                                            (list (list beg oldlen (- end beg))))
               spyglass-max-intervals)))
      (setf (spyglass-share-hook-chars-tick share) (buffer-chars-modified-tick)
            (spyglass-share-hook-modiff share) (buffer-modified-tick))
      (unless (spyglass-share-change-time share)
        (setf (spyglass-share-change-time share) (float-time)))
      (spyglass--schedule-flush))))
(put 'spyglass--after-change 'permanent-local-hook t)

(defun spyglass--before-change (_beg _end)
  "In direct mode, notice changes of text made without hooks before this one."
  (let ((share spyglass--share))
    (when (and share (eq (spyglass-share-mode share) 'direct)
               (/= (buffer-chars-modified-tick)
                   (spyglass-share-hook-chars-tick share)))
      (setf (spyglass-share-resync share) t))))
(put 'spyglass--before-change 'permanent-local-hook t)

(defconst spyglass--max-hints 8
  "Most hinted regions kept apart; nearer ones merge past it.")

(defun spyglass--hint (share beg end)
  "Note that SHARE's buffer may have changed between BEG and END.
Regions are kept apart, so that a deletion at the top and an append
at the end do not hint the whole buffer."
  (let ((hints (spyglass-share-hints share)) (found nil))
    (while (and hints (not found))
      (let ((h (car hints)))
        (when (and (<= beg (cdr h)) (>= end (car h)))
          (when (< beg (car h)) (set-marker (car h) beg))
          (when (> end (cdr h)) (set-marker (cdr h) end))
          (setq found t)))
      (setq hints (cdr hints)))
    (unless found
      (push (cons (copy-marker beg) (copy-marker end t)) (spyglass-share-hints share))
      (when (> (length (spyglass-share-hints share)) spyglass--max-hints)
        (spyglass--merge-hints share)))))

(defun spyglass--merge-hints (share)
  "Merge the two nearest hinted regions of SHARE."
  (let* ((sorted (sort (copy-sequence (spyglass-share-hints share))
                       (lambda (a b) (< (car a) (car b)))))
         (best nil) (gap most-positive-fixnum))
    (cl-loop for (a b) on sorted while b
             do (let ((g (- (car b) (cdr a))))
                  (when (< g gap) (setq gap g best (cons a b)))))
    (let ((a (car best)) (b (cdr best)))
      (set-marker (cdr a) (max (cdr a) (cdr b)))
      (set-marker (car b) nil)
      (set-marker (cdr b) nil)
      (setf (spyglass-share-hints share) (delq b (spyglass-share-hints share))))))

(defun spyglass--clear-hints (share)
  "Forget SHARE's hinted regions."
  (dolist (h (spyglass-share-hints share))
    (set-marker (car h) nil)
    (set-marker (cdr h) nil))
  (setf (spyglass-share-hints share) nil))

(defun spyglass--kill-buffer ()
  "Unshare the buffer being killed."
  (when spyglass--share
    (spyglass--unshare spyglass--share "killed")))
(put 'spyglass--kill-buffer 'permanent-local-hook t)

(defun spyglass--jit-hint (beg end)
  "Note that jit-lock fontifies BEG..END of a shared buffer."
  (let ((share spyglass--share))
    (when (and share (spyglass-share-mode share))
      (spyglass-notify (current-buffer) beg end t)))
  nil)

(defun spyglass-notify (&optional buffer beg end props-only)
  "Tell spyglass that BUFFER changed, between BEG and END if given.
For code changing a shared buffer with `inhibit-modification-hooks'
bound, as terminal emulators do.  BUFFER defaults to the current
buffer.  BEG and END bound every change of equal length since the
last notification; insertions and deletions are found anywhere
anyway.  Without them the whole buffer is compared.  PROPS-ONLY
non-nil says only text properties or overlays changed there.  Does
nothing for buffers no one subscribes to."
  (let* ((buffer (or buffer (current-buffer)))
         (share (and (buffer-live-p buffer)
                     (buffer-local-value 'spyglass--share buffer))))
    (when (and share (spyglass-share-mode share))
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (let ((beg (and beg (max (point-min) (min beg (point-max)))))
                (end (and end (max (point-min) (min end (point-max))))))
            (if (eq (spyglass-share-mode share) 'shadow)
                (if (and beg end)
                    (progn
                      (spyglass--hint share (min beg end) (max beg end))
                      (unless props-only
                        (setf (spyglass-share-hook-chars-tick share)
                              (buffer-chars-modified-tick)))
                      (setf (spyglass-share-props-hinted share) t))
                  (setf (spyglass-share-hint-all share) t))
              (cond
               ((/= (buffer-chars-modified-tick) (spyglass-share-hook-chars-tick share))
                (setf (spyglass-share-resync share) t))
               ((and beg end (< beg end))
                (setf (spyglass-share-pending share)
                      (spyglass--intervals-cap
                       (spyglass--intervals-compose
                        (spyglass-share-pending share)
                        (list (list beg (- end beg) (- end beg))))
                       spyglass-max-intervals)))))
            (setf (spyglass-share-hook-modiff share) (buffer-modified-tick))
            (unless (spyglass-share-change-time share)
              (setf (spyglass-share-change-time share) (float-time)))
            (spyglass--schedule-flush)))))))

;;;; Shadow

(defun spyglass--shadow-op (share function)
  "Call FUNCTION in SHARE's shadow, hooks and read-only text disabled."
  (with-current-buffer (spyglass-share-shadow share)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (buffer-undo-list t))
      (funcall function))))

(defun spyglass--spread-sentinels (share)
  "Spread SHARE's sentinels evenly through its buffer, now equal to its shadow."
  (let* ((size (buffer-size))
         (k (max 8 (min 256 (/ size 16384))))
         (old (spyglass-share-sentinels share)))
    (unless (and old (= (length old) k))
      (when old (mapc (lambda (m) (set-marker m nil)) old))
      (setq old (make-vector k nil))
      (dotimes (i k) (aset old i (make-marker)))
      (setf (spyglass-share-sentinels share) old))
    (let ((synced (make-vector k 0)))
      (dotimes (i k)
        (let ((pos (+ 1 (/ (* (1+ i) size) (1+ k)))))
          (set-marker (aref old i) pos)
          (aset synced i pos)))
      (setf (spyglass-share-synced share) synced
            (spyglass-share-spread-size share) size))))

(defun spyglass--maybe-respread (share)
  "Spread SHARE's sentinels again when segments grew uneven."
  (let* ((size (buffer-size))
         (spread (spyglass-share-spread-size share))
         (synced (spyglass-share-synced share))
         (k (length synced))
         (limit (+ 4096 (* 4 (/ size (1+ k)))))
         (prev 1) (uneven nil))
    (when (or (> size (* 2 (max spread 4096))) (< (* 2 size) spread))
      (setq uneven t))
    (unless uneven
      (dotimes (i k)
        (when (> (- (aref synced i) prev) limit) (setq uneven t))
        (setq prev (aref synced i)))
      (when (> (- (point-max) prev) limit) (setq uneven t)))
    (when uneven (spyglass--spread-sentinels share))))

(defun spyglass--equal-region (shadow a b len)
  "Return non-nil when LEN characters at A here equal those at B in SHADOW.
Text and properties are compared."
  (or (<= len 0)
      (equal-including-properties
       (buffer-substring a (+ a len))
       (with-current-buffer shadow (buffer-substring b (+ b len))))))

(defun spyglass--common-prefix (shadow a b n)
  "Return how many of the N characters at A here and B in SHADOW are equal.
Counting from the start, text and properties alike."
  (let ((p 0) (step 64) (found nil))
    (while (and (< p n) (not found))
      (let ((len (min step (- n p))))
        (if (spyglass--equal-region shadow (+ a p) (+ b p) len)
            (setq p (+ p len) step (min (* 2 step) 65536))
          (let ((lo 0) (hi len))
            (while (> (- hi lo) 1)
              (let ((mid (/ (+ lo hi) 2)))
                (if (spyglass--equal-region shadow (+ a p lo) (+ b p lo) (- mid lo))
                    (setq lo mid)
                  (setq hi mid))))
            (setq p (+ p lo) found t)))))
    p))

(defun spyglass--common-suffix (shadow a b n)
  "Return how many of the N characters before A here and B in SHADOW are equal.
Counting from the end, text and properties alike."
  (let ((s 0) (step 64) (found nil))
    (while (and (< s n) (not found))
      (let ((len (min step (- n s))))
        (if (spyglass--equal-region shadow (- a s len) (- b s len) len)
            (setq s (+ s len) step (min (* 2 step) 65536))
          (let ((lo 0) (hi len))
            (while (> (- hi lo) 1)
              (let ((mid (/ (+ lo hi) 2)))
                (if (spyglass--equal-region shadow (- a s mid) (- b s mid) (- mid lo))
                    (setq lo mid)
                  (setq hi mid))))
            (setq s (+ s lo) found t)))))
    s))

(defun spyglass--line-starts (beg end)
  "Return the starts of the lines between BEG and END, ascending."
  (save-excursion
    (goto-char beg)
    (let (starts)
      (while (< (point) end)
        (push (point) starts)
        (forward-line 1))
      (nreverse starts))))

(defconst spyglass--line-refine-min 512
  "Shortest differing range compared line by line.")

(defun spyglass--refine-lines (shadow nb ne ob oe)
  "Return the changes from SHADOW's OB..OE to NB..NE here, line by line.
Nil when the ranges hold different numbers of lines."
  (when (and (>= (- ne nb) spyglass--line-refine-min)
             (>= (- oe ob) spyglass--line-refine-min))
    (let ((new (spyglass--line-starts nb ne))
          (old (with-current-buffer shadow (spyglass--line-starts ob oe))))
      (when (and (= (length new) (length old)) (< (length new) 8192))
        (let (changes run)
          (while new
            (let* ((n1 (pop new)) (n2 (or (car new) ne))
                   (o1 (pop old)) (o2 (or (car old) oe)))
              (if (and (= (- n2 n1) (- o2 o1))
                       (spyglass--equal-region shadow n1 o1 (- n2 n1)))
                  (when run (push run changes) (setq run nil))
                (if run
                    (setq run (list (car run) (- o2 (car run))
                                    (- n2 (nth 3 run)) (nth 3 run)))
                  (setq run (list o1 (- o2 o1) (- n2 n1) n1))))))
          (when run (push run changes))
          (nreverse changes))))))

(defun spyglass--refine (shadow nb ne ob oe)
  "Return the changes from SHADOW's OB..OE to NB..NE here.
Changes are (OB OL NL NB), ascending."
  (let* ((m (min (- ne nb) (- oe ob)))
         (p (spyglass--common-prefix shadow nb ob m))
         (s (spyglass--common-suffix shadow ne oe (- m p)))
         (nb (+ nb p)) (ne (- ne s)) (ob (+ ob p)) (oe (- oe s)))
    (unless (and (= nb ne) (= ob oe))
      (or (spyglass--refine-lines shadow nb ne ob oe)
          (list (list ob (- oe ob) (- ne nb) nb))))))

(defun spyglass--capture-shadow (share)
  "Return SHARE's changes since the last capture, and sync its shadow.
Changes are (OB OL NL NB) ascending, OB in the shadow as it was."
  (let* ((shadow (spyglass-share-shadow share))
         (ct (buffer-chars-modified-tick))
         (mt (buffer-modified-tick))
         (moved (or (/= ct (spyglass-share-chars-tick share))
                    (/= mt (spyglass-share-modiff share))))
         (hints (mapcar (lambda (h) (cons (marker-position (car h))
                                          (marker-position (cdr h))))
                        (spyglass-share-hints share)))
         (hinted hints)
         (all (spyglass-share-hint-all share))
         (scan-start (float-time)))
    ;; Text changed after the last change hooks or hints told of: it
    ;; may have changed anywhere.  Properties changed so: anywhere,
    ;; unless a hint without hooks, such as jit-lock's, came since.
    (when (or (/= ct (spyglass-share-hook-chars-tick share))
              (and (/= mt (spyglass-share-hook-modiff share))
                   (not (spyglass-share-props-hinted share))))
      (setq all t))
    ;; Comparing everything costs; keep it under a tenth of the time.
    (when (and all (< (- (float-time) (spyglass-share-scan-time share))
                      (* 10 (spyglass-share-scan-cost share))))
      (throw 'spyglass--defer
             (- (* 10 (spyglass-share-scan-cost share))
                (- (float-time) (spyglass-share-scan-time share)))))
    (when (or moved hinted all)
      (when all (spyglass--share-stat share :full-scans))
      (let* ((sentinels (spyglass-share-sentinels share))
             (synced (spyglass-share-synced share))
             (k (length sentinels))
             (old-max (with-current-buffer shadow (point-max)))
             (new-max (point-max))
             changes)
        (dotimes (i (1+ k))
          (let* ((ob (if (= i 0) 1 (aref synced (1- i))))
                 (oe (if (= i k) old-max (aref synced i)))
                 (nb (if (= i 0) 1 (marker-position (aref sentinels (1- i)))))
                 (ne (if (= i k) new-max (marker-position (aref sentinels i)))))
            (when (if (/= (- oe ob) (- ne nb))
                      t
                    (and (> ne nb)
                         (if all
                             (not (spyglass--equal-region shadow nb ob (- ne nb)))
                           (cl-some (lambda (h)
                                      (and (<= nb (cdr h)) (<= (car h) ne)
                                           (let ((cb (max nb (car h))) (ce (min ne (cdr h))))
                                             (not (spyglass--equal-region
                                                   shadow cb (+ ob (- cb nb)) (- ce cb))))))
                                    hints))))
              (setq changes (nconc changes (spyglass--refine shadow nb ne ob oe))))))
        (when changes
          (spyglass--shadow-apply share changes))
        (dotimes (i k)
          (aset synced i (marker-position (aref sentinels i))))
        (spyglass--maybe-respread share)
        (spyglass--check-overlays-after share changes hints all)
        (spyglass--clear-hints share)
        (setf (spyglass-share-hint-all share) nil
              (spyglass-share-props-hinted share) nil)
        (when all
          (let ((now (float-time)))
            (setf (spyglass-share-scan-cost share) (- now scan-start)
                  (spyglass-share-scan-time share) now)))
        changes))))

(defun spyglass--shadow-apply (share changes)
  "Apply CHANGES, (OB OL NL NB) ascending, to SHARE's shadow."
  (let ((buffer (current-buffer)))
    (spyglass--shadow-op
     share
     (lambda ()
       (dolist (change (reverse changes))
         (let ((ob (nth 0 change)) (ol (nth 1 change))
               (nl (nth 2 change)) (nb (nth 3 change)))
           (goto-char ob)
           (delete-region ob (+ ob ol))
           (insert-buffer-substring buffer nb (+ nb nl))))))))

;;;; Overlays

(defun spyglass--overlay-props (overlay)
  "Return the mirrored properties of OVERLAY as a plist, or nil."
  (unless (or (overlay-get overlay 'window)
              (let ((face (overlay-get overlay 'face)))
                (and face (symbolp face) (memq face spyglass-ignored-overlay-faces))))
    (let (out visible)
      (dolist (prop spyglass-overlay-properties)
        (let ((value (overlay-get overlay prop)))
          (when value
            (unless (eq prop 'priority) (setq visible t))
            (setq out (cons value (cons prop out))))))
      (and visible (nreverse out)))))

(defun spyglass--mark-overlay (share id overlay)
  "Mark overlay ID of SHARE, OVERLAY or nil when gone, for every viewer."
  (dolist (sub (spyglass-share-subs share))
    (puthash id overlay (spyglass--sub-ov-dirty sub))))

(defun spyglass--check-overlays (share beg end)
  "Compare SHARE's overlays between BEG and END with its shadow's."
  (let* ((buffer (current-buffer))
         (shadow (spyglass-share-shadow share))
         (table (spyglass-share-ovs share))
         (beg (max (point-min) (1- beg)))
         (end (min (point-max) (1+ end))))
    (dolist (ov (overlays-in beg end))
      (let ((props (spyglass--overlay-props ov))
            (rec (gethash ov table)))
        (cond
         ((and props (not rec))
          (let ((id (cl-incf (spyglass-share-next-ov share)))
                (sov (with-current-buffer shadow
                       (make-overlay (overlay-start ov) (overlay-end ov)))))
            (overlay-put sov 'spyglass-src ov)
            (puthash ov (vector id sov props) table)
            (spyglass--mark-overlay share id ov)))
         ((and rec (not props))
          (delete-overlay (aref rec 1))
          (remhash ov table)
          (spyglass--mark-overlay share (aref rec 0) nil))
         (rec
          (let ((sov (aref rec 1)))
            (unless (and (eq (overlay-start sov) (overlay-start ov))
                         (eq (overlay-end sov) (overlay-end ov))
                         (equal props (aref rec 2)))
              (move-overlay sov (overlay-start ov) (overlay-end ov))
              (aset rec 2 props)
              (spyglass--mark-overlay share (aref rec 0) ov)))))))
    (dolist (sov (with-current-buffer shadow (overlays-in beg end)))
      (let* ((ov (overlay-get sov 'spyglass-src))
             (rec (and ov (gethash ov table))))
        (cond
         ((not rec) (delete-overlay sov))
         ((not (eq (overlay-buffer ov) buffer))
          (delete-overlay sov)
          (remhash ov table)
          (spyglass--mark-overlay share (aref rec 0) nil))
         ((not (and (eq (overlay-start sov) (overlay-start ov))
                    (eq (overlay-end sov) (overlay-end ov))))
          (move-overlay sov (overlay-start ov) (overlay-end ov))
          (spyglass--mark-overlay share (aref rec 0) ov)))))))

(defun spyglass--check-overlays-after (share changes hints all)
  "Compare SHARE's overlays where CHANGES, HINTS or ALL touched.
HINTS are conses of positions."
  (if all
      (spyglass--check-overlays share (point-min) (point-max))
    (dolist (change changes)
      (let ((nb (nth 3 change)))
        (spyglass--check-overlays share nb (+ nb (nth 2 change)))))
    (dolist (h hints) (spyglass--check-overlays share (car h) (cdr h)))))

;;;; Sweep

(defun spyglass--sweep (share)
  "Compare the next stretch of SHARE with its shadow, hinting differences."
  (let* ((size (buffer-size))
         (beg (if (> (spyglass-share-sweep-pos share) (point-max)) 1
                (spyglass-share-sweep-pos share)))
         (end (min (point-max) (+ beg spyglass-sweep-chars))))
    (setf (spyglass-share-sweep-time share) (float-time)
          (spyglass-share-sweep-pos share) (if (>= end (point-max)) 1 end))
    (when (>= end (point-max))
      (cl-incf (spyglass-share-sweeps share)))
    (when (> size 0)
      (unless (spyglass--equal-region (spyglass-share-shadow share) beg beg (- end beg))
        (spyglass--share-stat share :sweep-repairs)
        (spyglass-notify (current-buffer) beg end t))
      (spyglass--check-overlays share beg end))))

;;;; Activation

(defun spyglass--activate (share)
  "Start capturing SHARE's buffer for its first subscription."
  (with-current-buffer (spyglass-share-buffer share)
    (save-restriction
      (widen)
      (let ((mode (pcase (spyglass-share-capture share)
                    ('auto (if (> (buffer-size) spyglass-shadow-limit) 'direct 'shadow))
                    (m m))))
        (setf (spyglass-share-mode share) mode
              (spyglass-share-pending share) nil
              (spyglass-share-resync share) nil
              (spyglass-share-hints share) nil
              (spyglass-share-hint-all share) nil
              (spyglass-share-props-hinted share) nil
              (spyglass-share-change-time share) nil
              (spyglass-share-styles share) (make-hash-table :test #'equal)
              (spyglass-share-size share) (buffer-size))
        (when (eq mode 'shadow)
          (let ((shadow (generate-new-buffer
                         (format " *spyglass-shadow %s*" (spyglass-share-id share)) t))
                (buffer (current-buffer)))
            (with-current-buffer shadow
              (set-buffer-multibyte (buffer-local-value 'enable-multibyte-characters
                                                        buffer))
              (setq buffer-undo-list t))
            (setf (spyglass-share-shadow share) shadow
                  (spyglass-share-ovs share) (make-hash-table :test #'eq))
            (spyglass--shadow-op share (lambda () (insert-buffer-substring buffer)))
            (spyglass--spread-sentinels share)
            (spyglass--check-overlays share (point-min) (point-max))))
        (setf (spyglass-share-chars-tick share) (buffer-chars-modified-tick)
              (spyglass-share-modiff share) (buffer-modified-tick)
              (spyglass-share-hook-chars-tick share) (buffer-chars-modified-tick)
              (spyglass-share-hook-modiff share) (buffer-modified-tick))
        (when (and (bound-and-true-p jit-lock-mode)
                   (not (spyglass-share-jit share)))
          (add-hook 'jit-lock-functions #'spyglass--jit-hint t t)
          (setf (spyglass-share-jit share) t)))))
  (spyglass--start-poll))

(defun spyglass--deactivate (share)
  "Stop capturing SHARE's buffer, which no one subscribes to anymore."
  (when (spyglass-share-shadow share)
    (kill-buffer (spyglass-share-shadow share)))
  (seq-doseq (m (or (spyglass-share-sentinels share) []))
    (set-marker m nil))
  (spyglass--clear-hints share)
  (when (and (spyglass-share-jit share) (buffer-live-p (spyglass-share-buffer share)))
    (with-current-buffer (spyglass-share-buffer share)
      (remove-hook 'jit-lock-functions #'spyglass--jit-hint t)))
  (setf (spyglass-share-mode share) nil
        (spyglass-share-shadow share) nil
        (spyglass-share-sentinels share) nil
        (spyglass-share-synced share) nil
        (spyglass-share-ovs share) nil
        (spyglass-share-pending share) nil
        (spyglass-share-styles share) nil
        (spyglass-share-jit share) nil))

(defun spyglass--to-direct (share)
  "Switch SHARE, grown past `spyglass-shadow-limit', to direct capture."
  (maphash (lambda (_ov rec) (spyglass--mark-overlay share (aref rec 0) nil))
           (spyglass-share-ovs share))
  (kill-buffer (spyglass-share-shadow share))
  (seq-doseq (m (or (spyglass-share-sentinels share) [])) (set-marker m nil))
  (setf (spyglass-share-mode share) 'direct
        (spyglass-share-shadow share) nil
        (spyglass-share-sentinels share) nil
        (spyglass-share-ovs share) nil
        (spyglass-share-pending share) nil))

;;;; Capture

(defun spyglass--capture (share)
  "Capture SHARE's changes and compose them into its subscriptions.
A comparison of the whole buffer too soon after the last one is put
off; polling captures again later."
  (if (eq (spyglass-share-mode share) 'shadow)
      (let ((result (catch 'spyglass--defer
                      (list (spyglass--capture-shadow share)))))
        (when (consp result)
          (spyglass--capture-changes share (car result))))
    (spyglass--capture-changes share (spyglass--capture-direct share))))

(defun spyglass--capture-changes (share changes)
  "Compose CHANGES, just captured from SHARE, into its subscriptions."
  (when changes
    (spyglass--share-stat share :captures)
    (let ((time (or (spyglass-share-change-time share) (float-time))))
      (dolist (sub (spyglass-share-subs share))
        (setf (spyglass--sub-intervals sub)
              (spyglass--intervals-cap
               (spyglass--intervals-compose (spyglass--sub-intervals sub) changes)
               spyglass-max-intervals))
        (unless (spyglass--sub-ct sub)
          (setf (spyglass--sub-ct sub) time)))))
  (setf (spyglass-share-change-time share) nil
        (spyglass-share-chars-tick share) (buffer-chars-modified-tick)
        (spyglass-share-modiff share) (buffer-modified-tick)
        (spyglass-share-hook-chars-tick share) (buffer-chars-modified-tick)
        (spyglass-share-hook-modiff share) (buffer-modified-tick)
        (spyglass-share-size share) (buffer-size))
  (when (and (eq (spyglass-share-mode share) 'shadow)
             (> (buffer-size) (+ spyglass-shadow-limit (/ spyglass-shadow-limit 2))))
    (spyglass--to-direct share)))

(defun spyglass--capture-direct (share)
  "Return SHARE's changes since the last capture, from its hooks."
  (when (/= (buffer-chars-modified-tick) (spyglass-share-hook-chars-tick share))
    (setf (spyglass-share-resync share) t))
  (prog1
      (if (spyglass-share-resync share)
          (progn
            (spyglass--share-stat share :resyncs)
            (list (list 1 (spyglass-share-size share) (buffer-size))))
        (spyglass--intervals-to-changes (spyglass-share-pending share)))
    (setf (spyglass-share-pending share) nil
          (spyglass-share-resync share) nil)))

;;;; Describing the buffer

(defun spyglass--invisibility-spec ()
  "Return `buffer-invisibility-spec' reduced to symbols."
  (if (listp buffer-invisibility-spec)
      (vconcat
       (delq nil (mapcar (lambda (e)
                           (cond ((symbolp e) (spyglass-wire-encode e))
                                 ((and (consp e) (symbolp (car e)))
                                  (spyglass-wire-encode (cons (car e) (and (cdr e) t))))))
                         buffer-invisibility-spec)))
    t))

(defun spyglass--compute-meta (share)
  "Return the description of SHARE's buffer sent to viewers."
  (let ((header (and header-line-format
                     (condition-case nil
                         (format-mode-line header-line-format nil nil (current-buffer))
                       (error nil)))))
    (append
     (list :name (spyglass-wire-plain-string (buffer-name))
           :mode (spyglass-wire-plain-string
                  (condition-case nil (format-mode-line mode-name) (error "")))
           :tw tab-width :tl (if truncate-lines t :false)
           :ww (if word-wrap t :false)
           :inv (spyglass--invisibility-spec)
           :hl (if (and header (not (string-empty-p header)))
                   (spyglass-wire-encode (spyglass-wire-sanitize-string header))
                 :false)
           :fl (if (spyglass-share-jit share) t :false)
           :in (if (spyglass-share-read-only share) :false t))
     (mapcan (lambda (f) (copy-sequence (funcall f share))) spyglass--meta-functions))))

(defun spyglass--refresh-share-state (share)
  "Compute what SHARE's viewers follow: point, window start, description.
When `font-lock-face' starts or stops showing, as font lock turns on
or off, every viewer gets the whole buffer again."
  (let ((flf (and (memq 'font-lock-face (alist-get 'face char-property-alias-alist)) t))
        (old (spyglass-share-font-lock-face share)))
    (unless (eq flf old)
      (setf (spyglass-share-font-lock-face share) flf)
      (unless (or (eq old :unknown) (zerop (buffer-size)))
        (spyglass--capture-changes share (list (list 1 (buffer-size) (buffer-size)))))))
  (let ((window (get-buffer-window (current-buffer) t)))
    (setf (spyglass-share-point share)
          (let ((pos (and spyglass-point-function
                          (condition-case nil (funcall spyglass-point-function)
                            (error nil)))))
            (if (number-or-marker-p pos)
                (max (point-min) (min (point-max) (+ 0 pos)))
              (if window (window-point window) (point))))
          (spyglass-share-window-start share) (and window (window-start window))))
  (let ((now (float-time)))
    (when (or (null (spyglass-share-meta share))
              (> (- now (spyglass-share-meta-time share)) 0.5))
      (setf (spyglass-share-meta share) (spyglass--compute-meta share)
            (spyglass-share-meta-time share) now))))

;;;; Styles

(defun spyglass--raw-props (plist props font-lock-face)
  "Return the values of PROPS in text property PLIST, as a plist.
With FONT-LOCK-FACE non-nil, `font-lock-face' stands for `face'
where `face' is absent."
  (let (out)
    (dolist (prop props)
      (let ((value (plist-get plist prop)))
        (when (and (null value) font-lock-face (eq prop 'face))
          (setq value (plist-get plist 'font-lock-face)))
        (when value
          (setq out (cons value (cons prop out))))))
    (nreverse out)))

(defun spyglass--face-fallback (face)
  "Return the attributes of FACE, a face name, for viewers without it."
  (when (facep face)
    (let (attrs)
      (dolist (attr spyglass-wire-face-attributes)
        (unless (eq attr :inherit)
          (let ((value (condition-case nil (face-attribute face attr nil t)
                         (error 'unspecified))))
            (unless (or (eq value 'unspecified)
                        (and (eq attr :height) (not (floatp value))))
              (setq attrs (cons value (cons attr attrs)))))))
      (spyglass-wire-sanitize-face (nreverse attrs)))))

(defun spyglass--face-names (face)
  "Return the face names in FACE, a sanitized face value."
  (cond
   ((and face (symbolp face)) (list face))
   ((and (consp face) (keywordp (car face)))
    (let ((inherit (plist-get face :inherit)))
      (if (symbolp inherit) (and inherit (list inherit)) inherit)))
   ((consp face) (mapcan #'spyglass--face-names (copy-sequence face)))))

(defun spyglass--style-id (sub raw acc)
  "Return SUB's style for RAW, a plist of mirrored properties, or 0.
New styles and the faces they name are added to ACC, a cons of lists
\(STYLE-DEFINITIONS . FACE-DEFINITIONS)."
  (if (null raw)
      0
    (or (gethash raw (spyglass--sub-styles sub))
        (let* ((share (spyglass--sub-share sub))
               (cache (spyglass-share-styles share))
               (clean (or (gethash raw cache)
                          (puthash raw (or (spyglass-wire-sanitize-props
                                            raw spyglass-wire-style-properties)
                                           :none)
                                   cache))))
          (if (eq clean :none)
              (puthash raw 0 (spyglass--sub-styles sub))
            (let ((sid (cl-incf (spyglass--sub-next-style sub))))
              (puthash raw sid (spyglass--sub-styles sub))
              (push (vector sid (spyglass-wire-encode clean)) (car acc))
              (dolist (face (spyglass--face-names (plist-get clean 'face)))
                (unless (gethash face (spyglass--sub-faces sub))
                  (puthash face t (spyglass--sub-faces sub))
                  (push (vector (symbol-name face)
                                (spyglass-wire-encode (spyglass--face-fallback face)))
                        (cdr acc))))
              sid))))))

(defun spyglass--runs (sub text acc)
  "Return the runs of styles of TEXT for SUB, as a flat vector.
The vector alternates lengths and styles; it is empty when TEXT has
no mirrored property.  ACC collects new definitions, see
`spyglass--style-id'."
  (let ((runs nil) (props spyglass-text-properties) (styled nil)
        ;; `font-lock-face' shows where `char-property-alias-alist' says.
        (flf (memq 'font-lock-face (alist-get 'face char-property-alias-alist))))
    (dolist (iv (object-intervals text))
      (let* ((len (- (cadr iv) (car iv)))
             (sid (spyglass--style-id sub (spyglass--raw-props (caddr iv) props flf)
                                      acc)))
        (unless (eq sid 0) (setq styled t))
        (if (and runs (eq (cdar runs) sid))
            (setcar (car runs) (+ (caar runs) len))
          (push (cons len sid) runs))))
    (if (not styled)
        []
      (let ((v (make-vector (* 2 (length runs)) 0)) (i 0))
        (dolist (run (nreverse runs))
          (aset v i (car run))
          (aset v (1+ i) (cdr run))
          (setq i (+ i 2)))
        v))))

;;;; Sending

(defun spyglass--sub-live-p (sub)
  "Return non-nil when SUB's viewer is connected."
  (let ((connection (spyglass--sub-connection sub)))
    (and connection (spyglass-connection-live-p connection))))

(defun spyglass--build-frame (sub)
  "Return the next frame for SUB as (HEADER . PAYLOAD), or nil if none is due."
  (let* ((share (spyglass--sub-share sub))
         (shadow-mode (eq (spyglass-share-mode share) 'shadow))
         (acc (cons nil nil))
         (budget spyglass-frame-chars)
         (intervals (reverse (spyglass--sub-intervals sub)))
         (delta (spyglass--intervals-delta intervals))
         (reset (spyglass--sub-reset sub))
         (styles-reset nil)
         ops texts)
    (when (> (hash-table-count (spyglass--sub-styles sub)) spyglass-max-styles)
      (clrhash (spyglass--sub-styles sub))
      (clrhash (spyglass--sub-faces sub))
      (setq styles-reset t))
    (while (and intervals (> budget 0))
      (let* ((iv (car intervals))
             (b (nth 0 iv)) (bl (nth 1 iv)) (vl (nth 2 iv))
             (vbeg (- b (- delta (- bl vl))))
             (take (min bl budget))
             (text (buffer-substring b (+ b take))))
        (push (vector vbeg vl take (spyglass--runs sub text acc)) ops)
        (push (if enable-multibyte-characters text (string-to-multibyte text)) texts)
        (when shadow-mode
          (let ((table (spyglass-share-ovs share)))
            (dolist (ov (overlays-in (max (point-min) (1- b))
                                     (min (point-max) (+ b take 1))))
              (when-let* ((rec (gethash ov table)))
                (puthash (aref rec 0) ov (spyglass--sub-ov-dirty sub))))))
        (setq budget (- budget take))
        (if (< take bl)
            (setcar intervals (list (+ b take) (- bl take) 0))
          (setq delta (- delta (- bl vl)))
          (pop intervals))))
    (setf (spyglass--sub-intervals sub) (nreverse intervals))
    (let* ((synced (null (spyglass--sub-intervals sub)))
           (ovs nil) (dels nil)
           (pt (and synced (spyglass-share-point share)))
           (ws (and synced (spyglass-share-window-start share)))
           (meta (spyglass-share-meta share))
           (send-pt (and pt (not (eql pt (spyglass--sub-point sub)))))
           (send-ws (and ws (not (eql ws (spyglass--sub-window-start sub)))))
           (send-meta (and meta (not (equal meta (spyglass--sub-meta sub))))))
      (when (and synced (> (hash-table-count (spyglass--sub-ov-dirty sub)) 0))
        (maphash
         (lambda (id ov)
           (let ((props (and ov (eq (overlay-buffer ov) (current-buffer))
                             (spyglass--overlay-props ov))))
             (if props
                 (push (vector id (overlay-start ov) (overlay-end ov)
                               (spyglass--style-id sub props acc))
                       ovs)
               (push id dels))))
         (spyglass--sub-ov-dirty sub))
        (clrhash (spyglass--sub-ov-dirty sub)))
      (when (or ops ovs dels send-pt send-ws send-meta reset styles-reset)
        (when send-pt (setf (spyglass--sub-point sub) pt))
        (when send-ws (setf (spyglass--sub-window-start sub) ws))
        (when send-meta (setf (spyglass--sub-meta sub) meta))
        (let ((header
               (append
                (list :t "u" :sub (spyglass--sub-id sub)
                      :seq (cl-incf (spyglass--sub-seq sub))
                      :ct (or (spyglass--sub-ct sub) :false)
                      :ops (vconcat (nreverse ops)))
                (and reset (list :reset t :epoch (spyglass--sub-epoch sub)))
                (and styles-reset (list :sr t))
                (and (car acc) (list :sd (vconcat (nreverse (car acc)))))
                (and (cdr acc) (list :fd (vconcat (nreverse (cdr acc)))))
                (and ovs (list :ov (vconcat ovs)))
                (and dels (list :od (vconcat dels)))
                (and send-pt (list :pt pt))
                (and send-ws (list :ws ws))
                (and send-meta (list :meta meta)))))
          (setf (spyglass--sub-reset sub) nil)
          (when synced (setf (spyglass--sub-ct sub) nil))
          ;; Each text is encoded apart: the frame is sent in parts.
          (let ((offset 0) parts surrogates)
            (dolist (text (nreverse texts))
              (let ((encoded (spyglass-wire-encode-payload text)))
                (push (car encoded) parts)
                (seq-doseq (s (or (cdr encoded) []))
                  (push (vector (+ offset (aref s 0)) (aref s 1)) surrogates))
                (setq offset (+ offset (length text)))))
            (cons (if surrogates
                      (append header (list :sur (vconcat (nreverse surrogates))))
                    header)
                  (nreverse parts))))))))

(defun spyglass--sub-restart (sub)
  "Make SUB start over from a snapshot, under a new epoch."
  (setf (spyglass--sub-epoch sub) (substring (spyglass-wire-random-token) 0 16)
        (spyglass--sub-reset sub) t
        (spyglass--sub-intervals sub) (and (> (buffer-size) 0)
                                           (list (list 1 (buffer-size) 0)))
        (spyglass--sub-ct sub) (float-time)
        (spyglass--sub-point sub) nil
        (spyglass--sub-window-start sub) nil
        (spyglass--sub-meta sub) nil)
  (clrhash (spyglass--sub-styles sub))
  (clrhash (spyglass--sub-faces sub))
  (clrhash (spyglass--sub-ov-dirty sub))
  (let ((table (spyglass-share-ovs (spyglass--sub-share sub))))
    (when table
      (maphash (lambda (ov rec) (puthash (aref rec 0) ov (spyglass--sub-ov-dirty sub)))
               table))))

(defun spyglass--sub-frame (sub)
  "Return SUB's next frame as (PARTS . CHANGE-TIME), or nil if none is due.
PARTS are the frame's strings, sent in order.  A frame that cannot
be built makes SUB start over; a snapshot that cannot either ends it."
  (condition-case err
      (let ((built (spyglass--build-frame sub)))
        (when built
          (let ((ct (plist-get (car built) :ct)))
            (cons (spyglass-wire-frame-parts (car built) (cdr built))
                  (and (numberp ct) ct)))))
    (error
     (spyglass--report (spyglass--sub-connection sub) "Cannot send %s: %s; starting over"
                       (spyglass-share-id (spyglass--sub-share sub))
                       (error-message-string err))
     (spyglass--sub-restart sub)
     (condition-case err
         (let ((built (spyglass--build-frame sub)))
           (and built (cons (spyglass-wire-frame-parts (car built) (cdr built)) nil)))
       (error
        ;; Even a snapshot fails: give up on this viewer.
        (spyglass--sub-free sub (format "cannot send: %s" (error-message-string err)))
        nil)))))

(defvar spyglass--sending nil
  "Non-nil while frames are being sent.
A write the stream cannot take at once makes Emacs wait, running
timers and process filters meanwhile: they may change shared buffers
and acknowledge frames, but must not send, lest captures and sends
interleave.  They schedule a flush instead.")

(defun spyglass--captured-p (share)
  "Return non-nil when SHARE's buffer has not changed since its capture."
  (and (= (buffer-chars-modified-tick) (spyglass-share-chars-tick share))
       (= (buffer-modified-tick) (spyglass-share-modiff share))))

(defun spyglass--sub-send (sub &optional deadline)
  "Send SUB's viewer what is due, as far as the window allows.
Past DEADLINE, a time, stop after the first frame and have the next
flush continue.  The current buffer is SUB's share's, widened and
just captured."
  (if spyglass--sending
      (spyglass--schedule-flush)
    (let ((spyglass--sending t)
          (connection (spyglass--sub-connection sub))
          (share (spyglass--sub-share sub))
          (first t))
      (catch 'done
        (while (and (< (spyglass--sub-inflight sub) spyglass-window)
                    (spyglass--sub-live-p sub))
          (when (and deadline (not first) (> (float-time) deadline))
            (spyglass--schedule-flush t)
            (throw 'done nil))
          (setq first nil)
          ;; Sending may have let the buffer change: capture again first.
          (unless (spyglass--captured-p share)
            (spyglass--schedule-flush)
            (throw 'done nil))
          (let ((built (spyglass--sub-frame sub)))
            (unless built (throw 'done nil))
            ;; Recorded before writing: the acknowledgment may come
            ;; while the write waits.
            (let* ((parts (car built))
                   (size (apply #'+ (mapcar #'length parts)))
                   (record (list (spyglass--sub-seq sub) size (cdr built) parts)))
              (push record (spyglass--sub-unacked sub))
              (cl-incf (spyglass--sub-inflight sub) size)
              (let ((before (float-time)))
                (unless (spyglass--send-frame connection parts)
                  (throw 'done nil))
                (spyglass--share-sample share :write (- (float-time) before))
                (when (cdr built)
                  (spyglass--share-sample share :queue (- before (cdr built)))))
              (spyglass--share-stat share :frames)
              (spyglass--share-stat share :bytes size)))))
      (when (>= (spyglass--sub-inflight sub) spyglass-window)
        (spyglass--share-stat share :window-full)))))

(defun spyglass--flush ()
  "Capture every subscribed share and send its viewers what is due."
  (setq spyglass--flush-timer nil)
  (if spyglass--sending
      (spyglass--schedule-flush)
    (spyglass--flush-1)))

(defun spyglass--flush-1 ()
  "Capture every subscribed share and send its viewers what is due."
  (let ((deadline (+ (float-time) spyglass-tick-budget)))
    (dolist (share spyglass--shares)
      (when (and (spyglass-share-mode share)
                 (buffer-live-p (spyglass-share-buffer share)))
        (spyglass--flush-share share deadline)))))

(defun spyglass--flush-share (share deadline)
  "Capture SHARE and send its viewers what is due, until DEADLINE."
  (spyglass--guard nil
    (let ((start (float-time)))
      (with-current-buffer (spyglass-share-buffer share)
        (save-restriction
          (widen)
          (spyglass--capture share)
          (spyglass--refresh-share-state share)
          (dolist (sub (spyglass-share-subs share))
            (when (spyglass--sub-live-p sub)
              (spyglass--sub-send sub deadline)))))
      (spyglass--share-sample share :tick (- (float-time) start)))))

;;;; Polling

(defun spyglass--start-poll ()
  "Start polling subscribed shares, unless already."
  (unless spyglass--poll-timer
    (setq spyglass--poll-timer
          (run-with-timer spyglass-poll-interval spyglass-poll-interval
                          #'spyglass--poll))))

(defun spyglass--poll ()
  "Look for changes made without hooks, sweep, and expire orphans."
  (spyglass--guard nil
    (let ((now (float-time)) (active nil) (expired nil))
      (maphash (lambda (_key sub)
                 (when (and (spyglass--sub-orphaned sub)
                            (> (- now (spyglass--sub-orphaned sub))
                               spyglass-resume-timeout))
                   (push sub expired)))
               spyglass--subs)
      (dolist (sub expired) (spyglass--sub-free sub "expired"))
      (dolist (share spyglass--shares)
        (when (and (spyglass-share-mode share)
                   (buffer-live-p (spyglass-share-buffer share)))
          (setq active t)
          (with-current-buffer (spyglass-share-buffer share)
            (when (or (/= (buffer-chars-modified-tick) (spyglass-share-chars-tick share))
                      (/= (buffer-modified-tick) (spyglass-share-modiff share))
                      (spyglass-share-hints share)
                      (spyglass-share-hint-all share)
                      (> (- now (spyglass-share-meta-time share)) 0.5)
                      (let ((window (get-buffer-window (current-buffer) t))
                            (pos (and spyglass-point-function
                                      (ignore-errors (funcall spyglass-point-function)))))
                        (not (eql (if (number-or-marker-p pos)
                                      (+ 0 pos)
                                    (if window (window-point window) (point)))
                                  (spyglass-share-point share)))))
              (spyglass--schedule-flush))
            (when (and (eq (spyglass-share-mode share) 'shadow)
                       (> (- now (spyglass-share-sweep-time share))
                          spyglass-sweep-interval)
                       (= (buffer-chars-modified-tick) (spyglass-share-chars-tick share))
                       (= (buffer-modified-tick) (spyglass-share-modiff share)))
              (save-restriction
                (widen)
                (spyglass--sweep share)))
            (dolist (f spyglass--poll-functions)
              (spyglass--guard nil (funcall f share))))))
      (unless active
        (cancel-timer spyglass--poll-timer)
        (setq spyglass--poll-timer nil)))))

;;;; Subscriptions

(defun spyglass--sub-free (sub reason)
  "End SUB, telling its viewer REASON if it is connected and REASON is non-nil."
  (let ((share (spyglass--sub-share sub)))
    (remhash (spyglass--sub-key sub) spyglass--subs)
    (when (and reason (spyglass--sub-live-p sub))
      (spyglass--send (spyglass--sub-connection sub)
                      (list :t "closed" :sub (spyglass--sub-id sub) :reason reason)))
    (setf (spyglass--sub-unacked sub) nil
          (spyglass--sub-connection sub) nil)
    (when (memq sub (spyglass-share-subs share))
      (setf (spyglass-share-subs share) (delq sub (spyglass-share-subs share)))
      (spyglass--share-event share 'unsubscribed (spyglass--sub-key sub))
      (unless (spyglass-share-subs share)
        (spyglass--deactivate share))
      (spyglass--sub-event sub 'freed))))

(defun spyglass--share-event (share event data)
  "Run `spyglass-share-state-functions' with SHARE, EVENT and DATA."
  (condition-case err
      (run-hook-with-args 'spyglass-share-state-functions share event data)
    (error (message "spyglass: error in share hook: %S" err))))

(defun spyglass--sub-event (sub event)
  "Run `spyglass--sub-functions' with SUB and EVENT, reporting errors."
  (dolist (f spyglass--sub-functions)
    (spyglass--guard (spyglass--sub-connection sub)
      (funcall f sub event))))

(defun spyglass--new-sub (connection share id key)
  "Return a new subscription of CONNECTION's peer, ID, to SHARE under KEY."
  (unless (spyglass-share-mode share)
    (spyglass--activate share))
  (let ((sub (spyglass--make-sub
              :key key :id id :connection connection :share share
              :epoch (substring (spyglass-wire-random-token) 0 16)
              :reset t :styles (make-hash-table :test #'equal)
              :faces (make-hash-table :test #'eq)
              :ov-dirty (make-hash-table :test #'eql))))
    (with-current-buffer (spyglass-share-buffer share)
      (save-restriction
        (widen)
        ;; Changes not captured yet are in the snapshot already.
        (spyglass--capture share)
        (setf (spyglass--sub-intervals sub)
              (and (> (buffer-size) 0) (list (list 1 (buffer-size) 0)))
              (spyglass--sub-ct sub) (float-time))))
    (when (spyglass-share-ovs share)
      (maphash (lambda (ov rec) (puthash (aref rec 0) ov (spyglass--sub-ov-dirty sub)))
               (spyglass-share-ovs share)))
    (push sub (spyglass-share-subs share))
    (puthash key sub spyglass--subs)
    (spyglass--share-stat share :snapshots)
    (spyglass--share-event share 'subscribed key)
    (spyglass--sub-event sub 'opened)
    sub))

(defun spyglass--sub-of (connection header)
  "Return the subscription HEADER names on CONNECTION, or nil."
  (let ((sub (gethash (format "%s/%s" (spyglass-connection-peer-id connection)
                              (plist-get header :sub))
                      spyglass--subs)))
    (and sub (eq (spyglass--sub-connection sub) connection) sub)))

(defun spyglass--share-by-id (id)
  "Return the share identified by ID, or nil."
  (cl-find id spyglass--shares :key #'spyglass-share-id :test #'equal))

(defun spyglass--owner-open (connection header _payload)
  "Open a subscription for CONNECTION's peer as HEADER asks."
  (let* ((id (plist-get header :sub))
         (share (spyglass--share-by-id (plist-get header :share)))
         (key (format "%s/%s" (spyglass-connection-peer-id connection) id))
         (resume (plist-get header :resume))
         (existing (gethash key spyglass--subs)))
    (cond
     ((not (natnump id))
      (spyglass--report connection "Open without a subscription identifier"))
     ((or (not share) (not (buffer-live-p (spyglass-share-buffer share))))
      (spyglass--send connection (list :t "closed" :sub id :reason "no such share")))
     ((and existing resume (eq (spyglass--sub-share existing) share)
           (equal (plist-get resume :epoch) (spyglass--sub-epoch existing))
           (natnump (plist-get resume :seq))
           (>= (plist-get resume :seq) (spyglass--sub-acked existing))
           (<= (plist-get resume :seq) (spyglass--sub-seq existing)))
      (spyglass--resume existing connection (plist-get resume :seq)))
     (t
      ;; The viewer asked for a snapshot under the same identifier.
      (when existing (spyglass--sub-free existing nil))
      (spyglass--new-sub connection share id key)
      (spyglass--schedule-flush)))))

(defun spyglass--resume (sub connection seq)
  "Resume SUB on CONNECTION after frame SEQ, which its viewer applied."
  (let* ((share (spyglass--sub-share sub))
         (kept (cl-remove-if (lambda (rec) (<= (car rec) seq))
                             (spyglass--sub-unacked sub))))
    (setf (spyglass--sub-connection sub) connection
          (spyglass--sub-orphaned sub) nil
          (spyglass--sub-acked sub) seq
          (spyglass--sub-unacked sub) kept
          (spyglass--sub-inflight sub) (apply #'+ (mapcar #'cadr kept)))
    (spyglass--share-stat share :resumes)
    (spyglass--send connection (list :t "resumed" :sub (spyglass--sub-id sub) :seq seq
                                     :resent (length kept)))
    (let ((spyglass--sending t))
      (dolist (rec (reverse kept))
        (spyglass--send-frame connection (nth 3 rec))))
    (spyglass--sub-event sub 'resumed)
    (spyglass--schedule-flush)))

(defun spyglass--owner-close (connection header _payload)
  "End the subscription HEADER names, which CONNECTION's peer closed."
  (when-let* ((sub (spyglass--sub-of connection header)))
    (setf (spyglass--sub-connection sub) nil)
    (spyglass--sub-free sub "closed")))

(defun spyglass--owner-ack (connection header _payload)
  "Release the frames CONNECTION's peer acknowledges in HEADER."
  (when-let* ((sub (spyglass--sub-of connection header))
              (seq (plist-get header :seq)))
    (when (and (natnump seq) (> seq (spyglass--sub-acked sub)))
      (let ((now (float-time)) (freed 0) kept
            (share (spyglass--sub-share sub)))
        (dolist (rec (spyglass--sub-unacked sub))
          (if (> (car rec) seq)
              (push rec kept)
            (setq freed (+ freed (cadr rec)))
            (when (nth 2 rec)
              (spyglass--share-sample share :latency (- now (nth 2 rec))))))
        (setf (spyglass--sub-unacked sub) (nreverse kept)
              (spyglass--sub-acked sub) seq
              (spyglass--sub-inflight sub) (- (spyglass--sub-inflight sub) freed))
        (when (or (spyglass--sub-intervals sub)
                  (> (hash-table-count (spyglass--sub-ov-dirty sub)) 0))
          (spyglass--schedule-flush))))))

(defun spyglass--owner-fontify (connection header _payload)
  "Fontify the range CONNECTION's peer displays, as HEADER asks."
  (when-let* ((sub (spyglass--sub-of connection header))
              (share (spyglass--sub-share sub))
              ((spyglass-share-jit share))
              ((null (spyglass--sub-intervals sub))))
    (let ((beg (plist-get header :b)) (end (plist-get header :e)))
      (when (and (natnump beg) (natnump end))
        (with-current-buffer (spyglass-share-buffer share)
          (save-restriction
            (widen)
            (let ((beg (max (point-min) (min beg (point-max))))
                  (end (max (point-min) (min end (point-max)))))
              (when (< beg end)
                (with-demoted-errors "spyglass: fontifying: %S"
                  (jit-lock-fontify-now beg end))
                (spyglass-notify (current-buffer) beg end t)))))))))

(defun spyglass--owner-input (connection header _payload)
  "Apply the input CONNECTION's peer sends in HEADER."
  (when-let* ((sub (spyglass--sub-of connection header)))
    (let* ((share (spyglass--sub-share sub))
           (kind (pcase (plist-get header :k) ("s" 'string) ("k" 'key)))
           (value (plist-get header :v))
           (problem
            (cond
             ((spyglass-share-read-only share) "this share is read-only")
             ((not (and kind (stringp value))) "malformed input")
             ((and (eq kind 'key) (not (key-valid-p value))) "invalid key")
             (t (condition-case err
                    (with-current-buffer (spyglass-share-buffer share)
                      (funcall (or (spyglass-share-input share)
                                   spyglass-input-function
                                   #'spyglass-default-input)
                               kind value)
                      nil)
                  (error (error-message-string err)))))))
      (if problem
          (spyglass--send connection (list :t "err" :sub (spyglass--sub-id sub)
                                           :msg problem))
        (spyglass--sub-event sub 'input)))))

(defun spyglass--owner-request (connection header _payload)
  "Answer request HEADER from CONNECTION's peer."
  (let ((rid (plist-get header :rid)))
    (pcase (plist-get header :op)
      ("list" (spyglass--send connection (list :t "res" :rid rid
                                               :shares (spyglass--share-list))))
      (_ (spyglass--send connection (list :t "res" :rid rid
                                          :error "unknown request"))))))

(defun spyglass--share-list ()
  "Return the shares described for peers, as a vector."
  (vconcat
   (mapcar (lambda (share)
             (let ((buffer (spyglass-share-buffer share)))
               (list :id (spyglass-share-id share)
                     :name (spyglass-wire-plain-string (buffer-name buffer))
                     :mode (spyglass-wire-plain-string
                            (with-current-buffer buffer
                              (condition-case nil (format-mode-line mode-name)
                                (error ""))))
                     :size (buffer-size buffer)
                     :input (if (spyglass-share-read-only share) :false t))))
           (reverse (cl-remove-if-not (lambda (s) (buffer-live-p (spyglass-share-buffer s)))
                                      spyglass--shares)))))

(defun spyglass--announce-shares ()
  "Tell every connected peer the list of shares."
  (let ((list (spyglass--share-list)))
    (dolist (connection (spyglass-connections))
      (when (spyglass-connection-live-p connection)
        (spyglass--send connection (list :t "shares" :list list))))))

(defun spyglass--owner-attach (connection)
  "Tell CONNECTION's new peer our shares."
  (spyglass--send connection (list :t "shares" :list (spyglass--share-list))))

(defun spyglass--owner-detach (connection final)
  "Orphan the subscriptions of CONNECTION's peer, or end them if FINAL."
  (let (subs)
    (maphash (lambda (_key sub)
               (when (eq (spyglass--sub-connection sub) connection)
                 (push sub subs)))
             spyglass--subs)
    (dolist (sub subs)
      (if final
          (progn (setf (spyglass--sub-connection sub) nil)
                 (spyglass--sub-free sub "disconnected"))
        (setf (spyglass--sub-connection sub) nil
              (spyglass--sub-orphaned sub) (float-time))
        (spyglass--sub-event sub 'orphaned)))))

(spyglass--register-handler "open" #'spyglass--owner-open)
(spyglass--register-handler "close" #'spyglass--owner-close)
(spyglass--register-handler "ack" #'spyglass--owner-ack)
(spyglass--register-handler "fl" #'spyglass--owner-fontify)
(spyglass--register-handler "in" #'spyglass--owner-input)
(spyglass--register-handler "req" #'spyglass--owner-request)
(add-hook 'spyglass--attach-functions #'spyglass--owner-attach)
(add-hook 'spyglass--detach-functions #'spyglass--owner-detach)

;;;; Input

(defconst spyglass--key-sequences
  '(("RET" . "\r") ("<return>" . "\r") ("TAB" . "\t") ("<tab>" . "\t")
    ("DEL" . "\d") ("<backspace>" . "\d") ("ESC" . "\e") ("<escape>" . "\e")
    ("SPC" . " ") ("<up>" . "\e[A") ("<down>" . "\e[B") ("<right>" . "\e[C")
    ("<left>" . "\e[D") ("<home>" . "\e[H") ("<end>" . "\e[F")
    ("<insert>" . "\e[2~") ("<delete>" . "\e[3~") ("<deletechar>" . "\e[3~")
    ("<prior>" . "\e[5~") ("<next>" . "\e[6~") ("<backtab>" . "\e[Z")
    ("<f1>" . "\eOP") ("<f2>" . "\eOQ") ("<f3>" . "\eOR") ("<f4>" . "\eOS")
    ("<f5>" . "\e[15~") ("<f6>" . "\e[17~") ("<f7>" . "\e[18~")
    ("<f8>" . "\e[19~") ("<f9>" . "\e[20~") ("<f10>" . "\e[21~")
    ("<f11>" . "\e[23~") ("<f12>" . "\e[24~"))
  "Keys and the bytes a terminal sends for them.")

(defun spyglass-key-bytes (key)
  "Return the bytes a terminal sends for KEY, in `key-description' form.
Nil for keys a terminal cannot send."
  (or (cdr (assoc key spyglass--key-sequences))
      (let* ((events (key-parse key))
             (event (and (= (length events) 1) (aref events 0))))
        (when (integerp event)
          (let* ((meta (/= 0 (logand event #x8000000)))
                 (base (logand event (lognot #x8000000)))
                 (char (cond ((< base #x400000) base)
                             ;; C- on a character with no ASCII control.
                             ((and (/= 0 (logand base #x4000000))
                                   (<= ?@ (logand base #x3fffff) ?_))
                              (- (logand base #x3fffff) ?@))
                             ((and (/= 0 (logand base #x4000000))
                                   (<= ?a (logand base #x3fffff) ?z))
                              (- (logand base #x3fffff) ?`)))))
            (when (and char (characterp char))
              (concat (and meta "\e")
                      (encode-coding-string (string char) 'utf-8))))))
      (and (string-prefix-p "M-" key)
           (let ((rest (spyglass-key-bytes (substring key 2))))
             (and rest (concat "\e" rest))))))

(defun spyglass--comint-input (kind value)
  "Apply a viewer's input of KIND and VALUE in this comint buffer."
  (let* ((process (get-buffer-process (current-buffer)))
         (mark (and process (process-mark process))))
    (if (eq kind 'string)
        (save-excursion
          (goto-char (point-max))
          (insert value))
      (pcase value
        ((or "RET" "<return>")
         (goto-char (point-max))
         (funcall (or spyglass-submit-function #'comint-send-input)))
        ((or "DEL" "<backspace>")
         (when (and mark (> (point-max) mark))
           (delete-region (1- (point-max)) (point-max))))
        ("C-c" (comint-interrupt-subjob))
        ("C-d" (if (and mark (= (point-max) mark))
                   (comint-send-eof)
                 (delete-region (1- (point-max)) (point-max))))
        ("C-u" (goto-char (point-max)) (comint-kill-input))
        ((or "M-p" "<up>") (goto-char (point-max)) (comint-previous-input 1))
        ((or "M-n" "<down>") (goto-char (point-max)) (comint-next-input 1))
        ("TAB" (save-excursion (goto-char (point-max)) (insert "\t")))
        (_ (user-error "Key %s is not forwarded to comint buffers" value))))))

(defun spyglass--text-input (kind value)
  "Apply a viewer's input of KIND and VALUE in this editable buffer, at point."
  (if (eq kind 'string)
      (insert value)
    (pcase value
      ((or "RET" "<return>") (insert "\n"))
      ((or "TAB" "<tab>") (insert "\t"))
      ((or "DEL" "<backspace>") (unless (bobp) (delete-char -1)))
      ((or "C-d" "<delete>" "<deletechar>") (unless (eobp) (delete-char 1)))
      ((or "<left>" "C-b") (unless (bobp) (forward-char -1)))
      ((or "<right>" "C-f") (unless (eobp) (forward-char 1)))
      ((or "<up>" "C-p") (forward-line -1))
      ((or "<down>" "C-n") (forward-line 1))
      ((or "<home>" "C-a") (beginning-of-line))
      ((or "<end>" "C-e") (end-of-line))
      ("SPC" (insert " "))
      (_ (let ((bytes (spyglass-key-bytes value)))
           (if (and bytes (= (length bytes) 1) (>= (aref bytes 0) 32))
               (insert (decode-coding-string bytes 'utf-8))
             (user-error "Key %s is not forwarded to text buffers" value)))))))

(defun spyglass-default-input (kind value)
  "Apply a viewer's input of KIND and VALUE in the current buffer.
In comint buffers, text goes to the input after the process mark and
RET runs `spyglass-submit-function'; in other buffers with a process,
text and keys go to the process as a terminal would send them; in
other editable buffers, text is inserted at point.  Signals an error
when the buffer cannot take the input."
  (cond
   ((derived-mode-p 'comint-mode) (spyglass--comint-input kind value))
   ((get-buffer-process (current-buffer))
    (let ((bytes (if (eq kind 'string)
                     (encode-coding-string value 'utf-8)
                   (spyglass-key-bytes value))))
      (unless bytes (user-error "Key %s has no terminal sequence" value))
      (process-send-string (get-buffer-process (current-buffer)) bytes)))
   (buffer-read-only (user-error "Buffer %s is read-only" (buffer-name)))
   (t (spyglass--text-input kind value))))

;;;; Sharing

(cl-defun spyglass-share (&optional buffer &key id (read-only nil read-only-p)
                                    (input nil input-p) (capture 'auto capture-p)
                                    (needs-window 'auto needs-window-p))
  "Share BUFFER, the current buffer by default, and return its share.
ID is what peers open it by, the buffer's name by default.
READ-ONLY non-nil refuses viewers' input.  INPUT, a function, applies
viewers' input instead of `spyglass-input-function' or
`spyglass-default-input'.  CAPTURE is how changes are found: `auto'
\(the default), `shadow' or `direct'; see the commentary of
spyglass-owner.el.  NEEDS-WINDOW non-nil says the buffer changes only
while a window shows it, so that spyglass shows it in a hidden window
while viewers subscribe and no other window shows it; `auto', the
default, takes the buffer's `spyglass-needs-window', which adapters
set.  Sharing a shared buffer updates the options given.  Mirrors
cannot be shared."
  (let ((buffer (window-normalize-buffer buffer)))
    (when (local-variable-p 'spyglass--mirror buffer)
      (error "Buffer %s is a mirror, which spyglass does not share again"
             (buffer-name buffer)))
    (let ((share (buffer-local-value 'spyglass--share buffer)))
      (if share
          (progn
            (when read-only-p (setf (spyglass-share-read-only share) read-only))
            (when input-p (setf (spyglass-share-input share) input))
            (when needs-window-p (setf (spyglass-share-needs-window share) needs-window))
            (when capture-p (setf (spyglass-share-capture share) capture)))
        (let ((id (spyglass-wire-plain-string (or id (buffer-name buffer)))))
          (when (spyglass--share-by-id id)
            (error "A share named %s exists" id))
          (setq share (spyglass--make-share :id id :buffer buffer :read-only read-only
                                            :input input :capture capture
                                            :needs-window needs-window))
          (with-current-buffer buffer
            (setq spyglass--share share)
            (add-hook 'after-change-functions #'spyglass--after-change nil t)
            (add-hook 'before-change-functions #'spyglass--before-change nil t)
            (add-hook 'kill-buffer-hook #'spyglass--kill-buffer nil t))
          (push share spyglass--shares)
          (run-hook-with-args 'spyglass-share-functions share)))
      (setf (spyglass-share-meta-time share) 0.0)
      (spyglass--announce-shares)
      share)))

(defun spyglass--unshare (share reason)
  "Stop sharing SHARE, telling its viewers REASON."
  (dolist (sub (spyglass-share-subs share))
    (spyglass--sub-free sub reason))
  (when (spyglass-share-mode share) (spyglass--deactivate share))
  (setq spyglass--shares (delq share spyglass--shares))
  (let ((buffer (spyglass-share-buffer share)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (kill-local-variable 'spyglass--share)
        (remove-hook 'after-change-functions #'spyglass--after-change t)
        (remove-hook 'before-change-functions #'spyglass--before-change t)
        (remove-hook 'kill-buffer-hook #'spyglass--kill-buffer t))))
  (spyglass--share-event share 'unshared reason)
  (spyglass--announce-shares))

(defun spyglass-unshare (buffer-or-share)
  "Stop sharing BUFFER-OR-SHARE, a buffer or a share.
Its viewers' mirrors close."
  (let ((share (if (spyglass-share-p buffer-or-share)
                   buffer-or-share
                 (buffer-local-value 'spyglass--share
                                     (window-normalize-buffer buffer-or-share)))))
    (when share (spyglass--unshare share "unshared"))))

(defun spyglass-shares ()
  "Return the shares of this Emacs, oldest first."
  (reverse spyglass--shares))

(defun spyglass-share-of (buffer)
  "Return BUFFER's share, or nil."
  (buffer-local-value 'spyglass--share (window-normalize-buffer buffer)))

(defun spyglass-share-viewers (share)
  "Return the keys of SHARE's subscriptions, connected or waiting."
  (mapcar #'spyglass--sub-key (spyglass-share-subs share)))

(provide 'spyglass-owner)
;;; spyglass-owner.el ends here
