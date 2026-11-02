;;; spyglass-bench-common.el --- Scenarios of spyglass's benchmarks  -*- lexical-binding: t; -*-

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

;; What both Emacs processes of a benchmark share: the scenarios an
;; owner plays, measuring the owner and the viewer, digests of a
;; buffer's text and mirrored properties to compare across processes,
;; and the guard that kills an Emacs growing past its memory limit.
;;
;; An owner's scenario fills or writes the buffer shared as "bench":
;;
;; - large: SIZE characters of lines, a few in faces, before sharing;
;;   the viewer's snapshot is measured.
;; - appends: COUNT appends of a line in three insertions each, as a
;;   process filter receives them, yielding every few hundred.
;; - stream: RATE characters a second for SECONDS, in 10 ms ticks,
;;   lines in faces, the top trimmed to KEEP characters.
;; - term: a ROWS by COLS screen at the end of a scrollback, rewritten
;;   HZ times a second with `inhibit-modification-hooks' bound, as a
;;   terminal does: random runs of cells in random faces, and a scroll
;;   now and then; HINT non-nil tells spyglass the screen's range, as
;;   an adapter does.
;; - overlays: text streamed in paragraphs, each given overlays with
;;   faces, display strings and before-strings, some removed and
;;   remade, as agent-shell's Markdown rendering does.
;; - terminal: a real terminal of KIND, Eat, vterm or Ghostel, running
;;   `seq 1 COUNT'.
;; - resize: a buffer of lines its viewers size as a terminal, without
;;   a process, so that it runs on every host; the viewer shows it in
;;   windows of several sizes (`spyglass-bench-show-mirror').

;;; Code:

(require 'cl-lib)
(require 'spyglass)

(defvar spyglass-bench-rss-limit (* 2 1024 1024)
  "Resident memory, in KB, past which a benchmark Emacs kills itself.")

(defvar spyglass-bench--rss-peak 0
  "Highest resident memory seen since the last reset, in KB.")

(defvar spyglass-bench--guard-timer nil
  "Timer watching this Emacs's memory.")

(defvar spyglass-bench-abort-function nil
  "Function called with a message before a benchmark Emacs kills itself.")

(defun spyglass-bench-rss ()
  "Return this Emacs's resident memory in KB, or 0 if unknown."
  (or (alist-get 'rss (process-attributes (emacs-pid))) 0))

(defun spyglass-bench--check-memory ()
  "Note the memory used; kill this Emacs past `spyglass-bench-rss-limit'."
  (let ((rss (spyglass-bench-rss)))
    (setq spyglass-bench--rss-peak (max spyglass-bench--rss-peak rss))
    (when (> rss spyglass-bench-rss-limit)
      (let ((msg (format "ABORT: resident memory %d MB over %d MB"
                         (/ rss 1024) (/ spyglass-bench-rss-limit 1024))))
        (when spyglass-bench-abort-function
          (ignore-errors (funcall spyglass-bench-abort-function msg)))
        (message "%s" msg)
        (kill-emacs 3)))))

(defun spyglass-bench-start-guard ()
  "Watch this Emacs's memory every 100 ms."
  (unless spyglass-bench--guard-timer
    (setq spyglass-bench--guard-timer
          (run-with-timer 0.1 0.1 #'spyglass-bench--check-memory))))

(defun spyglass-bench-reset-peak ()
  "Start measuring the peak of resident memory afresh."
  (setq spyglass-bench--rss-peak (spyglass-bench-rss)))

(defun spyglass-bench-step (&optional most)
  "Process events for at most MOST seconds (0.05), or until the next timer.
`accept-process-output' runs timers that are due, but does not wake
for a timer armed while it waits (in `wait_reading_process_output',
only callers redisplaying retry), so a batch Emacs waiting longer
than its timers' intervals delays them.  An interactive Emacs waits
in its command loop, which retries."
  (let* ((timer (car timer-list))
         (until (and timer (timer-until timer nil))))
    (accept-process-output nil (max 0.001 (min (or most 0.05) (or until 0.05))))))

(defun spyglass-bench-wait (predicate &optional timeout)
  "Process events until PREDICATE holds or TIMEOUT seconds (300) pass.
Return PREDICATE's last value."
  (let ((end (+ (float-time) (or timeout 300))) value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) end))
      (spyglass-bench-step))
    value))

(defun spyglass-bench-cpu ()
  "Return the processor time this Emacs used, in seconds."
  (let ((cpu (current-cpu-time)))
    (/ (float (car cpu)) (cdr cpu))))

(defun spyglass-bench-gc ()
  "Return the garbage collections so far and their time, as a cons."
  (cons gcs-done gc-elapsed))

;;;; Digests

(defun spyglass-bench-digest (buffer &optional owner)
  "Return a digest of BUFFER's text and mirrored properties, as a plist.
OWNER non-nil reads BUFFER as an owner's buffer, else as a mirror.
The digests of a buffer and of its exact mirror are equal."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let ((flf (memq 'font-lock-face (alist-get 'face char-property-alias-alist)))
            runs)
        (dolist (iv (object-intervals (current-buffer)))
          (let ((props (if owner
                           (spyglass-wire-sanitize-props
                            (spyglass--raw-props (caddr iv) spyglass-text-properties flf)
                            spyglass-wire-style-properties)
                         (spyglass-wire-sanitize-props (caddr iv)
                                                       spyglass-wire-style-properties))))
            (if (and runs (equal (nth 2 (car runs)) props))
                (setcar (cdar runs) (cadr iv))
              (push (list (car iv) (cadr iv) props) runs))))
        (let (ovs)
          (dolist (ov (overlays-in (point-min) (point-max)))
            (let ((props (if owner (spyglass--overlay-props ov)
                           (and (overlay-get ov 'spyglass-id) (overlay-properties ov)))))
              (when props
                (push (list (overlay-start ov) (overlay-end ov)
                            (spyglass-wire-sanitize-props props
                                                          spyglass-wire-style-properties))
                      ovs))))
          (list :size (buffer-size)
                :text (buffer-hash)
                :props (md5 (prin1-to-string
                             (cl-remove-if-not #'caddr (nreverse runs))))
                :overlays (md5 (prin1-to-string
                                (sort ovs (lambda (a b)
                                            (or (< (car a) (car b))
                                                (and (= (car a) (car b))
                                                     (< (cadr a) (cadr b))))))))
                :overlay-count (length ovs)
                :overlay-list (and (getenv "SPYGLASS_BENCH_DEBUG")
                                   (vconcat (mapcar #'prin1-to-string ovs)))))))))

;;;; Text

(defun spyglass-bench--line (n width)
  "Return line N, WIDTH characters and a newline."
  (let ((head (format "%08d " n)))
    (concat head (make-string (- width (length head)) (+ ?a (% n 26))) "\n")))

(defconst spyglass-bench--faces
  '(bold italic (:foreground "red") (:foreground "#3070ff" :weight bold)
         (:background "gray20") (:foreground "green" :underline t))
  "Faces the scenarios use: named, and anonymous by value.")

(defun spyglass-bench--face (n)
  "Return the face of item N, or nil for most."
  (and (zerop (% n 5)) (nth (% (/ n 5) (length spyglass-bench--faces))
                            spyglass-bench--faces)))

(defun spyglass-bench--random-string (n)
  "Return N random printable characters."
  (let ((s (make-string n ?x)))
    (dotimes (i n) (aset s i (+ 33 (random 94))))
    s))

;;;; Producers

(defun spyglass-bench--every (interval function)
  "Call FUNCTION every INTERVAL seconds until it returns non-nil.
Each call is a one-shot timer armed after the last call returned, as
output arriving from a process comes: a repeating timer late on its
schedule would run again at once, as many as `timer-max-repeats'
times, ahead of every other timer."
  (run-with-timer interval nil
                  (lambda ()
                    (unless (funcall function)
                      (spyglass-bench--every interval function)))))

(defun spyglass-bench-fill-large (buffer size)
  "Fill BUFFER with about SIZE characters of lines, a few in faces."
  (with-current-buffer buffer
    (let* ((width 100)
           (block-lines 10000)
           (block (apply #'concat
                         (cl-loop for i below block-lines
                                  collect (let ((line (spyglass-bench--line i width)))
                                            (if (spyglass-bench--face i)
                                                (propertize line 'face (spyglass-bench--face i))
                                              line)))))
           (blocks (max 1 (/ size (length block)))))
      (dotimes (_ blocks) (insert block)))))

(defun spyglass-bench-run-appends (buffer args done)
  "Append ARGS's :count lines to BUFFER, three insertions each; then call DONE.
Two hundred lines every millisecond."
  (let ((count (plist-get args :count)) (i 0))
    (spyglass-bench--every
     0.001
     (lambda ()
       (cond
        ((not (buffer-live-p buffer)) t)
        (t (with-current-buffer buffer
             (dotimes (_ 200)
               (when (< i count)
                 (goto-char (point-max))
                 (insert (format "%06d " i))
                 (insert (let ((s (md5 (number-to-string i))))
                           (if (spyglass-bench--face i)
                               (propertize s 'face (spyglass-bench--face i))
                             s)))
                 (insert "\n")
                 (setq i (1+ i)))))
           (when (>= i count)
             (funcall done)
             t))))))
  nil)

(defun spyglass-bench-run-stream (buffer args done)
  "Write ARGS's :rate characters a second to BUFFER for :seconds; call DONE.
The top is trimmed to :keep characters, as a terminal's scrollback."
  (let* ((rate (plist-get args :rate)) (seconds (plist-get args :seconds))
         (keep (plist-get args :keep))
         (start (float-time)) (written 0) (width 120)
         (lines (cl-loop for i below 64
                         collect (let ((line (spyglass-bench--line i width)))
                                   (if (spyglass-bench--face i)
                                       (propertize line 'face (spyglass-bench--face i))
                                     line))))
         (chunk (apply #'concat lines)))
    (spyglass-bench--every
     0.01
     (lambda ()
       (if (not (buffer-live-p buffer))
           t
         (let* ((elapsed (- (float-time) start))
                (due (- (floor (* rate (min elapsed seconds))) written)))
           (with-current-buffer buffer
             (goto-char (point-max))
             (while (> due 0)
               (let ((piece (if (>= due (length chunk)) chunk
                              (substring chunk 0 due))))
                 (insert piece)
                 (setq due (- due (length piece))
                       written (+ written (length piece)))))
             (when (> (buffer-size) keep)
               (delete-region (point-min)
                              (save-excursion
                                (goto-char (- (point-max) keep))
                                (line-beginning-position)))))
           (when (>= elapsed seconds)
             (funcall done)
             t))))))
  nil)

(defun spyglass-bench-run-term (buffer args done)
  "Rewrite a terminal screen in BUFFER as ARGS say; call DONE when finished.
ARGS has :rows, :cols, :hz, :seconds and :hint."
  (let* ((rows (plist-get args :rows)) (cols (plist-get args :cols))
         (hz (plist-get args :hz)) (seconds (plist-get args :seconds))
         (hint (plist-get args :hint))
         (start (float-time)) (frames 0) (scrolled 0))
    (with-current-buffer buffer
      (let ((inhibit-modification-hooks t))
        (dotimes (_ rows)
          (insert (spyglass-bench--random-string cols) "\n")))
      (when hint (spyglass-notify buffer (point-min) (point-max))))
    (spyglass-bench--every
     (/ 1.0 hz)
     (lambda ()
       (if (not (buffer-live-p buffer))
           t
         (with-current-buffer buffer
           (let ((inhibit-modification-hooks t)
                 (screen nil))
             ;; Scroll: a line enters at the bottom, the top one joins
             ;; the scrollback, trimmed to 2000 lines.
             (when (zerop (random 4))
               (goto-char (point-max))
               (insert (spyglass-bench--random-string cols) "\n")
               (setq scrolled (1+ scrolled))
               (when (> scrolled 2000)
                 (goto-char (point-min))
                 (delete-region (point-min) (line-beginning-position 2))
                 (setq scrolled (1- scrolled))))
             (goto-char (point-max))
             (forward-line (- rows))
             (setq screen (point))
             ;; Rewrite a few runs of cells in place.
             (dotimes (_ (max 1 (/ rows 8)))
               (goto-char screen)
               (forward-line (random rows))
               (let* ((col (random (- cols 10)))
                      (len (min (- cols col) (+ 5 (random 40))))
                      (beg (+ (point) col)))
                 (delete-region beg (+ beg len))
                 (goto-char beg)
                 (insert (propertize (spyglass-bench--random-string len)
                                     'face (nth (random (length spyglass-bench--faces))
                                                spyglass-bench--faces)))))
             (goto-char (point-max))
             (when hint (spyglass-notify buffer screen (point-max)))))
         (setq frames (1+ frames))
         (when (>= (- (float-time) start) seconds)
           (funcall done)
           t)))))
  nil)

(defun spyglass-bench-run-overlays (buffer args done)
  "Stream ARGS's :paragraphs paragraphs into BUFFER with overlays; call DONE.
Every paragraph gets a heading face, a display string and a
before-string; every tenth paragraph's overlays are removed and
made again, as a Markdown renderer redraws."
  (let ((count (plist-get args :paragraphs)) (i 0) (made nil))
    (spyglass-bench--every
     0.005
     (lambda ()
       (if (not (buffer-live-p buffer))
           t
         (with-current-buffer buffer
           (dotimes (_ 5)
             (when (< i count)
               (goto-char (point-max))
               (let ((start (point)))
                 (insert (format "## Heading %d\n" i))
                 (insert (spyglass-bench--line i 80) (spyglass-bench--line (1+ i) 80))
                 (let ((h (make-overlay start (+ start 3)))
                       (t1 (make-overlay (+ start 3) (+ start 10)))
                       (b (make-overlay (+ start 11) (+ start 11))))
                   (overlay-put h 'display "")
                   (overlay-put t1 'face 'bold)
                   (overlay-put b 'before-string (propertize "* " 'face 'italic))
                   (push (list h t1 b) made)))
               (when (zerop (% i 10))
                 (let ((cell (nthcdr (random (length made)) made)))
                   (setcar cell
                           (mapcar (lambda (ov)
                                     (let ((new (make-overlay (overlay-start ov)
                                                              (overlay-end ov))))
                                       (cl-loop for (p v) on (overlay-properties ov)
                                                by #'cddr
                                                do (overlay-put new p v))
                                       (delete-overlay ov)
                                       new))
                                   (car cell)))))
               (setq i (1+ i)))))
         (when (>= i count)
           (funcall done)
           t)))))
  nil)

(defvar spyglass-bench-eat-dir nil
  "Directory of eat.el, or nil when Eat is not available.")

(defvar spyglass-bench-vterm-dir nil
  "Directory of vterm.el and its built module, or nil.")

(defvar spyglass-bench-ghostel-dir nil
  "Directory of ghostel.el, or nil.")

(defvar spyglass-bench-ghostel-module-dir nil
  "Directory of Ghostel's native module, or nil.")

(defun spyglass-bench-read-environment ()
  "Locate the terminal emulators from the environment, as the Makefile sets it.
SPYGLASS_BENCH_WINDOW, if set, is `spyglass-window' in bytes."
  (let ((window (getenv "SPYGLASS_BENCH_WINDOW")))
    (when (and window (not (string-empty-p window)))
      (setq spyglass-window (string-to-number window))))
  (cl-flet ((dir (name) (let ((d (getenv name))) (and d (not (string-empty-p d)) d))))
    (setq spyglass-bench-eat-dir (dir "SPYGLASS_EAT_EL")
          spyglass-bench-vterm-dir (dir "SPYGLASS_VTERM_EL")
          spyglass-bench-ghostel-dir (dir "SPYGLASS_GHOSTEL_EL")
          spyglass-bench-ghostel-module-dir (dir "SPYGLASS_GHOSTEL_MODULE_DIR"))))

(defun spyglass-bench-terminal-available-p (kind)
  "Return non-nil when terminals of KIND can run in this Emacs."
  (and (not (memq system-type '(windows-nt ms-dos)))
       (pcase kind
         ("eat" spyglass-bench-eat-dir)
         ("vterm" spyglass-bench-vterm-dir)
         ("ghostel" (and spyglass-bench-ghostel-dir spyglass-bench-ghostel-module-dir)))))

(defvar vterm-kill-buffer-on-exit)
(defvar ghostel-kill-buffer-on-exit)

(defun spyglass-bench-load-terminal (kind)
  "Load the terminal emulator KIND and its adapter."
  (pcase kind
    ("eat" (add-to-list 'load-path spyglass-bench-eat-dir)
     (require 'eat) (require 'spyglass-eat))
    ("vterm" (add-to-list 'load-path spyglass-bench-vterm-dir)
     (require 'vterm) (require 'spyglass-vterm)
     ;; The digest is taken after the program exits.
     (setq vterm-kill-buffer-on-exit nil))
    ("ghostel" (add-to-list 'load-path spyglass-bench-ghostel-dir)
     (defvar ghostel-module-directory)
     (defvar ghostel-module-auto-install)
     (setq ghostel-module-directory spyglass-bench-ghostel-module-dir
           ghostel-module-auto-install nil)
     (require 'ghostel) (require 'spyglass-ghostel)
     (setq ghostel-kill-buffer-on-exit nil))))

(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function vterm-mode "vterm" ())
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(defvar vterm-shell)

(defun spyglass-bench-make-terminal (kind count)
  "Return a new terminal of KIND printing 1 to COUNT, in colors, then exiting."
  (spyglass-bench-load-terminal kind)
  (let* ((script (format "sleep 1; seq 1 %d; printf '\\033[1;31mdone\\033[0m\\n'; sleep 2"
                         count))
         (buffer
          (pcase kind
            ("eat" (eat-make "bench-eat" "/bin/sh" nil "-c" script))
            ("vterm" (with-current-buffer (generate-new-buffer "bench-vterm")
                       (let ((vterm-shell (format "/bin/sh -c %s"
                                                  (shell-quote-argument script))))
                         (vterm-mode))
                       (current-buffer)))
            ("ghostel" (let ((buffer (generate-new-buffer "bench-ghostel")))
                         ;; Ghostel renders buffers shown in a window only.
                         (set-window-buffer (selected-window) buffer)
                         (ghostel-exec buffer "/bin/sh" (list "-c" script))
                         buffer)))))
    (with-current-buffer buffer (rename-buffer "bench" t))
    buffer))

;;;; The owner's side

(defun spyglass-bench--sizable-refit ()
  "Take the size viewers give, as a terminal would, without a process."
  (funcall (default-value 'window-adjust-process-window-size-function) nil nil))

(defun spyglass-bench-make-sizable (buffer)
  "Fill BUFFER with lines and make it a terminal viewers size, process or not.
It takes the size of its viewers' windows, which its viewers learn: what
the resize scenario measures, on any host, Windows included, where Eat
has no pty."
  (with-current-buffer buffer
    (dotimes (i 100) (insert (spyglass-bench--line i 60)))
    (setq-local spyglass-refit-function #'spyglass-bench--sizable-refit)))

(defun spyglass-bench-show-mirror (buffer cols rows)
  "Show BUFFER in a window about COLS by ROWS cells, and report its size.
The window is cut from the frame of this batch Emacs.  Return its
cells, (COLUMNS . ROWS)."
  (delete-other-windows)
  (let ((window (selected-window)))
    (set-window-buffer window buffer)
    (let ((cells (spyglass-window-cells window)))
      (when (> (cdr cells) rows)
        (split-window window (+ rows (- (window-total-height window) (cdr cells)))))
      (when (> (car cells) cols)
        (split-window window (+ cols (- (window-total-width window) (car cells))) t)))
    (setf (spyglass--mirror-sent-size (buffer-local-value 'spyglass--mirror buffer)) nil)
    (spyglass--mirror-report-sizes)
    (spyglass-window-cells window)))

(defvar spyglass-bench--owner nil
  "Plist of the scenario the owner plays: its buffer and measurements.")

(defun spyglass-bench-owner-prepare (scenario args)
  "Make the buffer of SCENARIO with ARGS, share it as \"bench\", and describe it."
  (spyglass-bench-owner-cleanup)
  (garbage-collect)
  (let* ((start (float-time))
         (buffer (if (equal scenario "terminal")
                     (if (spyglass-bench-terminal-available-p (plist-get args :kind))
                         (spyglass-bench-make-terminal (plist-get args :kind)
                                                       (plist-get args :count))
                       (error "No %s here" (plist-get args :kind)))
                   (get-buffer-create "bench"))))
    ;; Terminals and shells keep no undo; a batch Emacs never trims it.
    (with-current-buffer buffer (setq buffer-undo-list t))
    (when (equal scenario "large")
      (spyglass-bench-fill-large buffer (plist-get args :size)))
    (when (equal scenario "resize")
      (spyglass-bench-make-sizable buffer))
    (let ((prepared (- (float-time) start))
          (share (spyglass-share buffer :id "bench")))
      (setq spyglass-bench--owner
            (list :buffer buffer :scenario scenario :args args :share share))
      (add-hook 'spyglass-share-state-functions #'spyglass-bench--owner-event)
      (list :prepare prepared :size (buffer-size buffer)
            :mode (symbol-name (or (spyglass-share-mode share) 'unsubscribed))))))

(defun spyglass-bench--owner-event (share event _data)
  "Note when the viewer subscribes to SHARE, the bench's, for EVENT."
  (when (and (eq event 'subscribed) spyglass-bench--owner
             (eq share (plist-get spyglass-bench--owner :share)))
    (setq spyglass-bench--owner
          (plist-put spyglass-bench--owner :subscribed (float-time)))))

(defun spyglass-bench-owner-synced-p (&optional sweeps)
  "Return non-nil when the bench's viewer has everything, acknowledged.
With SWEEPS, also wait until the share's sweep completed that many
rounds: changes of overlays alone, far from changed text, are found
by the sweep."
  (let* ((share (plist-get spyglass-bench--owner :share))
         (buffer (plist-get spyglass-bench--owner :buffer))
         (subs (and share (spyglass-share-subs share))))
    (and subs (buffer-live-p buffer)
         (or (null sweeps) (null (spyglass-share-mode share))
             (eq (spyglass-share-mode share) 'direct)
             (>= (spyglass-share-sweeps share) sweeps))
         (with-current-buffer buffer
           (and (= (buffer-chars-modified-tick) (spyglass-share-chars-tick share))
                (= (buffer-modified-tick) (spyglass-share-modiff share))
                (not (spyglass-share-hints share))
                (not (spyglass-share-hint-all share))))
         (cl-every (lambda (sub)
                     (and (null (spyglass--sub-intervals sub))
                          (zerop (hash-table-count (spyglass--sub-ov-dirty sub)))
                          (zerop (spyglass--sub-inflight sub))
                          (spyglass--sub-live-p sub)))
                   subs))))

(defun spyglass-bench--produce (scenario buffer args done)
  "Play SCENARIO's producer with ARGS in BUFFER, then call DONE."
  (pcase scenario
    ("appends" (spyglass-bench-run-appends buffer args done))
    ("stream" (spyglass-bench-run-stream buffer args done))
    ("term" (spyglass-bench-run-term buffer args done))
    ("overlays" (spyglass-bench-run-overlays buffer args done))
    ("terminal" (let ((timer nil))
             (setq timer (run-with-timer
                          0.05 0.05
                          (lambda ()
                            (unless (and (buffer-live-p buffer) (get-buffer-process buffer))
                              (cancel-timer timer)
                              (funcall done)))))))
    (_ (funcall done))))

(defun spyglass-bench-owner-baseline ()
  "Play the scenario's producer on a buffer no one sees; return its cost.
The cost is a plist of processor seconds and wall seconds."
  (let* ((scenario (plist-get spyglass-bench--owner :scenario))
         (args (plist-get spyglass-bench--owner :args))
         (buffer (with-current-buffer (get-buffer-create "bench-baseline")
                   (setq buffer-undo-list t)
                   (current-buffer)))
         (cpu (spyglass-bench-cpu)) (start (float-time)) (finished nil))
    (when (member scenario '("appends" "stream" "term" "overlays"))
      (spyglass-bench--produce scenario buffer args (lambda () (setq finished t)))
      (spyglass-bench-wait (lambda () finished) 300))
    (prog1 (list :cpu (- (spyglass-bench-cpu) cpu) :wall (- (float-time) start))
      (kill-buffer buffer))))

(defun spyglass-bench-owner-run (callback)
  "Play the producer, wait until the viewer has it all; call CALLBACK.
CALLBACK gets the owner's measurements as a plist."
  (let* ((scenario (plist-get spyglass-bench--owner :scenario))
         (args (plist-get spyglass-bench--owner :args))
         (buffer (plist-get spyglass-bench--owner :buffer))
         (share (plist-get spyglass-bench--owner :share))
         (subscribed (or (plist-get spyglass-bench--owner :subscribed) (float-time)))
         (baseline (and (plist-get args :baseline) (spyglass-bench-owner-baseline)))
         (gc (spyglass-bench-gc)) (cpu (spyglass-bench-cpu))
         (consed string-chars-consed)
         (start (float-time)) produced)
    (unless (equal scenario "large") (spyglass-share-reset-stats share))
    (spyglass-bench-reset-peak)
    (spyglass-bench--produce
     scenario buffer args
     (lambda ()
       (setq produced (float-time))
       (let ((timer nil)
             (sweeps (and (equal scenario "overlays")
                          (+ 2 (spyglass-share-sweeps share)))))
         (setq timer
               (run-with-timer
                0.005 0.005
                (lambda ()
                  (when (spyglass-bench-owner-synced-p sweeps)
                    (cancel-timer timer)
                    (let ((end (float-time)) (gc2 (spyglass-bench-gc)))
                      (funcall
                       callback
                       (list :scenario scenario
                             :produce (- produced start)
                             :synced (- end (if (equal scenario "large") subscribed start))
                             :drain (- end produced)
                             :cpu (- (spyglass-bench-cpu) cpu)
                             :baseline baseline
                             :gcs (- (car gc2) (car gc)) :gc-time (- (cdr gc2) (cdr gc))
                             :rss-peak spyglass-bench--rss-peak
                             :string-mb (/ (- string-chars-consed consed) 1e6)
                             :gc-pct gc-cons-percentage :gc-threshold gc-cons-threshold
                             :stats (spyglass-bench--plain-stats
                                     (spyglass-share-stats share))
                             :digest (spyglass-bench-digest buffer t)))))))))))))

(defun spyglass-bench--plain-stats (stats)
  "Return STATS with only what JSON carries."
  (cl-loop for (k v) on stats by #'cddr
           when (or (numberp v) (consp v))
           nconc (list k v)))

(defun spyglass-bench-owner-cleanup ()
  "Unshare and kill the bench's buffer."
  (when spyglass-bench--owner
    (let ((buffer (plist-get spyglass-bench--owner :buffer)))
      (when (buffer-live-p buffer)
        (spyglass-unshare buffer)
        (let ((kill-buffer-query-functions nil))
          (when (get-buffer-process buffer) (delete-process (get-buffer-process buffer)))
          (kill-buffer buffer)))))
  (setq spyglass-bench--owner nil)
  (garbage-collect))

;;;; The viewer's side

(defvar spyglass-bench--viewer nil
  "Plist of the mirror the viewer measures.")

(defun spyglass-bench-viewer-open (connection)
  "Open the mirror of \"bench\" on CONNECTION and start measuring."
  (spyglass-bench-viewer-cleanup)
  (garbage-collect)
  (spyglass-bench-reset-peak)
  (let ((buffer (spyglass-open connection "bench" :buffer-name "bench-mirror")))
    (setq spyglass-bench--viewer
          (list :buffer buffer :gc (spyglass-bench-gc) :cpu (spyglass-bench-cpu)
                :opened (float-time)))
    buffer))

(defun spyglass-bench-viewer-result ()
  "Return the viewer's measurements and the mirror's digest."
  (let* ((buffer (plist-get spyglass-bench--viewer :buffer))
         (gc (spyglass-bench-gc)) (gc0 (plist-get spyglass-bench--viewer :gc)))
    (list :gcs (- (car gc) (car gc0)) :gc-time (- (cdr gc) (cdr gc0))
          :cpu (- (spyglass-bench-cpu) (plist-get spyglass-bench--viewer :cpu))
          :rss-peak spyglass-bench--rss-peak
          :state (symbol-name (spyglass-mirror-state buffer))
          :stats (spyglass-bench--plain-stats (spyglass-mirror-stats buffer))
          :digest (spyglass-bench-digest buffer))))

(defun spyglass-bench-viewer-cleanup ()
  "Close and kill the mirror."
  (when spyglass-bench--viewer
    (let ((buffer (plist-get spyglass-bench--viewer :buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer))))
  (setq spyglass-bench--viewer nil)
  (garbage-collect))

(provide 'spyglass-bench-common)
;;; spyglass-bench-common.el ends here
