;;; spyglass-mirror.el --- Mirrors of buffers shared by spyglass peers  -*- lexical-binding: t; -*-

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

;; The viewer's side: a mirror is a read-only buffer in
;; `spyglass-mirror-mode' showing a peer's shared buffer.  It runs no
;; process and none of the owner's modes; it applies what the owner
;; sends, text, properties, overlays, its cursor and a description of
;; the buffer, after reducing every property to forms that show
;; without evaluating anything (see `spyglass-wire-sanitize-props').
;; Faces are sent by name, so that this Emacs's theme draws them, with
;; their attributes for faces this Emacs does not know.
;;
;; Keys typed in a mirror go to the owner, which applies them through
;; the shared buffer's input function: printing characters, RET, TAB,
;; DEL and arrows directly, other keys through `C-c' commands.
;;
;; A mirror outlives its connection's streams: when a stream breaks it
;; shows `reconnecting', and when the connection comes back it resumes
;; from the last update it applied, or starts over from a snapshot.
;;
;; A mirror of a terminal its owner sizes reports the size of the
;; window showing it, as it changes, and says when that window gets
;; focus (see spyglass-window.el).

;;; Code:

(require 'cl-lib)
(require 'spyglass-wire)
(require 'spyglass-conn)

(declare-function spyglass-share-of "spyglass-owner" (buffer))

(defcustom spyglass-mirror-follow t
  "Non-nil: windows on a mirror follow the owner's cursor.
A window follows while its point is where the owner's cursor was, so
that moving away to read stops following until you come back."
  :group 'spyglass
  :type 'boolean)

(defcustom spyglass-mirror-show-cursor t
  "Non-nil: show the owner's cursor in mirrors."
  :group 'spyglass
  :type 'boolean)

(defface spyglass-cursor '((t :inverse-video t))
  "Face of the owner's cursor in a mirror."
  :group 'spyglass)

(defface spyglass-state-live '((t :inherit success))
  "Face of a mirror's state while live."
  :group 'spyglass)

(defface spyglass-state-waiting '((t :inherit warning))
  "Face of a mirror's state while it connects or reconnects."
  :group 'spyglass)

(defface spyglass-state-closed '((t :inherit error))
  "Face of a mirror's state once closed."
  :group 'spyglass)

(defvar spyglass-mirror-state-functions nil
  "Abnormal hook run with (BUFFER STATE) when a mirror changes state.
STATE is one of `opening', `live', `reconnecting' and `closed'.")

(cl-defstruct (spyglass-mirror (:constructor spyglass--make-mirror)
                               (:conc-name spyglass--mirror-)
                               (:copier nil))
  "A mirror of a peer's shared buffer."
  (connection-ref nil :documentation "A reference to the connection to the owner.
See `spyglass--mirror-connection'.")
  (id nil :documentation "Subscription identifier, unique on the connection.")
  (share nil :documentation "The share's identifier.")
  (buffer nil :documentation "The mirror buffer.")
  (state 'opening :documentation "opening, live, reconnecting or closed.")
  (reason nil :documentation "Why the mirror closed.")
  (epoch nil :documentation "The owner's epoch of the subscription.")
  (seq 0 :documentation "Last update applied.")
  (acked 0 :documentation "Last update acknowledged.")
  (resync nil :documentation "Non-nil while waiting for a snapshot asked for.")
  (styles nil :documentation "Hash table of style identifiers and properties.")
  (faces nil :documentation "Hash table of face names and their attributes.")
  (overlays nil :documentation "Hash table of overlay identifiers and overlays.")
  (point nil :documentation "The owner's cursor.")
  (window-start nil :documentation "The owner's window start.")
  (cursor nil :documentation "Overlay showing the owner's cursor.")
  (meta nil :documentation "The owner's description of the buffer.")
  (header nil :documentation "The owner's header line, sanitized.")
  (callback nil :documentation "Function called with (MIRROR EVENT DATA).")
  (sizable nil :documentation "Non-nil when the owner sizes its terminal to viewers' windows.")
  (grid nil :documentation "The owner's terminal size, (COLUMNS . ROWS), or nil.")
  (sent-size nil :documentation "Window size last reported to the owner, or nil.")
  (stats nil :documentation "Plist of counters and samples."))

(defun spyglass--mirror-connection (mirror)
  "Return MIRROR's connection to its owner.
MIRROR holds it through a reference (see `spyglass--ref'): the
connection holds all its mirrors, which each printed again from every
mirror."
  (spyglass--deref (spyglass--mirror-connection-ref mirror)))

(defvar-local spyglass--mirror nil
  "The mirror shown in this buffer.")
(put 'spyglass--mirror 'permanent-local t)

(defun spyglass--mirror-event (mirror event data)
  "Call MIRROR's callback with EVENT and DATA, catching its errors."
  (when-let* ((callback (spyglass--mirror-callback mirror)))
    (condition-case err
        (funcall callback mirror event data)
      (error (message "spyglass: error in mirror callback: %S" err)))))

(defun spyglass--mirror-set-state (mirror state &optional reason)
  "Set MIRROR's STATE, for REASON, and tell those who listen."
  (unless (eq state (spyglass--mirror-state mirror))
    (setf (spyglass--mirror-state mirror) state)
    (when reason (setf (spyglass--mirror-reason mirror) reason))
    (when (and (eq state 'live) (spyglass--mirror-sizable mirror))
      (spyglass--mirror-schedule-report))
    (let ((buffer (spyglass--mirror-buffer mirror)))
      (when (buffer-live-p buffer)
        (force-mode-line-update)
        (condition-case err
            (run-hook-with-args 'spyglass-mirror-state-functions buffer state)
          (error (message "spyglass: error in mirror state hook: %S" err)))))
    (spyglass--mirror-event mirror 'state state)))

(defun spyglass--mirror-stat (mirror key &optional amount)
  "Add AMOUNT (1 by default) to MIRROR's counter KEY."
  (let ((stats (spyglass--mirror-stats mirror)))
    (setf (spyglass--mirror-stats mirror)
          (plist-put stats key (+ (or (plist-get stats key) 0) (or amount 1))))))

(defun spyglass--mirror-sample (mirror key value)
  "Add VALUE to MIRROR's ring of samples KEY."
  (let ((ring (plist-get (spyglass--mirror-stats mirror) key)))
    (unless ring
      (setq ring (spyglass--ring))
      (setf (spyglass--mirror-stats mirror)
            (plist-put (spyglass--mirror-stats mirror) key ring)))
    (spyglass--ring-add ring value)))

(defun spyglass-mirror-stats (mirror-or-buffer)
  "Return the counters and summarized samples of MIRROR-OR-BUFFER.
Samples are `:apply' (seconds applying each update) and
`:latency' (seconds from a change on the owner to its update applied
here, through the estimated clock offset)."
  (let ((mirror (spyglass--mirror-of mirror-or-buffer)) out)
    (cl-loop for (key value) on (spyglass--mirror-stats mirror) by #'cddr
             do (setq out (plist-put out key
                                     (if (vectorp value)
                                         (spyglass-ring-summary value)
                                       value))))
    out))

(defun spyglass--mirror-of (mirror-or-buffer)
  "Return the mirror MIRROR-OR-BUFFER is or shows, or signal an error."
  (cond
   ((spyglass-mirror-p mirror-or-buffer) mirror-or-buffer)
   ((buffer-local-value 'spyglass--mirror (window-normalize-buffer mirror-or-buffer)))
   (t (error "Not a spyglass mirror: %s" mirror-or-buffer))))

;;;; Mode

(defvar-keymap spyglass-mirror-mode-map
  :doc "Keymap of `spyglass-mirror-mode'."
  "<remap> <self-insert-command>" #'spyglass-mirror-self-insert
  "RET" #'spyglass-mirror-forward-key
  "TAB" #'spyglass-mirror-forward-key
  "DEL" #'spyglass-mirror-forward-key
  "<backspace>" #'spyglass-mirror-forward-key
  "<up>" #'spyglass-mirror-forward-key
  "<down>" #'spyglass-mirror-forward-key
  "<left>" #'spyglass-mirror-forward-key
  "<right>" #'spyglass-mirror-forward-key
  "<escape>" #'spyglass-mirror-forward-key
  "C-c C-c" #'spyglass-mirror-send-C-c
  "C-c C-d" #'spyglass-mirror-send-C-d
  "C-c C-z" #'spyglass-mirror-send-C-z
  "C-c C-e" #'spyglass-mirror-send-escape
  "C-c C-j" #'spyglass-mirror-send-C-j
  "C-c C-q" #'spyglass-mirror-send-next-key
  "C-c C-s" #'spyglass-mirror-send-string-command
  "C-c C-f" #'spyglass-mirror-toggle-follow
  "C-c C-k" #'spyglass-mirror-close-command)

(define-derived-mode spyglass-mirror-mode nil "Spyglass"
  "Major mode of mirrors of buffers shared by spyglass peers.
The buffer is read-only: it shows the owner's buffer as the owner
sends it.  Keys typed go to the owner when its share takes input.

\\{spyglass-mirror-mode-map}"
  :interactive nil
  (setq buffer-read-only t
        buffer-undo-list t
        ;; Faces come from the owner: font lock would strip them.
        font-lock-defaults nil)
  (setq-local font-lock-global-modes nil)
  (setq-local header-line-format '(:eval (spyglass--mirror-header-line)))
  (setq-local mode-name '(:eval (spyglass--mirror-mode-name)))
  (add-hook 'kill-buffer-hook #'spyglass--mirror-killed nil t))

(defun spyglass--mirror-mode-name ()
  "Return the mode name of a mirror: Spyglass and the owner's mode."
  (let ((meta (and spyglass--mirror (spyglass--mirror-meta spyglass--mirror))))
    (if (and meta (stringp (plist-get meta :mode)))
        (concat "Spyglass:" (plist-get meta :mode))
      "Spyglass")))

(defun spyglass--mirror-header-line ()
  "Return the header line of a mirror: where it comes from and its state."
  (when-let* ((mirror spyglass--mirror))
    (let* ((state (spyglass--mirror-state mirror))
           (label (spyglass-connection-label (spyglass--mirror-connection mirror)))
           (state-text
            (propertize (pcase state
                          ('live "live")
                          ('opening "opening")
                          ('reconnecting "reconnecting")
                          ('closed (format "closed%s"
                                           (if (spyglass--mirror-reason mirror)
                                               (concat ": " (spyglass--mirror-reason mirror))
                                             ""))))
                        'face (pcase state
                                ('live 'spyglass-state-live)
                                ('closed 'spyglass-state-closed)
                                (_ 'spyglass-state-waiting)))))
      (concat " " (spyglass--mirror-share mirror) "@" label " [" state-text "]"
              (let ((grid (spyglass--mirror-grid mirror)))
                (if grid (format " %d×%d" (car grid) (cdr grid)) ""))
              (if (spyglass--mirror-header mirror)
                  (concat "  " (string-replace "%" "%%" (spyglass--mirror-header mirror)))
                "")))))

;;;; Opening and closing

(defun spyglass--mirror-name (share-id connection)
  "Return a name for the mirror of SHARE-ID on CONNECTION."
  (format "*%s@%s*" (string-trim share-id "\\*+" "\\*+")
          (spyglass-connection-label connection)))

(cl-defun spyglass-open (connection share-id &key buffer-name callback)
  "Open a mirror of SHARE-ID, a share of CONNECTION's peer; return its buffer.
The buffer, named BUFFER-NAME or after the share and the peer, fills
asynchronously.  CALLBACK, if non-nil, is called with (MIRROR EVENT
DATA): `state' with the mirror's new state, `error' with a message
from the owner.  Opening a share already mirrored on CONNECTION
returns that mirror's buffer."
  (or (cl-loop for m being the hash-values of (spyglass-connection-mirrors connection)
               when (and (equal (spyglass--mirror-share m) share-id)
                         (not (eq (spyglass--mirror-state m) 'closed))
                         (buffer-live-p (spyglass--mirror-buffer m)))
               return (spyglass--mirror-buffer m))
      (let* ((buffer (generate-new-buffer
                      (or buffer-name (spyglass--mirror-name share-id connection))))
             (mirror (spyglass--make-mirror
                      :connection-ref (spyglass--ref connection)
                      :id (cl-incf (spyglass-connection-next-sub connection))
                      :share share-id :buffer buffer :callback callback
                      :styles (make-hash-table :test #'eql)
                      :faces (make-hash-table :test #'equal)
                      :overlays (make-hash-table :test #'eql))))
        (with-current-buffer buffer
          (spyglass-mirror-mode)
          (setq spyglass--mirror mirror))
        (puthash (spyglass--mirror-id mirror) mirror (spyglass-connection-mirrors connection))
        (when (spyglass-connection-live-p connection)
          (spyglass--mirror-send-open mirror))
        buffer)))

(defun spyglass--mirror-send-open (mirror &optional fresh)
  "Ask MIRROR's owner for its updates, resuming unless FRESH."
  (let ((connection (spyglass--mirror-connection mirror)))
    (spyglass--send connection
                    (append (list :t "open" :sub (spyglass--mirror-id mirror)
                                  :share (spyglass--mirror-share mirror))
                            (and (not fresh) (not (spyglass--mirror-resync mirror))
                                 (spyglass--mirror-epoch mirror)
                                 (list :resume (list :epoch (spyglass--mirror-epoch mirror)
                                                     :seq (spyglass--mirror-seq mirror))))))))

(defun spyglass-close (mirror-or-buffer)
  "Stop updating MIRROR-OR-BUFFER, a mirror or its buffer, and tell its owner.
The buffer stays, showing what it showed."
  (let* ((mirror (spyglass--mirror-of mirror-or-buffer))
         (connection (spyglass--mirror-connection mirror)))
    (unless (eq (spyglass--mirror-state mirror) 'closed)
      (when (spyglass-connection-live-p connection)
        (spyglass--send connection (list :t "close" :sub (spyglass--mirror-id mirror))))
      (spyglass--mirror-finish mirror "closed here"))))

(defun spyglass--mirror-finish (mirror reason)
  "Mark MIRROR closed for REASON and forget it on its connection."
  (remhash (spyglass--mirror-id mirror)
           (spyglass-connection-mirrors (spyglass--mirror-connection mirror)))
  (spyglass--mirror-set-state mirror 'closed reason))

(defun spyglass--mirror-killed ()
  "Close the mirror of the buffer being killed."
  (when (and spyglass--mirror (not (eq (spyglass--mirror-state spyglass--mirror) 'closed)))
    (spyglass-close spyglass--mirror)))

(defun spyglass-mirrors (&optional connection)
  "Return the buffers of the open mirrors, on CONNECTION if given."
  (let (out)
    (dolist (c (if connection (list connection) (spyglass-connections)))
      (maphash (lambda (_ m)
                 (when (buffer-live-p (spyglass--mirror-buffer m))
                   (push (spyglass--mirror-buffer m) out)))
               (spyglass-connection-mirrors c)))
    out))

(defun spyglass-mirror-state (mirror-or-buffer)
  "Return the state of MIRROR-OR-BUFFER: opening, live, reconnecting or closed."
  (spyglass--mirror-state (spyglass--mirror-of mirror-or-buffer)))

(defun spyglass-mirror-connection (mirror-or-buffer)
  "Return the connection of MIRROR-OR-BUFFER."
  (spyglass--mirror-connection (spyglass--mirror-of mirror-or-buffer)))

(defun spyglass-mirror-share-id (mirror-or-buffer)
  "Return the identifier of the share MIRROR-OR-BUFFER mirrors."
  (spyglass--mirror-share (spyglass--mirror-of mirror-or-buffer)))

(defun spyglass-mirror-owner-point (mirror-or-buffer)
  "Return the owner's cursor in MIRROR-OR-BUFFER, or nil before it is known."
  (spyglass--mirror-point (spyglass--mirror-of mirror-or-buffer)))

(defun spyglass-mirror-closed-reason (mirror-or-buffer)
  "Return why MIRROR-OR-BUFFER closed, or nil while it is not closed."
  (spyglass--mirror-reason (spyglass--mirror-of mirror-or-buffer)))

(defun spyglass-list-shares (connection callback)
  "Ask CONNECTION's peer for its shares; call CALLBACK with them.
CALLBACK gets a list of plists with `:id', `:name', `:mode', `:size'
and `:input', or nil when the connection ended first.  The peer also
announces its shares on its own: see `spyglass-connection-shares'."
  (spyglass--request connection "list"
                     (lambda (reply)
                       (funcall callback
                                (and reply (spyglass--decode-shares
                                            (plist-get reply :shares)))))))

(defun spyglass--decode-shares (vector)
  "Return the shares VECTOR describes, as a list of plists."
  (when (vectorp vector)
    (delq nil
          (mapcar (lambda (s)
                    (and (consp s) (stringp (plist-get s :id))
                         (list :id (plist-get s :id)
                               :name (plist-get s :name)
                               :mode (plist-get s :mode)
                               :size (plist-get s :size)
                               :input (eq (plist-get s :input) t))))
                  vector))))

;;;; Receiving

(defun spyglass--mirror-for (connection header)
  "Return the mirror HEADER names on CONNECTION, or nil."
  (gethash (plist-get header :sub) (spyglass-connection-mirrors connection)))

(defun spyglass--mirror-resolve-face (mirror face)
  "Return FACE, sanitized, with names unknown here replaced by attributes.
The attributes come from MIRROR's owner."
  (cond
   ((null face) nil)
   ((symbolp face)
    (if (facep face) face
      (gethash (symbol-name face) (spyglass--mirror-faces mirror))))
   ((keywordp (car face))
    (let* ((inherit (plist-get face :inherit))
           (names (if (listp inherit) inherit (list inherit)))
           (unknown (cl-remove-if #'facep names)))
      (if (null unknown)
          face
        (let ((known (cl-remove-if-not #'facep names))
              (plist (copy-sequence face)))
          (setq plist (if known
                          (plist-put plist :inherit (if (cdr known) known (car known)))
                        (cl-loop for (k v) on plist by #'cddr
                                 unless (eq k :inherit) nconc (list k v))))
          (let ((fallbacks (delq nil (mapcar (lambda (n)
                                               (gethash (symbol-name n)
                                                        (spyglass--mirror-faces mirror)))
                                             unknown))))
            (if fallbacks (cons plist fallbacks) plist))))))
   (t (delq nil (mapcar (lambda (f) (spyglass--mirror-resolve-face mirror f)) face)))))

(defun spyglass--mirror-resolve-strings (mirror value)
  "Return VALUE with the faces of strings in it resolved for MIRROR."
  (cond
   ((stringp value)
    (dolist (iv (object-intervals value))
      (let ((face (plist-get (caddr iv) 'face)))
        (when face
          (put-text-property (car iv) (cadr iv) 'face
                             (spyglass--mirror-resolve-face mirror face) value))))
    value)
   ((consp value)
    (if (proper-list-p value)
        (mapcar (lambda (v) (spyglass--mirror-resolve-strings mirror v)) value)
      value))
   ((vectorp value)
    (vconcat (mapcar (lambda (v) (spyglass--mirror-resolve-strings mirror v)) value)))
   (t value)))

(defun spyglass--mirror-style (mirror data)
  "Return the properties of the style DATA encodes, made safe for MIRROR."
  (let* ((raw (spyglass-wire-decode data))
         (clean (and (proper-list-p raw) (cl-evenp (length raw))
                     (spyglass-wire-sanitize-props raw spyglass-wire-style-properties)))
         out)
    (cl-loop for (prop value) on clean by #'cddr
             do (let ((value (if (eq prop 'face)
                                 (spyglass--mirror-resolve-face mirror value)
                               (spyglass--mirror-resolve-strings mirror value))))
                  (when value (setq out (cons value (cons prop out))))))
    (nreverse out)))

(defun spyglass--mirror-update (connection header payload)
  "Apply the update HEADER, with PAYLOAD, from CONNECTION's peer."
  (when-let* ((mirror (spyglass--mirror-for connection header)))
    (let ((seq (plist-get header :seq))
          (buffer (spyglass--mirror-buffer mirror)))
      (cond
       ((not (buffer-live-p buffer))
        (spyglass-close mirror))
       ((not (natnump seq))
        (spyglass--report connection "Update without a sequence number"))
       ((plist-get header :reset)
        (setf (spyglass--mirror-resync mirror) nil)
        (spyglass--mirror-reset mirror (plist-get header :epoch))
        (spyglass--mirror-apply-or-resync mirror header payload))
       ((spyglass--mirror-resync mirror)
        (spyglass--mirror-stat mirror :skipped))
       ((<= seq (spyglass--mirror-seq mirror))
        (spyglass--mirror-stat mirror :duplicates))
       ((/= seq (1+ (spyglass--mirror-seq mirror)))
        (spyglass--mirror-start-over mirror (format "update %d missing"
                                                (1+ (spyglass--mirror-seq mirror)))))
       (t (spyglass--mirror-apply-or-resync mirror header payload))))))

(defun spyglass--mirror-apply-or-resync (mirror header payload)
  "Apply update HEADER with PAYLOAD to MIRROR, or ask for a snapshot if it fails."
  (condition-case err
      (spyglass--mirror-apply mirror header payload)
    (error (spyglass--mirror-start-over mirror (error-message-string err)))))

(defun spyglass--mirror-start-over (mirror reason)
  "Ask MIRROR's owner for a snapshot, because of REASON.
Updates are ignored until the snapshot comes."
  (spyglass--report (spyglass--mirror-connection mirror) "%s: %s; starting over"
                    (spyglass--mirror-share mirror) reason)
  (spyglass--mirror-stat mirror :resyncs)
  (setf (spyglass--mirror-resync mirror) t)
  (spyglass--mirror-send-open mirror t))

(defun spyglass--mirror-reset (mirror epoch)
  "Empty MIRROR for a snapshot of subscription EPOCH."
  (with-current-buffer (spyglass--mirror-buffer mirror)
    (let ((inhibit-read-only t))
      (widen)
      (erase-buffer)))
  (maphash (lambda (_ ov) (delete-overlay ov)) (spyglass--mirror-overlays mirror))
  (clrhash (spyglass--mirror-overlays mirror))
  (clrhash (spyglass--mirror-styles mirror))
  (clrhash (spyglass--mirror-faces mirror))
  (setf (spyglass--mirror-epoch mirror) (and (stringp epoch) epoch)
        (spyglass--mirror-seq mirror) 0
        (spyglass--mirror-acked mirror) 0)
  (spyglass--mirror-stat mirror :snapshots))

(defun spyglass--mirror-apply (mirror header payload)
  "Apply the update HEADER with PAYLOAD to MIRROR."
  (let* ((start (float-time))
         (text (if payload
                   (spyglass-wire-decode-payload payload (plist-get header :sur))
                 ""))
         (styles (spyglass--mirror-styles mirror))
         (offset 0))
    ;; An owner resets styles past `spyglass-max-styles'; one that does
    ;; not may not make this table grow without bound.
    (when (or (plist-get header :sr)
              (> (hash-table-count styles) (* 2 spyglass-max-styles)))
      (clrhash styles))
    (seq-doseq (def (or (plist-get header :fd) []))
      (when (and (vectorp def) (= (length def) 2) (stringp (aref def 0)))
        (let ((attrs (spyglass-wire-sanitize-face (spyglass-wire-decode (aref def 1)))))
          (puthash (aref def 0) (and (consp attrs) attrs) (spyglass--mirror-faces mirror)))))
    (seq-doseq (def (or (plist-get header :sd) []))
      (when (and (vectorp def) (= (length def) 2) (natnump (aref def 0)))
        (puthash (aref def 0) (spyglass--mirror-style mirror (aref def 1)) styles)))
    (with-current-buffer (spyglass--mirror-buffer mirror)
      (let ((inhibit-read-only t))
        (save-restriction
          (widen)
          (save-excursion
            (seq-doseq (op (or (plist-get header :ops) []))
              (setq offset (spyglass--mirror-apply-op op text offset styles)))
            (spyglass--mirror-apply-overlays mirror header styles)))
        (when-let* ((meta (plist-get header :meta)))
          (spyglass--mirror-apply-meta mirror meta))
        (let ((pt (plist-get header :pt)) (ws (plist-get header :ws)))
          (when (natnump ws) (setf (spyglass--mirror-window-start mirror) ws))
          (when (natnump pt) (spyglass--mirror-apply-point mirror pt)))))
    (setf (spyglass--mirror-seq mirror) (plist-get header :seq))
    (unless (eq (spyglass--mirror-state mirror) 'live)
      (spyglass--mirror-set-state mirror 'live))
    (let ((now (float-time))
          (ct (plist-get header :ct))
          (offset (spyglass-connection-clock-offset (spyglass--mirror-connection mirror))))
      (spyglass--mirror-sample mirror :apply (- now start))
      (when (and (numberp ct) offset)
        (spyglass--mirror-sample mirror :latency (- now (- ct offset)))))
    (spyglass--mirror-stat mirror :updates)
    (spyglass--mirror-stat mirror :chars (length text))))

(defun spyglass--mirror-apply-op (op text offset styles)
  "Apply OP, a replacement, taking its text from TEXT at OFFSET.
STYLES maps style identifiers to properties.  Return the offset of
the next op's text.  Malformed ops signal `spyglass-wire-error'."
  (unless (and (vectorp op) (= (length op) 4))
    (signal 'spyglass-wire-error (list "Malformed op")))
  (let ((beg (aref op 0)) (old (aref op 1)) (len (aref op 2)) (runs (aref op 3)))
    (unless (and (natnump beg) (natnump old) (natnump len) (vectorp runs)
                 (>= beg (point-min)) (<= (+ beg old) (point-max))
                 (<= (+ offset len) (length text)))
      (signal 'spyglass-wire-error
              (list (format "Op %S out of range (size %d)" op (buffer-size)))))
    (let ((string (substring text offset (+ offset len)))
          (pos 0) (i 0) (n (length runs)))
      (while (< (1+ i) n)
        (let ((run (aref runs i)) (sid (aref runs (1+ i))))
          (when (and (natnump run) (<= (+ pos run) len))
            (unless (eq sid 0)
              (let ((props (gethash sid styles)))
                (when props (set-text-properties pos (+ pos run) props string))))
            (setq pos (+ pos run))))
        (setq i (+ i 2)))
      (goto-char beg)
      (delete-region beg (+ beg old))
      (insert string))
    (+ offset len)))

(defun spyglass--mirror-apply-overlays (mirror header styles)
  "Apply the overlays of update HEADER to MIRROR, styled from STYLES."
  (let ((table (spyglass--mirror-overlays mirror)))
    (seq-doseq (id (or (plist-get header :od) []))
      (when-let* ((ov (gethash id table)))
        (delete-overlay ov)
        (remhash id table)))
    (seq-doseq (entry (or (plist-get header :ov) []))
      (when (and (vectorp entry) (= (length entry) 4))
        (let ((id (aref entry 0)) (beg (aref entry 1)) (end (aref entry 2))
              (props (gethash (aref entry 3) styles)))
          (when (and (natnump beg) (natnump end))
            (let ((beg (max (point-min) (min beg (point-max))))
                  (end (max (point-min) (min end (point-max))))
                  (ov (gethash id table)))
              (if ov
                  (progn
                    (move-overlay ov beg end)
                    (cl-loop for (prop _) on (overlay-properties ov) by #'cddr
                             unless (eq prop 'spyglass-id)
                             do (overlay-put ov prop nil)))
                (setq ov (make-overlay beg end))
                (overlay-put ov 'spyglass-id id)
                (puthash id ov table))
              (cl-loop for (prop value) on props by #'cddr
                       do (overlay-put ov prop value)))))))))

(defun spyglass--mirror-apply-meta (mirror meta)
  "Apply META, the owner's description of the buffer, to MIRROR."
  (setf (spyglass--mirror-meta mirror) meta)
  (let ((tw (plist-get meta :tw)))
    (when (and (natnump tw) (< 0 tw 1000)) (setq tab-width tw)))
  (setq truncate-lines (eq (plist-get meta :tl) t)
        word-wrap (eq (plist-get meta :ww) t))
  (let ((inv (plist-get meta :inv)))
    (setq buffer-invisibility-spec
          (if (eq inv t) t
            (and (vectorp inv)
                 (delq nil (mapcar (lambda (e)
                                     (let ((v (spyglass-wire-decode e)))
                                       (cond ((symbolp v) v)
                                             ((and (consp v) (symbolp (car v)))
                                              (cons (car v) (and (cdr v) t))))))
                                   inv))))))
  (let ((hl (spyglass-wire-decode (plist-get meta :hl))))
    (setf (spyglass--mirror-header mirror)
          (and (stringp hl)
               (spyglass--mirror-resolve-strings mirror (spyglass-wire-sanitize-string hl)))))
  (if (eq (plist-get meta :fl) t)
      (unless (memq #'spyglass--mirror-jit (bound-and-true-p jit-lock-functions))
        (jit-lock-register #'spyglass--mirror-jit))
    (when (memq #'spyglass--mirror-jit (bound-and-true-p jit-lock-functions))
      (jit-lock-unregister #'spyglass--mirror-jit)))
  (let ((grid (plist-get meta :gs)))
    (setf (spyglass--mirror-grid mirror)
          (and (vectorp grid) (= (length grid) 2)
               (natnump (aref grid 0)) (natnump (aref grid 1))
               (cons (aref grid 0) (aref grid 1)))))
  (let ((sizable (eq (plist-get meta :rs) t)))
    (unless (eq sizable (spyglass--mirror-sizable mirror))
      (setf (spyglass--mirror-sizable mirror) sizable
            (spyglass--mirror-sent-size mirror) nil)
      (when sizable
        (spyglass--mirror-watch-windows)
        (spyglass--mirror-schedule-report))))
  (force-mode-line-update))

(defun spyglass--mirror-apply-point (mirror pt)
  "Show the owner's cursor of MIRROR at PT, and follow it in windows there."
  (let* ((old (spyglass--mirror-point mirror))
         (pt (max (point-min) (min pt (point-max)))))
    (setf (spyglass--mirror-point mirror) pt)
    (when spyglass-mirror-follow
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (when (or (null old) (eql (window-point window) old)
                  (= (window-point window) (point-max)))
          (set-window-point window pt)))
      (when (or (null old) (eql (point) old))
        (goto-char pt)))
    (when spyglass-mirror-show-cursor
      (let ((cursor (or (spyglass--mirror-cursor mirror)
                        (let ((ov (make-overlay 1 1)))
                          (overlay-put ov 'priority 1000)
                          (setf (spyglass--mirror-cursor mirror) ov)))))
        (if (< pt (point-max))
            (progn
              (move-overlay cursor pt (1+ pt))
              (overlay-put cursor 'after-string nil)
              (overlay-put cursor 'face 'spyglass-cursor))
          (move-overlay cursor pt pt)
          (overlay-put cursor 'face nil)
          (overlay-put cursor 'after-string (propertize " " 'face 'spyglass-cursor)))))))

(defun spyglass--mirror-jit (beg end)
  "Ask the owner to fontify BEG..END, which redisplay shows here."
  (let ((mirror spyglass--mirror))
    (when (and mirror (eq (spyglass--mirror-state mirror) 'live))
      (spyglass--send (spyglass--mirror-connection mirror)
                      (list :t "fl" :sub (spyglass--mirror-id mirror) :b beg :e end))))
  nil)

(defun spyglass--mirror-acknowledge (connection)
  "Acknowledge the updates CONNECTION's mirrors applied."
  (maphash (lambda (_ mirror)
             (when (> (spyglass--mirror-seq mirror) (spyglass--mirror-acked mirror))
               (setf (spyglass--mirror-acked mirror) (spyglass--mirror-seq mirror))
               (spyglass--send connection (list :t "ack" :sub (spyglass--mirror-id mirror)
                                                :seq (spyglass--mirror-seq mirror)))))
           (spyglass-connection-mirrors connection)))

(defun spyglass--mirror-closed (connection header _payload)
  "Close the mirror HEADER names on CONNECTION: its owner ended it."
  (when-let* ((mirror (spyglass--mirror-for connection header)))
    (let ((reason (plist-get header :reason)))
      (spyglass--mirror-finish mirror (if (stringp reason) reason "closed by owner")))))

(defun spyglass--mirror-resumed (connection header _payload)
  "Note that the mirror HEADER names on CONNECTION resumed."
  (when-let* ((mirror (spyglass--mirror-for connection header)))
    (spyglass--mirror-stat mirror :resumes)
    (spyglass--mirror-set-state mirror 'live)))

(defun spyglass--mirror-error (connection header _payload)
  "Report the error HEADER tells about a mirror of CONNECTION."
  (let ((mirror (spyglass--mirror-for connection header))
        (msg (format "%s" (plist-get header :msg))))
    (if mirror
        (progn
          (spyglass--mirror-event mirror 'error msg)
          (spyglass--report connection "%s: %s" (spyglass--mirror-share mirror) msg))
      (spyglass--report connection "%s" msg))))

(defun spyglass--mirror-shares (connection header _payload)
  "Remember the shares CONNECTION's peer announces in HEADER."
  (let ((shares (spyglass--decode-shares (plist-get header :list))))
    (setf (spyglass-connection-shares connection) shares)
    (condition-case err
        (run-hook-with-args 'spyglass-shares-changed-functions connection shares)
      (error (message "spyglass: error in shares hook: %S" err)))
    (spyglass--callback connection 'shares shares)))

(defun spyglass--mirror-attach (connection)
  "Reopen CONNECTION's mirrors on its new stream."
  (maphash (lambda (_ mirror)
             (unless (eq (spyglass--mirror-state mirror) 'closed)
               ;; The owner may have lost the size, or be another one.
               (setf (spyglass--mirror-sent-size mirror) nil)
               (spyglass--mirror-send-open mirror)))
           (spyglass-connection-mirrors connection)))

(defun spyglass--mirror-detach (connection final)
  "Mark CONNECTION's mirrors reconnecting, or closed if FINAL."
  (let (mirrors)
    (maphash (lambda (_ m) (push m mirrors)) (spyglass-connection-mirrors connection))
    (dolist (mirror mirrors)
      (if final
          (spyglass--mirror-finish
           mirror (pcase (spyglass-connection-closing connection)
                    ('peer "peer closed") ('local "disconnected") (_ "connection lost")))
        (spyglass--mirror-set-state mirror 'reconnecting)))))

(spyglass--register-handler "u" #'spyglass--mirror-update)
(spyglass--register-handler "closed" #'spyglass--mirror-closed)
(spyglass--register-handler "resumed" #'spyglass--mirror-resumed)
(spyglass--register-handler "err" #'spyglass--mirror-error)
(spyglass--register-handler "shares" #'spyglass--mirror-shares)
(add-hook 'spyglass--after-input-functions #'spyglass--mirror-acknowledge)
(add-hook 'spyglass--attach-functions #'spyglass--mirror-attach)
(add-hook 'spyglass--detach-functions #'spyglass--mirror-detach)

;;;; Sizes

(defcustom spyglass-mirror-size-delay 0.1
  "Seconds changes of windows gather before mirrors report their sizes."
  :group 'spyglass
  :type 'number)

(defvar spyglass--mirror-size-timer nil
  "Timer reporting the sizes of mirrors' windows, or nil.")

(defvar spyglass--mirror-focus nil
  "Buffer of the mirror that got focus and was not reported yet, or nil.")

(defvar spyglass--mirror-focused nil
  "Buffer of the mirror last reported as focused, or nil.")

(defvar spyglass--mirror-watching nil
  "Non-nil while the window hooks of sizable mirrors are installed.")

(defun spyglass--mirror-window (buffer)
  "Return the window whose size BUFFER, a mirror, reports, or nil.
The selected window when it shows BUFFER, else the most recently used
window showing it on a visible frame."
  (let ((windows (cl-remove-if-not (lambda (w) (eq (frame-visible-p (window-frame w)) t))
                                   (get-buffer-window-list buffer nil t))))
    (if (memq (selected-window) windows)
        (selected-window)
      (car (cl-sort windows #'> :key (lambda (w) (or (window-use-time w) 0)))))))

(defun spyglass--mirror-report-sizes ()
  "Report to owners the windows of sizable mirrors that changed or got focus."
  (setq spyglass--mirror-size-timer nil)
  (spyglass--guard nil
    (let ((focus (prog1 spyglass--mirror-focus (setq spyglass--mirror-focus nil)))
          (any nil))
      (dolist (connection (spyglass-connections))
        (maphash
         (lambda (_ mirror)
           (let ((buffer (spyglass--mirror-buffer mirror)))
             (when (and (spyglass--mirror-sizable mirror) (buffer-live-p buffer))
               (setq any t)
               (when (and (eq (spyglass--mirror-state mirror) 'live)
                          (spyglass-connection-live-p connection)
                          (spyglass-connection-capable-p connection "sz"))
                 (let* ((window (spyglass--mirror-window buffer))
                        (size (if window (spyglass-window-cells window) (cons 0 0)))
                        (focused (and window (eq buffer focus))))
                   (when (or focused
                             (not (equal size (spyglass--mirror-sent-size mirror))))
                     (setf (spyglass--mirror-sent-size mirror) size)
                     (spyglass--mirror-stat mirror :size-reports)
                     (spyglass--send connection
                                     (list :t "sz" :sub (spyglass--mirror-id mirror)
                                           :c (car size) :r (cdr size)
                                           :f (if focused t :false)))))))))
         (spyglass-connection-mirrors connection)))
      (unless any (spyglass--mirror-unwatch-windows)))))

(defun spyglass--mirror-schedule-report (&rest _)
  "Report mirrors' window sizes after `spyglass-mirror-size-delay'."
  (unless spyglass--mirror-size-timer
    (setq spyglass--mirror-size-timer
          (run-with-timer spyglass-mirror-size-delay nil #'spyglass--mirror-report-sizes))))

(defun spyglass--mirror-check-focus (&rest _)
  "Report a sizable mirror whose window just got the focus."
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (mirror (buffer-local-value 'spyglass--mirror buffer)))
    (if (and mirror (spyglass--mirror-sizable mirror)
             (frame-focus-state (window-frame window)))
        (unless (eq buffer spyglass--mirror-focused)
          (setq spyglass--mirror-focused buffer
                spyglass--mirror-focus buffer)
          (spyglass--mirror-schedule-report))
      (setq spyglass--mirror-focused nil))))

(defun spyglass--mirror-watch-windows ()
  "Report sizes and focus of mirrors' windows as they change."
  (unless spyglass--mirror-watching
    (setq spyglass--mirror-watching t)
    (add-hook 'window-size-change-functions #'spyglass--mirror-schedule-report)
    (add-hook 'window-buffer-change-functions #'spyglass--mirror-schedule-report)
    (add-hook 'window-selection-change-functions #'spyglass--mirror-check-focus)
    (add-function :after after-focus-change-function #'spyglass--mirror-check-focus)))

(defun spyglass--mirror-unwatch-windows ()
  "Stop watching windows: no sizable mirror is left."
  (when spyglass--mirror-watching
    (setq spyglass--mirror-watching nil)
    (remove-hook 'window-size-change-functions #'spyglass--mirror-schedule-report)
    (remove-hook 'window-buffer-change-functions #'spyglass--mirror-schedule-report)
    (remove-hook 'window-selection-change-functions #'spyglass--mirror-check-focus)
    (remove-function after-focus-change-function #'spyglass--mirror-check-focus)))

;;;; Input

(defun spyglass--mirror-input (mirror kind value)
  "Send MIRROR's owner input of KIND, `string' or `key', and VALUE."
  (let ((connection (spyglass--mirror-connection mirror))
        (meta (spyglass--mirror-meta mirror)))
    (cond
     ((not (spyglass-connection-live-p connection))
      (user-error "Mirror %s is not connected" (spyglass--mirror-share mirror)))
     ((eq (plist-get meta :in) :false)
      (user-error "Share %s is read-only" (spyglass--mirror-share mirror)))
     (t (spyglass--send connection (list :t "in" :sub (spyglass--mirror-id mirror)
                                         :k (if (eq kind 'key) "k" "s") :v value))))))

(defun spyglass-mirror-send-string (string &optional mirror-or-buffer)
  "Send STRING as typed text to the owner of MIRROR-OR-BUFFER.
MIRROR-OR-BUFFER defaults to the current buffer."
  (spyglass--mirror-input (spyglass--mirror-of (or mirror-or-buffer (current-buffer)))
                          'string string))

(defun spyglass-mirror-send-key (key &optional mirror-or-buffer)
  "Send KEY, in `key-description' form, to the owner of MIRROR-OR-BUFFER.
MIRROR-OR-BUFFER defaults to the current buffer."
  (unless (key-valid-p key) (error "Invalid key: %S" key))
  (spyglass--mirror-input (spyglass--mirror-of (or mirror-or-buffer (current-buffer)))
                          'key key))

(defun spyglass-mirror-self-insert (n)
  "Send the character typed, N times, to the owner."
  (interactive "p" spyglass-mirror-mode)
  (spyglass-mirror-send-string (make-string n last-command-event)))

(defun spyglass-mirror-forward-key ()
  "Send the key typed to the owner."
  (interactive nil spyglass-mirror-mode)
  (spyglass-mirror-send-key (key-description (this-command-keys-vector))))

(defun spyglass-mirror-send-C-c ()
  "Send the owner control-C, the interrupt character."
  (interactive nil spyglass-mirror-mode)
  (spyglass-mirror-send-key "C-c"))

(defun spyglass-mirror-send-C-d ()
  "Send the owner control-D, the end of input."
  (interactive nil spyglass-mirror-mode)
  (spyglass-mirror-send-key "C-d"))

(defun spyglass-mirror-send-C-z ()
  "Send the owner control-Z, which suspends a job."
  (interactive nil spyglass-mirror-mode)
  (spyglass-mirror-send-key "C-z"))

(defun spyglass-mirror-send-C-j ()
  "Send the owner control-J, a line feed."
  (interactive nil spyglass-mirror-mode)
  (spyglass-mirror-send-key "C-j"))

(defun spyglass-mirror-send-escape ()
  "Send ESC to the owner."
  (interactive nil spyglass-mirror-mode)
  (spyglass-mirror-send-key "ESC"))

(defun spyglass-mirror-send-next-key ()
  "Read a key and send it to the owner."
  (interactive nil spyglass-mirror-mode)
  (spyglass-mirror-send-key (key-description (vector (read-event "Key to send: ")))))

(defun spyglass-mirror-send-string-command (string)
  "Read STRING and send it to the owner as typed text."
  (interactive (list (read-string "Send: ")) spyglass-mirror-mode)
  (spyglass-mirror-send-string string))

(defun spyglass-mirror-toggle-follow ()
  "Toggle whether this mirror's windows follow the owner's cursor."
  (interactive nil spyglass-mirror-mode)
  (setq-local spyglass-mirror-follow (not spyglass-mirror-follow))
  (when (and spyglass-mirror-follow spyglass--mirror (spyglass--mirror-point spyglass--mirror))
    (goto-char (spyglass--mirror-point spyglass--mirror)))
  (message "Following the owner's cursor %s" (if spyglass-mirror-follow "on" "off")))

(defun spyglass-mirror-close-command ()
  "Close this mirror, keeping its buffer."
  (interactive nil spyglass-mirror-mode)
  (spyglass-close (current-buffer)))

(provide 'spyglass-mirror)
;;; spyglass-mirror.el ends here
