;;; spyglass-window.el --- Park shared buffers and size shared terminals  -*- lexical-binding: t; -*-

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

;; The owner's side of what windows do to shared buffers.
;;
;; Parking.  Some buffers change only while a window shows them:
;; Ghostel renders its terminal through a window, so a Ghostel terminal
;; hidden on the owner stands still, and its mirrors with it.  While a
;; share needing a window (`spyglass-needs-window', or `:needs-window'
;; of `spyglass-share') has viewers and no window shows its buffer,
;; spyglass shows it in the window of a parking frame: an invisible
;; frame on a graphical display, which is never made visible, never
;; selected but for an instant, as measuring a window does, never given
;; focus, and left out of `next-frame', desktops, tab bars and the
;; hooks run for new frames.  The parking frame is deleted as soon as
;; another window shows the buffer, the buffer is unshared or killed,
;; or its last viewer leaves.  Each parked buffer has a frame of its
;; own rather than a window in a frame shared by all: a parked terminal
;; takes the size its viewers' windows ask for, and windows of one
;; frame share their widths or their heights, so that sizing one would
;; resize the others and the frame would grow to hold them all.
;;
;; Sizing.  A terminal has one grid, laid out by the programs in it for
;; its size.  Viewers report the size in cells of the window showing
;; their mirror ("sz"), when it opens and whenever it changes, and say
;; when it gets focus; typing in a mirror counts as activity too.  The
;; terminal takes the size `spyglass-size-policy' picks among the
;; viewers' windows and the owner's.  Terminals compute their size from
;; `window-adjust-process-window-size-function', Eat and vterm through
;; their processes' `adjust-window-size-function', Ghostel directly:
;; spyglass wraps its default value to answer for shared terminals, and
;; asks a terminal to fit its size again through
;; `spyglass-refit-function'.  A parked terminal is sized by sizing its
;; parking window.  The owner's own windows are never resized: when a
;; viewer's size wins, the owner's windows show the terminal at that
;; size, until the owner uses it again.

;;; Code:

(require 'cl-lib)
(require 'spyglass-wire)
(require 'spyglass-conn)
(require 'spyglass-owner)

;;;; Options and buffer-local settings

(defcustom spyglass-size-policy 'latest
  "Whose window sizes a shared terminal that several parties show.
The parties are the owner, when a window of its shows the terminal,
and the viewers whose windows show their mirrors.  `latest': the
party that last typed into the terminal or selected it, the owner
winning ties; `smallest': the smallest width and the smallest height
among all; `owner': the owner's windows while one shows the terminal,
else as `latest'."
  :group 'spyglass
  :type '(choice (const :tag "Most recently active" latest)
                 (const :tag "Smallest" smallest)
                 (const :tag "Owner's window" owner)))

(defcustom spyglass-max-terminal-size '(500 . 200)
  "Largest size, (COLUMNS . ROWS), a viewer may give a terminal.
Larger reports are reduced to it: a parked terminal's window has that
many cells, and glyph matrices with them."
  :group 'spyglass
  :type '(cons natnum natnum))

(defvar-local spyglass-needs-window nil
  "Non-nil when this buffer changes only while a window shows it.
While it is shared, has viewers and no window shows it, spyglass shows
it in a hidden window.  Ghostel's adapter sets it.")

(defvar-local spyglass-park-function nil
  "Function called with no argument in this buffer once it is parked, or nil.
Ghostel's adapter renders there what arrived while it was hidden.")

(defvar-local spyglass-refit-function nil
  "Function making this buffer's terminal take its size again, or nil.
Called with no argument in the buffer when the size its parties call
for may have changed; the terminal finds that size through
`window-adjust-process-window-size-function', which spyglass wraps.
Non-nil makes the share sizable by viewers.  Adapters set it;
`spyglass-refit-process' suits terminals sizing their process through
its `adjust-window-size-function', as Eat and vterm do.")

(defvar-local spyglass-terminal-size-function nil
  "Function returning this buffer's terminal size, (COLUMNS . ROWS), or nil.
Spyglass compares it with the size called for, and tells viewers.")

;;;; Parking frames

(defconst spyglass--parking-parameters
  '((visibility . nil)
    (minibuffer . nil)
    (spyglass-parking . t)
    (no-other-frame . t)
    (desktop-dont-save . t)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (skip-taskbar . t)
    (unsplittable . t)
    (auto-hide-function . spyglass--park-auto-hide)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . t)
    (tool-bar-lines . 0)
    (menu-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (fullscreen . nil)
    (left . 0)
    (top . 0)
    (user-position . t)
    (name . "spyglass parking"))
  "Frame parameters of parking frames.")

(defvar spyglass--park-retry 1.0
  "Seconds before parking a buffer again after its parking frame vanished.")

(defvar spyglass--park-failures (make-hash-table :test #'eq :weakness 'key)
  "Shares whose parking frame vanished, and when.")

(defun spyglass-parking-frame-p (frame)
  "Return non-nil when FRAME is a parking frame of spyglass."
  (and (frame-live-p frame) (frame-parameter frame 'spyglass-parking) t))

(defun spyglass--parking-window-p (window)
  "Return non-nil when WINDOW is in a parking frame."
  (spyglass-parking-frame-p (window-frame window)))

(defun spyglass--owner-windows (buffer)
  "Return the windows showing BUFFER but parking windows.
A daemon's initial frame, which no one sees, does not count."
  (cl-remove-if (lambda (w)
                  (let ((frame (window-frame w)))
                    (or (spyglass-parking-frame-p frame)
                        (and (daemonp) (eq frame terminal-frame)))))
                (get-buffer-window-list buffer nil t)))

(defun spyglass--needs-window-p (share)
  "Return non-nil when SHARE's buffer changes only while a window shows it."
  (let ((value (spyglass-share-needs-window share)))
    (if (eq value 'auto)
        (buffer-local-value 'spyglass-needs-window (spyglass-share-buffer share))
      value)))

(defun spyglass--park-host ()
  "Return a frame of the graphical display parking frames go on, or nil.
The selected frame when graphical, else another graphical frame."
  (cl-find-if (lambda (f)
                (and (display-graphic-p f)
                     (not (spyglass-parking-frame-p f))
                     (frame-visible-p f)))
              (cons (selected-frame) (frame-list))))

(defun spyglass--park-auto-hide (frame)
  "Delete FRAME, a parking frame whose buffer is going away."
  (when (frame-live-p frame)
    (spyglass--delete-parking-frame frame)))

(defun spyglass--delete-parking-frame (frame)
  "Delete FRAME, a parking frame, without the hooks of frames people saw."
  ;; Hooks run for deleted frames would remember it for
  ;; `undelete-frame' or save it; it was never shown.
  (let ((delete-frame-functions nil)
        (after-delete-frame-functions nil))
    (delete-frame frame t)))

(defun spyglass--make-parking-frame (buffer host)
  "Return a new parking frame showing BUFFER, on the display of HOST."
  ;; Hooks for new frames are meant for frames people see: Cocoa's
  ;; selects every new frame, and configurations raise Emacs there.
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (frame (with-current-buffer buffer
                  (make-frame (append `((terminal . ,(frame-terminal host))
                                        (font . ,(frame-parameter host 'font)))
                                      spyglass--parking-parameters))))
         (window (frame-root-window frame)))
    ;; The minibuffer's frame cannot be deleted while it serves
    ;; another: deleting it deletes this one first.
    (modify-frame-parameters frame `((delete-before . ,(window-frame
                                                         (minibuffer-window frame)))))
    (unless (eq (window-buffer window) buffer)
      (set-window-buffer window buffer))
    (set-window-dedicated-p window t)
    (set-window-parameter window 'no-other-window t)
    (set-window-parameter window 'no-delete-other-windows t)
    frame))

(defun spyglass--park (share)
  "Show SHARE's buffer in a parking frame, if a graphical display is there."
  (when-let* ((host (spyglass--park-host)))
    (let* ((buffer (spyglass-share-buffer share))
           (frame (spyglass--make-parking-frame buffer host)))
      (setf (spyglass-share-parked share) frame
            (spyglass-share-park-size share)
            (or (spyglass-share-target share)
                (spyglass--terminal-size share)
                (spyglass-window-cells (frame-root-window frame))))
      (spyglass--share-stat share :parks)
      (spyglass--size-parking share)
      (with-current-buffer buffer
        (when spyglass-park-function
          (funcall spyglass-park-function))
        (spyglass--refit share)))))

(defun spyglass--unpark (share)
  "Delete SHARE's parking frame, if it has one."
  (let ((frame (spyglass-share-parked share)))
    (setf (spyglass-share-parked share) nil)
    (when (frame-live-p frame)
      (spyglass--delete-parking-frame frame))))

(defun spyglass--parking-window (share)
  "Return the window SHARE's buffer is parked in, or nil."
  (let ((frame (spyglass-share-parked share)))
    (and (frame-live-p frame) (frame-root-window frame))))

(defun spyglass--size-parking (share)
  "Give SHARE's parking window the size SHARE calls for.
Return non-nil when it was resized."
  (when-let* ((window (spyglass--parking-window share)))
    (let* ((size (or (spyglass--choose-size share nil)
                     (spyglass-share-park-size share)))
           (frame (window-frame window))
           (body-w (window-body-width window t))
           (body-h (window-body-height window t))
           ;; What the window's cells depend on.
           (key (list size body-w body-h (frame-char-width frame) (frame-char-height frame)
                      (buffer-local-value 'face-remapping-alist (window-buffer window)))))
      (setf (spyglass-share-park-size share) size)
      ;; Measuring cells selects the window and asks the font: done
      ;; only when the size called for or the window may have changed.
      (unless (equal key (window-parameter window 'spyglass-checked))
        (let ((cells (spyglass-window-cells window)))
          (if (equal cells size)
              (progn (set-window-parameter window 'spyglass-checked key) nil)
            (let* ((metrics (with-selected-window window
                              (cons (default-font-width) (default-line-height))))
                   (fw (car metrics)) (lh (cdr metrics))
                   ;; Columns the window loses to a continuation glyph
                   ;; or line numbers, at any width.
                   (lost (- (/ body-w fw) (car cells)))
                   ;; Half a cell more, that rounding never loses one.
                   (want-w (+ (* (+ (car size) lost) fw) (/ fw 2)))
                   (want-h (+ (* (cdr size) lh) (/ lh 2)))
                   (request (list (+ want-w (- (frame-text-width frame) body-w))
                                  (+ want-h (- (frame-text-height frame) body-h)))))
              ;; A window system may apply sizes later, or refuse some:
              ;; ask once for each.
              (unless (equal request (frame-parameter frame 'spyglass-request))
                (set-frame-parameter frame 'spyglass-request request)
                (set-frame-size frame (car request) (cadr request) t)
                t))))))))

(defun spyglass--check-parking (share)
  "Park or unpark SHARE's buffer as its windows and viewers call for."
  (let* ((buffer (spyglass-share-buffer share))
         (frame (spyglass-share-parked share))
         (want (and (buffer-live-p buffer)
                    (spyglass-share-subs share)
                    (spyglass--needs-window-p share)
                    (null (spyglass--owner-windows buffer)))))
    (cond
     ;; Something showed the frame, as `display-buffer' may when told to
     ;; reuse windows of invisible frames: the frame is the user's now.
     ;; It still goes with its minibuffer's frame, whose deletion it
     ;; would otherwise refuse.
     ((and frame (frame-live-p frame) (frame-visible-p frame))
      (setf (spyglass-share-parked share) nil)
      (modify-frame-parameters frame '((spyglass-parking . nil) (no-other-frame . nil)
                                       (desktop-dont-save . nil) (unsplittable . nil)
                                       (auto-hide-function . nil)))
      (let ((window (frame-root-window frame)))
        (set-window-dedicated-p window nil)
        (set-window-parameter window 'no-other-window nil)
        (set-window-parameter window 'no-delete-other-windows nil)))
     ((and frame (or (not (frame-live-p frame))
                     (not (eq (window-buffer (frame-root-window frame)) buffer))))
      (spyglass--unpark share)
      (puthash share (float-time) spyglass--park-failures))
     ((and frame (not want))
      (spyglass--unpark share)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (spyglass--refit share))))
     ((and want (not frame)
           (> (- (float-time) (gethash share spyglass--park-failures 0))
              spyglass--park-retry))
      (spyglass--park share))
     (frame
      (when (spyglass--size-parking share)
        (with-current-buffer buffer (spyglass--refit share)))))))

;;;; Sizes

(defun spyglass--terminal-size (share)
  "Return the size of SHARE's terminal, (COLUMNS . ROWS), or nil if unknown."
  (with-current-buffer (spyglass-share-buffer share)
    (let ((size (and spyglass-terminal-size-function
                     (ignore-errors (funcall spyglass-terminal-size-function)))))
      (if (and (consp size) (natnump (car size)) (natnump (cdr size)))
          size
        (spyglass-share-grid share)))))

(defun spyglass--sizable-p (share)
  "Return non-nil when viewers may size SHARE's terminal."
  (and (buffer-live-p (spyglass-share-buffer share))
       (buffer-local-value 'spyglass-refit-function (spyglass-share-buffer share))
       t))

(defun spyglass--viewer-sizes (share)
  "Return the sizes SHARE's connected viewers show it at, as (SIZE . TIME).
TIME is when the viewer last typed into its mirror or selected it."
  (cl-loop for sub in (spyglass-share-subs share)
           when (and (spyglass--sub-size sub) (spyglass--sub-live-p sub))
           collect (cons (spyglass--sub-size sub) (spyglass--sub-active sub))))

(defun spyglass--latest (parties)
  "Return the size of the latest of PARTIES, (SIZE . TIME), the first on ties."
  (let ((best (car parties)))
    (dolist (p (cdr parties))
      (when (> (cdr p) (cdr best)) (setq best p)))
    (car best)))

(defun spyglass--choose-size (share owner-size)
  "Return the size SHARE's terminal should take, or nil for its own choice.
OWNER-SIZE is the size the owner's windows showing the terminal give,
or nil when none shows it.  Nil unless a viewer's window shows it."
  (let ((viewers (spyglass--viewer-sizes share)))
    (when viewers
      (let ((owner (and owner-size
                        (cons owner-size (spyglass-share-owner-active share)))))
        (pcase spyglass-size-policy
          ('smallest
           (let ((all (mapcar #'car (if owner (cons owner viewers) viewers))))
             (cons (apply #'min (mapcar #'car all)) (apply #'min (mapcar #'cdr all)))))
          ('owner (if owner (car owner) (spyglass--latest viewers)))
          (_ (spyglass--latest (if owner (cons owner viewers) viewers))))))))

(defun spyglass--process-window-size (orig process windows)
  "Return the size PROCESS's terminal should take for WINDOWS.
Wraps ORIG, the default `window-adjust-process-window-size-function':
for a process of a shared terminal its viewers size, the size
`spyglass-size-policy' picks, ORIG's own answer otherwise."
  (let* ((buffer (if (processp process) (process-buffer process) (current-buffer)))
         (share (and (buffer-live-p buffer) (buffer-local-value 'spyglass--share buffer))))
    (if (not (and share (spyglass-share-mode share) (spyglass--sizable-p share)))
        (funcall orig process windows)
      (let* ((own (cl-remove-if #'spyglass--parking-window-p windows))
             (target (spyglass--choose-size share (and own (funcall orig process own))))
             (size (or target (funcall orig process windows))))
        (setf (spyglass-share-target share) target)
        (when (consp size)
          (unless (equal size (spyglass-share-grid share))
            ;; Viewers learn the new size with the next description.
            (setf (spyglass-share-grid share) size
                  (spyglass-share-meta-time share) 0.0)))
        size))))

(defun spyglass-refit-process ()
  "Make the current buffer's process take the size its windows and viewers give.
As `window--adjust-process-windows' does on a change of windows, for
this buffer's process alone, windows or not.  The size is set again a
second later: a program starting in the terminal may set its pty's size
to the terminal's first one, as Eat and vterm run `stty' first."
  (let ((process (get-buffer-process (current-buffer)))
        (windows (get-buffer-window-list (current-buffer) nil t)))
    ;; Terminals' functions expect a size for windows: without any,
    ;; only a viewer's size can be had.
    (when (and (process-live-p process)
               (or windows (and spyglass--share (spyglass--choose-size spyglass--share nil))))
      (let ((size (funcall (or (process-get process 'adjust-window-size-function)
                               window-adjust-process-window-size-function)
                           process windows)))
        (when (and (consp size) (natnump (car size)) (natnump (cdr size)))
          (set-process-window-size process (cdr size) (car size))
          (let ((timer (process-get process 'spyglass-reassert)))
            (when (timerp timer) (cancel-timer timer)))
          (process-put process 'spyglass-reassert
                       (run-with-timer 1 nil #'spyglass--reassert-size process size
                                       (and spyglass--share
                                            (spyglass-share-grid spyglass--share)))))))))

(defun spyglass--reassert-size (process size grid)
  "Set PROCESS's pty to SIZE again, if its terminal's size is still GRID."
  (when (process-live-p process)
    (process-put process 'spyglass-reassert nil)
    (let* ((buffer (process-buffer process))
           (share (and (buffer-live-p buffer) (buffer-local-value 'spyglass--share buffer))))
      (when (and share (equal (spyglass-share-grid share) grid))
        (set-process-window-size process (cdr size) (car size))))))

(defun spyglass--refit (share)
  "Have SHARE's terminal take the size its parties call for, now."
  (when (and (spyglass--sizable-p share) (spyglass-share-mode share))
    (with-current-buffer (spyglass-share-buffer share)
      (spyglass--size-parking share)
      (spyglass--guard nil
        (funcall spyglass-refit-function))
      (spyglass--schedule-flush))))

(defun spyglass--latest-is (share sub)
  "Return non-nil when SUB, or the owner if nil, is SHARE's latest active party."
  (let ((time (if sub (spyglass--sub-active sub) (spyglass-share-owner-active share))))
    (and (>= time (spyglass-share-owner-active share))
         (cl-every (lambda (other)
                     (or (eq other sub) (not (spyglass--sub-size other))
                         (>= time (spyglass--sub-active other))))
                   (spyglass-share-subs share)))))

(defun spyglass--activity (share &optional sub)
  "Note that SUB, or the owner if nil, used SHARE's terminal just now."
  (let ((was (spyglass--latest-is share sub))
        (now (float-time)))
    (if sub
        (setf (spyglass--sub-active sub) now)
      (setf (spyglass-share-owner-active share) now))
    (unless was
      (spyglass--refit share))))

;;;; The owner's activity

(defvar spyglass--sizing nil
  "Non-nil while some viewer sizes a terminal: the hooks below are installed.")

(defun spyglass--owner-command ()
  "Note the owner's activity in the selected window's shared terminal."
  (spyglass--guard nil
    (let* ((window (selected-window))
           (share (buffer-local-value 'spyglass--share (window-buffer window))))
      (when (and share (spyglass-share-mode share) (not (spyglass--parking-window-p window))
                 (spyglass--viewer-sizes share))
        (spyglass--activity share)))))

(defun spyglass--owner-focus ()
  "Note the owner's activity when a frame showing a shared terminal gets focus."
  (spyglass--guard nil
    (dolist (frame (frame-list))
      (when (and (eq (frame-focus-state frame) t) (not (spyglass-parking-frame-p frame)))
        (let ((share (buffer-local-value 'spyglass--share
                                         (window-buffer (frame-selected-window frame)))))
          (when (and share (spyglass-share-mode share) (spyglass--viewer-sizes share))
            (spyglass--activity share)))))))

(defun spyglass--start-sizing ()
  "Install the wrapper of terminal sizes and the hooks of the owner's activity."
  (let ((default (default-value 'window-adjust-process-window-size-function)))
    ;; Customizing the variable drops the wrapper: put it back.
    (unless (advice-function-member-p #'spyglass--process-window-size default)
      (add-function :around (default-value 'window-adjust-process-window-size-function)
                    #'spyglass--process-window-size)))
  (unless spyglass--sizing
    (setq spyglass--sizing t)
    (add-hook 'post-command-hook #'spyglass--owner-command)
    (add-function :after after-focus-change-function #'spyglass--owner-focus)))

(defun spyglass--stop-sizing ()
  "Remove what `spyglass--start-sizing' installed once no viewer sizes anything."
  (when (and spyglass--sizing
             (not (cl-some (lambda (share)
                             (cl-some #'spyglass--sub-size (spyglass-share-subs share)))
                           (spyglass-shares))))
    (setq spyglass--sizing nil)
    (remove-hook 'post-command-hook #'spyglass--owner-command)
    (remove-function after-focus-change-function #'spyglass--owner-focus)
    (remove-function (default-value 'window-adjust-process-window-size-function)
                     #'spyglass--process-window-size)))

;;;; Events

(defun spyglass--owner-size-report (connection header _payload)
  "Note the size of the window showing a mirror, which CONNECTION's peer sends.
HEADER has the subscription, columns `c' and rows `r', 0 when no
window shows the mirror, and `f' true when it just got focus."
  (when-let* ((sub (spyglass--sub-of connection header)))
    (let ((cols (plist-get header :c)) (rows (plist-get header :r))
          (share (spyglass--sub-share sub)))
      (setf (spyglass--sub-size sub)
            (and (natnump cols) (natnump rows) (> cols 0) (> rows 0)
                 (cons (min cols (car spyglass-max-terminal-size))
                       (min rows (cdr spyglass-max-terminal-size)))))
      (spyglass--share-stat share :size-reports)
      (when (eq (plist-get header :f) t)
        (setf (spyglass--sub-active sub) (float-time)))
      (if (spyglass--sub-size sub)
          (spyglass--start-sizing)
        (spyglass--stop-sizing))
      (when (buffer-live-p (spyglass-share-buffer share))
        (spyglass--refit share)))))

(defun spyglass--window-sub-event (sub event)
  "Size and park SUB's share as EVENT, an event of SUB, calls for."
  (let ((share (spyglass--sub-share sub)))
    (when (buffer-live-p (spyglass-share-buffer share))
      (pcase event
        ('opened (setf (spyglass--sub-active sub) (float-time))
                 (spyglass--check-parking share))
        ('input (spyglass--activity share sub))
        ((or 'orphaned 'resumed) (spyglass--refit share))
        ('freed (spyglass--check-parking share)
                (spyglass--stop-sizing)
                (spyglass--refit share))))
    (when (and (eq event 'freed) (not (buffer-live-p (spyglass-share-buffer share))))
      (spyglass--unpark share))))

(defun spyglass--window-share-event (share event _data)
  "Unpark SHARE when EVENT says it is no longer shared."
  (when (eq event 'unshared)
    (spyglass--unpark share)
    (spyglass--stop-sizing)))

(defun spyglass--window-poll (share)
  "Park, unpark or resize SHARE's parking window as needed."
  (spyglass--check-parking share))

(defun spyglass--window-meta (share)
  "Return what viewers learn of SHARE's size: whether they size it, and its size."
  (when (spyglass--sizable-p share)
    (let ((size (spyglass--terminal-size share)))
      (append (list :rs t)
              (and size (list :gs (vector (car size) (cdr size))))))))

(spyglass--register-handler "sz" #'spyglass--owner-size-report)
(add-hook 'spyglass--sub-functions #'spyglass--window-sub-event)
(add-hook 'spyglass-share-state-functions #'spyglass--window-share-event)
(add-hook 'spyglass--poll-functions #'spyglass--window-poll)
(add-hook 'spyglass--meta-functions #'spyglass--window-meta)

(defun spyglass-window-unload-function ()
  "Unpark every buffer and remove the hooks of spyglass-window."
  (dolist (share (spyglass-shares)) (spyglass--unpark share))
  (setq spyglass--sizing nil)
  (remhash "sz" spyglass--handlers)
  (remove-hook 'post-command-hook #'spyglass--owner-command)
  (remove-function after-focus-change-function #'spyglass--owner-focus)
  (remove-function (default-value 'window-adjust-process-window-size-function)
                   #'spyglass--process-window-size)
  (remove-hook 'spyglass--sub-functions #'spyglass--window-sub-event)
  (remove-hook 'spyglass-share-state-functions #'spyglass--window-share-event)
  (remove-hook 'spyglass--poll-functions #'spyglass--window-poll)
  (remove-hook 'spyglass--meta-functions #'spyglass--window-meta)
  nil)

(provide 'spyglass-window)
;;; spyglass-window.el ends here
