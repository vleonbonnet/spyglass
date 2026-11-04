;;; gui-emacs.el --- Keep a graphical test Emacs out of the user's way  -*- lexical-binding: t; -*-

;;; Commentary:

;; test/gui-emacs.sh loads this first into the graphical Emacs it
;; starts, from a copy in the directory of the run.  It writes this
;; Emacs's process ID there, and the status `kill-emacs' is given: on
;; macOS, `open' returns neither.  It keeps every frame out of sight and
;; of the focus: off screen, fully transparent, refusing focus.  Frames
;; stay visible to Emacs, as tests and benchmarks need: redisplay draws
;; only visible frames, and a mirror sizes only by their windows.
;;
;; On macOS the application starts hidden; the frame Emacs made before
;; this file loaded shows once concealed, as the application unhides
;; without being activated.  Nothing here activates Emacs, nor raises a
;; frame where it could be seen.

;;; Code:

(defconst gui-emacs-directory (file-name-directory load-file-name)
  "The directory of this run.")

(defconst gui-emacs-concealed
  '((left . (+ -10000)) (top . (+ -10000)) (alpha . 0)
    (no-accept-focus . t) (no-focus-on-map . t))
  "Frame parameters keeping a frame out of sight and of the focus.")

;; Native compilation would write to the user's cache, and so would
;; the trampolines advising primitives compiles.
(setq native-comp-jit-compilation nil
      native-comp-enable-subr-trampolines nil)

(write-region (number-to-string (emacs-pid)) nil
              (expand-file-name "pid" gui-emacs-directory) nil 'silent)

(defun gui-emacs--status (&optional arg &rest _)
  "Write the status `kill-emacs' exits with, given ARG."
  (write-region (number-to-string (if (integerp arg) arg 0)) nil
                (expand-file-name "status" gui-emacs-directory) nil 'silent))

(advice-add 'kill-emacs :before #'gui-emacs--status)

;; Below it, a frame's alpha is raised to it.
(setq frame-alpha-lower-limit 0)

(defun gui-emacs--conceal (frame)
  "Keep FRAME out of sight and of the focus, and return it."
  (when (display-graphic-p frame)
    (modify-frame-parameters frame gui-emacs-concealed))
  frame)

(defun gui-emacs--concealed-parameters (parameters)
  "Return PARAMETERS, a list of the frame parameters, concealed."
  (list (append gui-emacs-concealed (car parameters))))

;; The Mac port applies alpha only to a frame made: once more after.
(advice-add 'x-create-frame-with-faces :filter-args #'gui-emacs--concealed-parameters)
(advice-add 'x-create-frame-with-faces :filter-return #'gui-emacs--conceal)

(defun gui-emacs--select-frame (frame &optional norecord)
  "Select FRAME, NORECORD as for `select-frame', and focus it without activating Emacs."
  (select-frame frame norecord)
  (raise-frame frame)
  (when (display-graphic-p frame)
    (x-focus-frame frame t)))

;; Focusing a frame activates the application, in front of the user's.
(advice-add 'select-frame-set-input-focus :override #'gui-emacs--select-frame)
(advice-add 'x-focus-frame :filter-args (lambda (args) (list (car args) t)))

(gui-emacs--conceal (selected-frame))

(defun gui-emacs--unhide ()
  "Show this application's frames, without activating it, if still hidden."
  (unless (frame-visible-p (selected-frame))
    (call-process "osascript" nil nil nil "-e"
                  (format "tell application \"System Events\" to set visible of (first process whose unix id is %d) to true"
                          (emacs-pid)))))

(when (memq window-system '(mac ns))
  (run-with-timer 1 nil #'gui-emacs--unhide))

;;; gui-emacs.el ends here
