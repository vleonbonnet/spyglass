;;; spyglass-test-helpers.el --- Helpers for spyglass's tests  -*- lexical-binding: t; -*-

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

;; Both ends of a connection in one batch Emacs: a server on 127.0.0.1
;; accepts the connection a transport makes to it, so that this Emacs
;; shares buffers with itself.  `spyglass-test-runs' describes the text
;; and mirrored properties of an owner's buffer or a mirror the same
;; way, for comparing them; the benchmarks use it too.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'spyglass)

(defvar spyglass-test-errors nil
  "Errors reported during the current test, newest first.")

(defun spyglass-test--log-error (_connection message)
  "Log MESSAGE, an error spyglass reported."
  (push message spyglass-test-errors)
  (message "spyglass error: %s" message))

(add-hook 'spyglass-error-functions #'spyglass-test--log-error)

(defun spyglass-test-step ()
  "Process events for at most 10 ms, or until the next timer is due.
`accept-process-output' does not wake for a timer armed while it
waits, so a batch Emacs waiting longer delays its timers."
  (let* ((timer (car timer-list))
         (until (and timer (timer-until timer nil))))
    (accept-process-output nil (max 0.001 (min 0.01 (or until 0.01))))))

(defun spyglass-test-wait (predicate &optional timeout)
  "Process events until PREDICATE returns non-nil or TIMEOUT seconds pass.
Return PREDICATE's last value.  TIMEOUT defaults to 10 seconds."
  (let ((end (+ (float-time) (or timeout 10))) value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) end))
      (spyglass-test-step))
    value))

(defun spyglass-test-settle (&optional seconds)
  "Process events for SECONDS (0.2 by default)."
  (let ((end (+ (float-time) (or seconds 0.2))))
    (while (< (float-time) end)
      (spyglass-test-step))))

(defconst spyglass-test-lag-period 0.005
  "Seconds between the ticks of the timer `spyglass-test-call-with-lag' runs.")

(defun spyglass-test-call-with-lag (function)
  "Call FUNCTION and return its value and how long Emacs ran no timer meanwhile.
The value is (VALUE . SECONDS): SECONDS is the longest a timer due
every `spyglass-test-lag-period' waited past its time, while FUNCTION
waits with `spyglass-test-wait', say.  A step of Emacs's taking long,
as a timer, sentinel or filter not returning, makes it wait."
  (let* ((last (float-time))
         (worst 0.0)
         (timer (run-with-timer spyglass-test-lag-period spyglass-test-lag-period
                                (lambda ()
                                  (let ((now (float-time)))
                                    (setq worst (max worst (- now last))
                                          last now))))))
    (unwind-protect
        (let ((value (funcall function)))
          (setq worst (max worst (- (float-time) last)))
          (cons value (max 0.0 (- worst spyglass-test-lag-period))))
      (cancel-timer timer))))

(defun spyglass-test-wait-in-timer (stage)
  "Wait a second when STAGE, a cons, holds `armed', as a timer's function.
STAGE then holds `waiting' while it waits, and `returned' once it
returned, which a throw out of the wait never lets happen: see
`spyglass-test-throw-through'."
  (when (eq (car stage) 'armed)
    (setcar stage 'waiting)
    (let ((end (+ (float-time) 1)))
      (while (< (float-time) end) (accept-process-output nil 0.05)))
    (setcar stage 'returned)))

(defun spyglass-test-throw-through (stage)
  "Throw out of a timer's function waiting, as a `with-timeout' around does.
STAGE, a cons, holds `armed': a timer's function calling
`spyglass-test-wait-in-timer' with it waits a second.  Meanwhile this
waits in a `with-timeout' of 0.2 s, as Anvil's evaluations and
`while-no-input' wait around the timers Emacs runs; its timer fires in
the function's wait, and throws out of it.  Return what STAGE holds
then, `waiting' when the throw went through the function."
  (with-timeout (0.2 nil)
    (while t (accept-process-output nil 0.01)))
  (car stage))

(defun spyglass-test-runs (buffer &optional owner)
  "Return BUFFER's text and mirrored properties as a list of runs.
Each run is (TEXT . PROPS), PROPS sanitized as a mirror keeps them.
OWNER non-nil reads BUFFER as an owner's: the properties
`spyglass-text-properties' names, `font-lock-face' standing for
`face' where it shows."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let ((flf (memq 'font-lock-face (alist-get 'face char-property-alias-alist)))
            runs)
        ;; A buffer without any property has no intervals.
        (dolist (iv (or (object-intervals (current-buffer))
                        (and (> (buffer-size) 0) (list (list 0 (buffer-size) nil)))))
          (let* ((plist (caddr iv))
                 (props (if owner
                            (spyglass-wire-sanitize-props
                             (spyglass--raw-props plist spyglass-text-properties flf)
                             spyglass-wire-style-properties)
                          (spyglass-wire-sanitize-props plist spyglass-wire-style-properties)))
                 (text (buffer-substring-no-properties (1+ (car iv)) (1+ (cadr iv)))))
            (if (and runs (equal (cdar runs) props))
                (setcar (car runs) (concat (caar runs) text))
              (push (cons text props) runs))))
        (nreverse runs)))))

(defun spyglass-test-overlays (buffer &optional owner)
  "Return BUFFER's mirrored overlays as sorted (START END PROPS).
OWNER non-nil reads BUFFER as an owner's."
  (with-current-buffer buffer
    (let (out)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (let ((props (if owner
                         (spyglass--overlay-props ov)
                       (and (overlay-get ov 'spyglass-id) (overlay-properties ov)))))
          (when props
            (push (list (overlay-start ov) (overlay-end ov)
                        (spyglass-wire-sanitize-props props spyglass-wire-style-properties))
                  out))))
      (sort out (lambda (a b) (or (< (car a) (car b))
                                  (and (= (car a) (car b))
                                       (< (cadr a) (cadr b)))))))))

(defun spyglass-test-same-p (owner mirror)
  "Return non-nil when MIRROR shows OWNER's text and properties exactly."
  (and (buffer-live-p mirror)
       (= (buffer-size owner) (buffer-size mirror))
       (equal (spyglass-test-runs owner t) (spyglass-test-runs mirror))))

(defun spyglass-test-diff (owner mirror)
  "Return a description of where MIRROR differs from OWNER, for failures."
  (let ((a (spyglass-test-runs owner t)) (b (spyglass-test-runs mirror)) (i 0))
    (while (and a b (equal (car a) (car b)))
      (setq a (cdr a) b (cdr b) i (1+ i)))
    (list :sizes (list (buffer-size owner) (buffer-size mirror))
          :run i :owner (car a) :mirror (car b))))

(defvar spyglass-test--server-file nil
  "Registration file of the server the tests start.")

(defun spyglass-test-start ()
  "Start a server for one test, registered in a temporary file."
  (setq spyglass-test--server-file (make-temp-file "spyglass-test-" nil ".json"))
  (spyglass-server-start :file spyglass-test--server-file))

(defun spyglass-test-stop ()
  "Close every mirror, connection and share, and stop the server."
  ;; Mirrors first: one reporting its size while connections close
  ;; would fail to send it.
  (dolist (buffer (buffer-list))
    (when (local-variable-p 'spyglass--mirror buffer)
      (kill-buffer buffer)))
  ;; One end says goodbye; the other hears it and closes too.
  (dolist (c (spyglass-connections))
    (ignore-errors (spyglass-disconnect c))
    (spyglass-test-settle 0.02))
  (dolist (share (spyglass-shares))
    (ignore-errors (spyglass-unshare share)))
  (spyglass-server-stop)
  (clrhash spyglass--subs)
  (setq spyglass--connections nil)
  (when (and spyglass-test--server-file (file-exists-p spyglass-test--server-file))
    (delete-file spyglass-test--server-file)))

(defun spyglass-test-connect (&optional name)
  "Connect this Emacs to its own server and wait for the connection.
NAME labels the connection."
  (let ((connection (spyglass-connect (spyglass-file-transport spyglass-test--server-file)
                                      :name (or name "self"))))
    (spyglass-test-wait (lambda () (spyglass-connection-live-p connection)))
    (should (spyglass-connection-live-p connection))
    connection))

(defun spyglass-test-other-end (connection)
  "Return the connection the server accepted for CONNECTION, its other end."
  (cl-find-if (lambda (c) (and (spyglass-connection-server c)
                               (equal (spyglass-connection-peer-id c) spyglass-instance-id)
                               (not (eq c connection))
                               (spyglass-connection-live-p c)))
              (spyglass-connections)))

(defmacro spyglass-test-with-connection (var &rest body)
  "Run BODY with VAR bound to a connection of this Emacs to itself."
  (declare (indent 1) (debug (symbolp body)))
  `(unwind-protect
       (progn
         (spyglass-test-start)
         (let ((,var (spyglass-test-connect)))
           ,@body))
     (spyglass-test-stop)))

(defun spyglass-test-mirror (connection buffer)
  "Share BUFFER, open its mirror on CONNECTION, wait until live, return it."
  (spyglass-share buffer)
  (let ((mirror (spyglass-open connection (buffer-name buffer))))
    (spyglass-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'live)))
    (should (eq (spyglass-mirror-state mirror) 'live))
    mirror))

(defun spyglass-test-converge (owner mirror &optional timeout)
  "Wait until MIRROR shows OWNER, and check that it does."
  (spyglass-test-wait (lambda () (spyglass-test-same-p owner mirror)) (or timeout 10))
  (unless (spyglass-test-same-p owner mirror)
    (ert-fail (list "Mirror differs" (spyglass-test-diff owner mirror)))))

(provide 'spyglass-test-helpers)
;;; spyglass-test-helpers.el ends here
