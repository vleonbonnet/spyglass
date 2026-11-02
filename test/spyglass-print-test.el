;;; spyglass-print-test.el --- What spyglass keeps prints in proportion  -*- lexical-binding: t; -*-

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

;; Emacs prints a value whole wherever nothing bounds it: an error's
;; data, `format', a value evaluated, the list of timers.  Its printer
;; prints a structure again from every path reaching it, so that
;; structures leading back to what holds them all print with the
;; square of their number: each pending request's timer led to its
;; connection, and so to every other request.  This Emacs shares many
;; buffers with itself, mirrors each twice and leaves many requests
;; pending both ways, then prints whole all that spyglass keeps, each
;; against a bound proportional to the count, and what is held once a
;; share, mirror or request against a bound whatever the count.
;; Printing counts characters and stops at the bound, so that a
;; failure is quick rather than a runaway.

;;; Code:

(require 'spyglass-test-helpers)

(defun spyglass-print-test-length (object limit)
  "Return how many characters `prin1' writes for OBJECT, or nil past LIMIT.
OBJECT prints whole, as Emacs prints an error's data, and printing
stops at LIMIT."
  (let ((count 0)
        (print-length nil)
        (print-level nil)
        (print-circle nil))
    (catch 'spyglass-print-test-length
      (prin1 object (lambda (_char)
                      (when (> (setq count (1+ count)) limit)
                        (throw 'spyglass-print-test-length nil))))
      count)))

(defun spyglass-print-test-kept ()
  "Return what spyglass keeps, as (NAME . OBJECT).
The values of the variables named spyglass..., global and local to
any buffer, `timer-list' and `timer-idle-list' and each of their
timers, and the filters, sentinels and properties of processes."
  (let ((kept nil))
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol)
                           (symbol-value symbol)
                           (string-prefix-p "spyglass" (symbol-name symbol)))
                  (push (cons (symbol-name symbol) (symbol-value symbol)) kept))))
    (dolist (buffer (buffer-list))
      (pcase-dolist (`(,variable . ,value) (seq-filter #'consp (buffer-local-variables buffer)))
        (when (and value (string-prefix-p "spyglass" (symbol-name variable)))
          (push (cons (format "%s in %s" variable (buffer-name buffer)) value) kept))))
    (push (cons "timer-list" timer-list) kept)
    (push (cons "timer-idle-list" timer-idle-list) kept)
    (dolist (timer (append timer-list timer-idle-list))
      (push (cons (format "a timer of %s" (let ((function (timer--function timer)))
                                            (if (symbolp function) function (type-of function))))
                  timer)
            kept))
    (dolist (process (process-list))
      (push (cons (format "process %s" (process-name process))
                  (list (process-filter process) (process-sentinel process)
                        (process-plist process)))
            kept))
    kept))

(defconst spyglass-print-test-count 40
  "Shares, each mirrored twice, and requests pending each way.")

(defconst spyglass-print-test-each 4000
  "Characters allowed to print per share, mirror or request kept.")

(ert-deftest spyglass-print-in-proportion ()
  "All spyglass keeps prints in proportion to its shares, mirrors and requests.
Each request's timer led to its connection and every other request,
each subscription to its share and its every other subscription, each
mirror to its connection and every other mirror: printed, each led to
all the others, and the list of timers to the square of the requests."
  (let* ((n spyglass-print-test-count)
         (buffers nil) (app-timers nil) (answers 0)
         (spyglass-test-errors nil))
    (spyglass-register-channel
     "print" #'ignore
     ;; Answered after the test: held meanwhile in a timer of the
     ;; application's, as one answering later would.
     :request (lambda (_connection _message reply)
                (push (run-at-time 600 nil reply '(:late t)) app-timers)))
    (unwind-protect
        (spyglass-test-with-connection client
          (let ((server (spyglass-test-wait (lambda () (spyglass-test-other-end client))))
                (second (spyglass-test-connect "second")))
            (should server)
            ;; Owners key subscriptions by the viewer's instance and
            ;; identifier: both connections are this Emacs's.
            (setf (spyglass-connection-next-sub second) 1000)
            (dotimes (i n)
              (let ((buffer (generate-new-buffer (format "print-%d" i))))
                (push buffer buffers)
                (with-current-buffer buffer
                  (insert (format "share %d\n" i))
                  (put-text-property 1 6 'face 'bold)
                  (overlay-put (make-overlay 1 3) 'face 'italic))
                (spyglass-share buffer)))
            (let ((pairs nil))
              (dolist (buffer buffers)
                (push (cons buffer (spyglass-open client (buffer-name buffer))) pairs)
                (push (cons buffer (spyglass-open second (buffer-name buffer))) pairs))
              (spyglass-test-wait (lambda () (cl-every (lambda (p) (eq (spyglass-mirror-state (cdr p)) 'live))
                                                       pairs))
                                  30)
              (pcase-dolist (`(,buffer . ,mirror) pairs)
                (spyglass-test-converge buffer mirror)))
            ;; Frames not acknowledged yet would print with the text sent.
            (cl-flet ((acked () (cl-every (lambda (share)
                                            (cl-every (lambda (sub) (zerop (spyglass--sub-inflight sub)))
                                                      (spyglass-share-subs share)))
                                          (spyglass-shares))))
              (spyglass-test-wait #'acked 10)
              (should (acked)))
            (should (= (hash-table-count spyglass--subs) (* 2 n)))
            (dotimes (i n)
              (spyglass-channel-request client "print" (list :n i)
                                        (lambda (_reply _error) (cl-incf answers)) 600)
              (spyglass-channel-request server "print" (list :n i)
                                        (lambda (_reply _error) (cl-incf answers)) 600))
            (spyglass-test-wait (lambda () (>= (length app-timers) (* 2 n))) 20)
            (should (= (length app-timers) (* 2 n)))
            (should (= answers 0))
            (should (>= (length (spyglass-connection-requests client)) n))
            (let ((limit (+ 65536 (* spyglass-print-test-each 2 n))))
              (pcase-dolist (`(,name . ,object) (spyglass-print-test-kept))
                (ert-info ((format "%s, for %d shares and requests" name n))
                  (let ((printed (spyglass-print-test-length object limit)))
                    (should printed)))))
            (let ((each nil))
              (dolist (share (spyglass-shares))
                (dolist (sub (spyglass-share-subs share))
                  (push (cons (format "a subscription to %s" (spyglass-share-id share)) sub) each)))
              (dolist (connection (list client second))
                (maphash (lambda (id mirror) (push (cons (format "mirror %s" id) mirror) each))
                         (spyglass-connection-mirrors connection)))
              (dolist (connection (list client server))
                (pcase-dolist (`(,rid . ,callback) (spyglass-connection-requests connection))
                  (push (cons (format "request %s" rid) callback) each)))
              (dolist (timer app-timers)
                (push (cons "a reply kept by the application" timer) each))
              (dolist (timer timer-list)
                (unless (memq timer app-timers)
                  (push (cons "a timer" timer) each)))
              (should (> (length each) (* 6 n)))
              (pcase-dolist (`(,name . ,object) each)
                (ert-info ((format "%s, alone" name))
                  (let ((printed (spyglass-print-test-length object spyglass-print-test-each)))
                    (should printed)))))
            (should-not spyglass-test-errors)))
      (mapc #'cancel-timer app-timers)
      (spyglass-unregister-channel "print")
      (dolist (buffer buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'spyglass-print-test)
;;; spyglass-print-test.el ends here
