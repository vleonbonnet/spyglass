;;; spyglass-transport-test.el --- Tests of transports, which never have Emacs wait  -*- lexical-binding: t; -*-

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

;; Attempts to connect, failing or not, through each transport, while a
;; timer measures how long Emacs goes without running it: no step of an
;; attempt may have Emacs wait.  A host that never answers is a
;; listener of this Emacs that accepts nothing, its backlog full; SSH,
;; a shell script running the command here or forwarding with nc.
;; Nothing goes beyond 127.0.0.1 but the resolving of a name that
;; cannot resolve.  `spyglass-gui-test.el' runs the same attempts in a
;; graphical Emacs.

;;; Code:

(require 'spyglass-test-helpers)

(defconst spyglass-transport-test-max-lag 0.1
  "Seconds Emacs may go without running timers during an attempt, here.
An attempt itself takes a few milliseconds; a loaded machine more.")

(defun spyglass-transport-test-attempt (transport predicate &optional timeout)
  "Connect through TRANSPORT until PREDICATE, given the connection, is non-nil.
Return (CONNECTION . LAG): LAG is how long Emacs went without running
timers, from connecting until PREDICATE held or TIMEOUT seconds (10 by
default) passed.  The connection is left as it is."
  (let ((connection nil))
    (spyglass-test-call-with-lag
     (lambda ()
       (setq connection (spyglass-connect transport :name "attempt"))
       (spyglass-test-wait (lambda () (funcall predicate connection)) (or timeout 10))
       connection))))

(defun spyglass-transport-test-reported (pattern)
  "Return a predicate true once an error matching PATTERN was reported."
  (lambda (_connection)
    (cl-some (lambda (message) (string-match-p pattern message)) spyglass-test-errors)))

(defun spyglass-transport-test-silent-port ()
  "Return a port of 127.0.0.1 where connections neither complete nor fail.
Its listener accepts none: once its backlog is full, the system drops
the next connections' first packets, as a host gone silent would, and
a connection waits for an answer, about a minute.  The value
is (PORT . PROCESSES), to delete when done."
  (let* ((server (make-network-process :name "silent" :server 1 :host "127.0.0.1"
                                       :service 0 :family 'ipv4 :stop t :noquery t))
         (port (process-contact server :service))
         (processes (list server)))
    (dotimes (_ 8)
      (push (make-network-process :name "filler" :host "127.0.0.1" :service port
                                  :nowait t :noquery t :sentinel #'ignore)
            processes))
    (spyglass-test-settle 0.3)
    (let ((probe (make-network-process :name "probe" :host "127.0.0.1" :service port
                                       :nowait t :noquery t :sentinel #'ignore)))
      (push probe processes)
      (spyglass-test-settle 0.5)
      (unless (eq (process-status probe) 'connect)
        (mapc #'delete-process processes)
        (ert-skip "No connection waits on a full backlog here")))
    (cons port processes)))

(defun spyglass-transport-test-closed-port ()
  "Return a port of 127.0.0.1 nothing listens on."
  (let* ((server (make-network-process :name "closed" :server t :host "127.0.0.1"
                                       :service 0 :family 'ipv4 :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defmacro spyglass-transport-test-with (bindings &rest body)
  "Run BODY with spyglass's options bound by BINDINGS, then clean up.
Each attempt is spaced, so that a test sees the first, and errors are
gathered in `spyglass-test-errors'."
  (declare (indent 1) (debug (sexp body)))
  `(let ((spyglass-test-errors nil)
         (spyglass-reconnect-delays '(30))
         (spyglass-connect-timeout 20.0)
         ,@bindings)
     (unwind-protect
         (progn ,@body)
       (spyglass-test-stop))))

;;;; TCP

(ert-deftest spyglass-transport-tcp-silent-host ()
  "Connecting to a host that never answers has Emacs wait for nothing, and times out."
  (let ((silent (spyglass-transport-test-silent-port)))
    (unwind-protect
        (spyglass-transport-test-with ((spyglass-connect-timeout 1.0))
          (pcase-let ((`(,connection . ,lag)
                       (spyglass-transport-test-attempt
                        (spyglass-tcp-transport "127.0.0.1" (car silent) "token")
                        (spyglass-transport-test-reported "timed out"))))
            (message "silent host: Emacs waited %.1f ms" (* 1000 lag))
            (should (cl-some (lambda (m) (string-match-p "timed out after 1s" m)) spyglass-test-errors))
            (should (eq (spyglass-connection-state connection) 'reconnecting))
            (should (< lag spyglass-transport-test-max-lag))
            ;; The socket is gone with the attempt.
            (should-not (cl-some (lambda (p) (and (equal (process-name p) "spyglass")
                                                  (process-live-p p)))
                                 (process-list)))))
      (mapc #'delete-process (cdr silent)))))

(ert-deftest spyglass-transport-tcp-refused ()
  "A refused connection fails at once, and the connection tries again."
  (spyglass-transport-test-with ()
    (pcase-let ((`(,connection . ,lag)
                 (spyglass-transport-test-attempt
                  (spyglass-tcp-transport "127.0.0.1" (spyglass-transport-test-closed-port) "token")
                  (spyglass-transport-test-reported "Cannot connect"))))
      (should (cl-some (lambda (m) (string-match-p "Cannot connect to attempt: 127.0.0.1 port [0-9]+: failed"
                                                   m))
                       spyglass-test-errors))
      (should (eq (spyglass-connection-state connection) 'reconnecting))
      (should (spyglass-connection-timer connection))
      (should (< lag spyglass-transport-test-max-lag)))))

(defun spyglass-transport-test-call-with-hosts (function)
  "Call FUNCTION, returning its value and the hosts sockets were opened to.
As (VALUE . HOSTS): the `:host' of every client `make-network-process'
made meanwhile."
  (let* ((hosts nil)
         (record (lambda (&rest args)
                   (unless (plist-get args :server)
                     (push (plist-get args :host) hosts)))))
    (advice-add 'make-network-process :before record)
    (unwind-protect
        (let ((value (funcall function)))
          (cons value hosts))
      (advice-remove 'make-network-process record))))

(ert-deftest spyglass-transport-tcp-resolves-elsewhere ()
  "A host name is resolved by another Emacs: Emacs only connects to addresses."
  (spyglass-transport-test-with ()
    (spyglass-test-start)
    (let* ((reg (with-temp-buffer
                  (insert-file-contents spyglass-test--server-file)
                  (spyglass-parse-registration (buffer-string))))
           (result (spyglass-transport-test-call-with-hosts
                    (lambda ()
                      (spyglass-transport-test-attempt
                       (spyglass-tcp-transport "localhost" (car reg) (cdr reg))
                       #'spyglass-connection-live-p))))
           (connection (car (car result)))
           (hosts (cdr result)))
      (should (spyglass-connection-live-p connection))
      (should hosts)
      (dolist (host hosts)
        (should (member host '("127.0.0.1" "0:0:0:0:0:0:0:1" "::1"))))
      (should (< (cdr (car result)) spyglass-transport-test-max-lag)))))

(ert-deftest spyglass-transport-tcp-unresolvable ()
  "A host name that cannot resolve fails without Emacs resolving it."
  (spyglass-transport-test-with ()
    (let* ((result (spyglass-transport-test-call-with-hosts
                    (lambda ()
                      (spyglass-transport-test-attempt
                       (spyglass-tcp-transport "spyglass.invalid" 4000 "token")
                       (spyglass-transport-test-reported "Cannot connect")))))
           (lag (cdr (car result))))
      (message "unresolvable host: Emacs waited %.1f ms" (* 1000 lag))
      (should (cl-some (lambda (m) (string-match-p "cannot resolve spyglass.invalid" m))
                       spyglass-test-errors))
      (should-not (cdr result))
      (should-not (get-buffer " *spyglass-resolve*"))
      (should (< lag spyglass-transport-test-max-lag)))))

(ert-deftest spyglass-transport-tcp-resolving-times-out ()
  "An attempt resolving a name for too long fails, its resolver gone."
  (spyglass-transport-test-with ((spyglass-connect-timeout 0.001))
    (pcase-let ((`(,connection . ,lag)
                 (spyglass-transport-test-attempt
                  (spyglass-tcp-transport "localhost" 4000 "token")
                  (spyglass-transport-test-reported "Cannot connect"))))
      (should (cl-some (lambda (m) (string-match-p "connecting to localhost timed out" m))
                       spyglass-test-errors))
      (should (eq (spyglass-connection-state connection) 'reconnecting))
      (should-not (cl-some (lambda (p) (and (equal (process-name p) "spyglass-resolve")
                                            (process-live-p p)))
                           (process-list)))
      (should-not (get-buffer " *spyglass-resolve*"))
      (should (< lag spyglass-transport-test-max-lag)))))

;;;; Registration files

(ert-deftest spyglass-transport-file-remote ()
  "A registration file of another host is not fetched: it is not found here."
  (spyglass-transport-test-with ()
    (let ((tramp (featurep 'tramp)))
      (pcase-let ((`(,connection . ,lag)
                   (spyglass-transport-test-attempt
                    (spyglass-file-transport "/ssh:spyglass.invalid:/server.json")
                    (spyglass-transport-test-reported "Cannot connect"))))
        (should (cl-some (lambda (m) (string-match-p "no registration file" m))
                         spyglass-test-errors))
        (should (eq (spyglass-connection-state connection) 'reconnecting))
        (should (eq (featurep 'tramp) tramp))
        (should (< lag spyglass-transport-test-max-lag))))))

;;;; SSH

(defconst spyglass-transport-test-ssh-script
  "#!/bin/sh
# A stand-in for ssh: runs the command here, or forwards to the port.
forward=
while [ $# -gt 0 ]; do
  case $1 in
    -W) forward=$2; shift 2 ;;
    -[bcDEeFIiJLlmOopQRSw]) shift 2 ;;
    -*) shift ;;
    *) break ;;
  esac
done
shift
if [ -n \"$SPYGLASS_FAKE_SSH_LOG\" ]; then
  echo \"${forward:-$*}\" >> \"$SPYGLASS_FAKE_SSH_LOG\"
fi
if [ -n \"$forward\" ]; then
  exec nc \"${forward%:*}\" \"${forward##*:}\"
fi
if [ -n \"$SPYGLASS_FAKE_SSH_DELAY\" ]; then
  exec sleep \"$SPYGLASS_FAKE_SSH_DELAY\"
fi
exec /bin/sh -c \"$*\"
"
  "A shell script standing for ssh, for `spyglass-transport-test-with-ssh'.
It logs each command, or port forwarded to, in SPYGLASS_FAKE_SSH_LOG,
and sleeps SPYGLASS_FAKE_SSH_DELAY seconds rather than run a command.")

(defmacro spyglass-transport-test-with-ssh (vars &rest body)
  "Run BODY with VARS, (PROGRAM [LOG]), bound to a stand-in for ssh and its log.
The log is a file of the commands run, one per line.  Skip without a
POSIX shell and nc."
  (declare (indent 1) (debug (sexp body)))
  (let ((dir (make-symbol "dir"))
        (program (car vars))
        (log (or (cadr vars) (make-symbol "log"))))
    `(progn
       (unless (and (not (memq system-type '(windows-nt ms-dos)))
                    (file-executable-p "/bin/sh")
                    (executable-find "nc"))
         (ert-skip "No POSIX shell or nc"))
       (let* ((,dir (make-temp-file "spyglass-ssh-" t))
              (,program (expand-file-name "ssh" ,dir))
              (,log (expand-file-name "log" ,dir))
              (process-environment (cons (concat "SPYGLASS_FAKE_SSH_LOG=" ,log)
                                         process-environment)))
         (unwind-protect
             (progn
               (with-temp-file ,program
                 (insert spyglass-transport-test-ssh-script))
               (set-file-modes ,program #o700)
               (write-region "" nil ,log nil 'silent)
               ,@body)
           (delete-directory ,dir t))))))

(defun spyglass-transport-test-log (log)
  "Return the lines of LOG, the stand-in for ssh's."
  (with-temp-buffer
    (insert-file-contents log)
    (split-string (buffer-string) "\n" t)))

(ert-deftest spyglass-transport-ssh-unregistered ()
  "A host where no server is registered fails, with what ssh said."
  (spyglass-transport-test-with-ssh (ssh log)
    (spyglass-transport-test-with ()
      (pcase-let ((`(,connection . ,lag)
                   (spyglass-transport-test-attempt
                    (spyglass-ssh-transport "peer" :program ssh :file "/nonexistent/server.json")
                    (spyglass-transport-test-reported "Cannot connect"))))
        (should (cl-some (lambda (m) (string-match-p "no spyglass server registered on peer: .*nonexistent" m))
                         spyglass-test-errors))
        (should (eq (spyglass-connection-state connection) 'reconnecting))
        (should (equal (spyglass-transport-test-log log) '("cat /nonexistent/server.json")))
        (should-not (get-buffer " *spyglass-registration*"))
        (should-not (get-buffer " *spyglass-registration-stderr*"))
        (should (< lag spyglass-transport-test-max-lag))))))

(ert-deftest spyglass-transport-ssh-registration-times-out ()
  "Reading a registration for too long fails, the reading process gone."
  (spyglass-transport-test-with-ssh (ssh)
    (spyglass-transport-test-with ((spyglass-connect-timeout 0.5)
                                   (process-environment (cons "SPYGLASS_FAKE_SSH_DELAY=30"
                                                              process-environment)))
      (pcase-let ((`(,connection . ,lag)
                   (spyglass-transport-test-attempt
                    (spyglass-ssh-transport "peer" :program ssh :file "/nonexistent/server.json")
                    (spyglass-transport-test-reported "Cannot connect")
                    5)))
        (should (cl-some (lambda (m) (string-match-p "registration on peer timed out after 0.5s" m))
                         spyglass-test-errors))
        (should (eq (spyglass-connection-state connection) 'reconnecting))
        (should-not (cl-some (lambda (p) (and (string-prefix-p "spyglass-registration" (process-name p))
                                              (process-live-p p)))
                             (process-list)))
        (should-not (get-buffer " *spyglass-registration*"))
        (should (< lag spyglass-transport-test-max-lag))))))

(ert-deftest spyglass-transport-ssh-connects-once-registered ()
  "Once connected, connecting again goes straight to the port: one process."
  (spyglass-transport-test-with-ssh (ssh log)
    (spyglass-transport-test-with ((spyglass-reconnect-delays '(0.2)))
      (spyglass-test-start)
      (pcase-let ((`(,connection . ,lag)
                   (spyglass-transport-test-attempt
                    (spyglass-ssh-transport "peer" :program ssh :file spyglass-test--server-file)
                    #'spyglass-connection-live-p)))
        (should (spyglass-connection-live-p connection))
        (should (< lag spyglass-transport-test-max-lag))
        (let ((port (spyglass-server-port (spyglass-server))))
          (should (equal (spyglass-transport-test-log log)
                         (list (concat "cat " spyglass-test--server-file)
                               (format "127.0.0.1:%d" port))))
          (spyglass-connection-drop-stream connection)
          (should-not (spyglass-connection-live-p connection))
          (spyglass-test-wait (lambda () (spyglass-connection-live-p connection)))
          (should (spyglass-connection-live-p connection))
          (should (equal (spyglass-transport-test-log log)
                         (list (concat "cat " spyglass-test--server-file)
                               (format "127.0.0.1:%d" port)
                               (format "127.0.0.1:%d" port))))
          (should-not spyglass-test-errors))))))

(ert-deftest spyglass-transport-ssh-reads-again-after-restart ()
  "When the server moved, the registration is read again and the connection comes back."
  (spyglass-transport-test-with-ssh (ssh log)
    (spyglass-transport-test-with ((spyglass-reconnect-delays '(0.2)))
      (spyglass-test-start)
      (let* ((file spyglass-test--server-file)
             (connection (spyglass-connect (spyglass-ssh-transport "peer" :program ssh :file file)
                                           :name "peer")))
        (spyglass-test-wait (lambda () (spyglass-connection-live-p connection)))
        (should (spyglass-connection-live-p connection))
        (let ((old (spyglass-server-port (spyglass-server))))
          (spyglass-server-stop)
          (spyglass-server-start :file file)
          (should-not (= old (spyglass-server-port (spyglass-server))))
          (spyglass-connection-drop-stream connection)
          (spyglass-test-wait (lambda () (spyglass-connection-live-p connection)))
          (should (spyglass-connection-live-p connection))
          (should (equal (spyglass-transport-test-log log)
                         (list (concat "cat " file)
                               (format "127.0.0.1:%d" old)
                               (format "127.0.0.1:%d" old)
                               (concat "cat " file)
                               (format "127.0.0.1:%d" (spyglass-server-port (spyglass-server)))))))))))

(ert-deftest spyglass-transport-ssh-from-any-directory ()
  "A buffer whose directory is gone does not stop a transport starting ssh."
  (spyglass-transport-test-with-ssh (ssh)
    (spyglass-transport-test-with ((spyglass-reconnect-delays '(0.2)))
      (spyglass-test-start)
      (let ((gone (make-temp-file "spyglass-gone-" t))
            (buffer (generate-new-buffer "gone")))
        (unwind-protect
            (with-current-buffer buffer
              (setq default-directory (file-name-as-directory gone))
              (delete-directory gone)
              (pcase-let ((`(,connection . ,lag)
                           (spyglass-transport-test-attempt
                            (spyglass-ssh-transport "peer" :program ssh
                                                    :file spyglass-test--server-file)
                            #'spyglass-connection-live-p)))
                (should (spyglass-connection-live-p connection))
                (should-not spyglass-test-errors)
                ;; Again, as when a timer runs in such a buffer.
                (spyglass-connection-drop-stream connection)
                (spyglass-test-wait (lambda () (spyglass-connection-live-p connection)))
                (should (spyglass-connection-live-p connection))
                (should-not spyglass-test-errors)
                (should (< lag spyglass-transport-test-max-lag))))
          (kill-buffer buffer))))))

(provide 'spyglass-transport-test)
;;; spyglass-transport-test.el ends here
