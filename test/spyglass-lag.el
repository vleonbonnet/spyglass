;;; spyglass-lag.el --- How long attempts to connect keep Emacs from running  -*- lexical-binding: t; -*-

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

;; `make lag' and `make lag-gui' connect, in a batch or a graphical
;; Emacs, to hosts that refuse, never answer or cannot be resolved,
;; through TCP and SSH, to a host where no server is registered, and
;; to a server, and measure the longest Emacs ran no timer meanwhile:
;; a timer due every 5 ms notes how late it runs.  They use only the
;; public API, so that they measure older spyglass too.
;;
;; The graphical Emacs waits in its command loop, redisplaying, as
;; the user's does; the batch one in `accept-process-output'.  Both
;; run the scenarios from timers.  SSH scenarios need
;; SPYGLASS_LAG_SSH_HOST, a host reached without a prompt and sharing
;; this file system, as localhost; SPYGLASS_LAG_SSH names the program
;; (ssh).  Results go to standard output, or SPYGLASS_LAG_LOG.

;;; Code:

(require 'cl-lib)
(require 'spyglass)
(require 'spyglass-bench-common)

(defconst spyglass-lag-period 0.005
  "Seconds between the ticks of the timer measuring how late timers run.")

(defvar spyglass-lag--results nil
  "Lines of results, newest first.")

(defvar spyglass-lag--errors nil
  "Errors spyglass reported during the current scenario, newest first.")

(defvar spyglass-lag--longest-call 0.0
  "Longest call of `spyglass--reconnect' or `spyglass-connect' in the scenario.")

(defun spyglass-lag--time-call (function &rest args)
  "Call FUNCTION with ARGS, noting how long it took."
  (let ((start (float-time)))
    (prog1 (apply function args)
      (setq spyglass-lag--longest-call
            (max spyglass-lag--longest-call (- (float-time) start))))))

(defun spyglass-lag--note-error (_connection message)
  "Note MESSAGE, an error spyglass reported."
  (push message spyglass-lag--errors))

(defun spyglass-lag--say (format-string &rest args)
  "Note a line of results, formatted from FORMAT-STRING and ARGS."
  (let ((line (apply #'format format-string args)))
    (push line spyglass-lag--results)
    (message "%s" line)))

(defun spyglass-lag--silent-port ()
  "Return a port of 127.0.0.1 never answering, and the processes making it.
A listener accepting nothing, its backlog full: the system drops further
connections' first packets.  As (PORT . PROCESSES)."
  (let* ((server (make-network-process :name "silent" :server 1 :host "127.0.0.1"
                                       :service 0 :family 'ipv4 :stop t :noquery t))
         (port (process-contact server :service))
         (processes (list server)))
    (dotimes (_ 8)
      (push (make-network-process :name "filler" :host "127.0.0.1" :service port
                                  :nowait t :noquery t :sentinel #'ignore)
            processes))
    (cons port processes)))

(defun spyglass-lag--closed-port ()
  "Return a port of 127.0.0.1 nothing listens on."
  (let* ((server (make-network-process :name "closed" :server t :host "127.0.0.1"
                                       :service 0 :family 'ipv4 :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defvar spyglass-lag--server-file nil
  "Registration file of the server the scenarios connect to.")

(defun spyglass-lag--registration ()
  "Return the port and token of this Emacs's server, as a cons."
  (cons (spyglass-server-port (spyglass-server)) (spyglass-server-token (spyglass-server))))

(defun spyglass-lag--failures (count)
  "Return a predicate true once COUNT attempts failed."
  (lambda (_connection) (>= (length spyglass-lag--errors) count)))

(defun spyglass-lag--live-twice ()
  "Return a predicate true once a connection came back after its stream broke."
  (let ((dropped nil))
    (lambda (connection)
      (when (spyglass-connection-live-p connection)
        (if dropped
            t
          (setq dropped t)
          (spyglass-connection-drop-stream connection)
          nil)))))

(defun spyglass-lag--scenarios ()
  "Return the scenarios, as (NAME TRANSPORT-FUNCTION PREDICATE TIMEOUT)."
  (let* ((ssh (let ((program (getenv "SPYGLASS_LAG_SSH")))
                (if (and program (not (string-empty-p program))) program "ssh")))
         (host (let ((host (getenv "SPYGLASS_LAG_SSH_HOST")))
                 (and host (not (string-empty-p host)) host)))
         (silent nil))
    (append
     (list
      (list "tcp refused"
            (lambda () (spyglass-tcp-transport "127.0.0.1" (spyglass-lag--closed-port) "token"))
            (spyglass-lag--failures 3) 20)
      (list "tcp never answers"
            (lambda ()
              (setq silent (spyglass-lag--silent-port))
              (spyglass-tcp-transport "127.0.0.1" (car silent) "token"))
            (spyglass-lag--failures 2) 20
            (lambda () (mapc #'delete-process (cdr silent))))
      (list "tcp name unresolvable"
            (lambda () (spyglass-tcp-transport "spyglass.invalid" 4000 "token"))
            (spyglass-lag--failures 3) 20)
      (list "tcp connects, again"
            (lambda () (spyglass-file-transport spyglass-lag--server-file))
            (spyglass-lag--live-twice) 20)
      (list "ssh host never answers"
            (lambda () (spyglass-ssh-transport "192.0.2.1" :program ssh))
            (spyglass-lag--failures 2) 30)
      (list "ssh name unresolvable"
            (lambda () (spyglass-ssh-transport "spyglass.invalid" :program ssh))
            (spyglass-lag--failures 3) 20))
     (and host
          (list
           (list (format "ssh %s, no server" host)
                 (lambda () (spyglass-ssh-transport host :program ssh
                                                    :file "/nonexistent/spyglass/server.json"))
                 (spyglass-lag--failures 3) 30)
           (list (format "ssh %s connects, again" host)
                 (lambda () (spyglass-ssh-transport host :program ssh
                                                    :file spyglass-lag--server-file))
                 (spyglass-lag--live-twice) 30))))))

(defun spyglass-lag--run (scenarios finish)
  "Run SCENARIOS one after the other from timers, then call FINISH."
  (if (null scenarios)
      (funcall finish)
    (pcase-let* ((`(,name ,make ,predicate ,timeout ,cleanup) (car scenarios))
                 (last (float-time))
                 (worst 0.0)
                 (start (float-time))
                 (gc gc-elapsed)
                 (connection nil)
                 (ticker (run-with-timer spyglass-lag-period spyglass-lag-period
                                         (lambda ()
                                           (let ((now (float-time)))
                                             (setq worst (max worst (- now last))
                                                   last now)))))
                 (watcher nil))
      (setq spyglass-lag--errors nil
            spyglass-lag--longest-call 0.0)
      (setq watcher
            (run-with-timer
             0.05 0.02
             (lambda ()
               (let ((done (funcall predicate connection))
                     (late (> (- (float-time) start) timeout)))
                 (when (or done late)
                   (cancel-timer watcher)
                   (cancel-timer ticker)
                   (spyglass-lag--say "%-28s %-9s %8.1f ms %8.1f ms %8.1f ms  %s"
                                      name (if done "done" "timed out")
                                      (* 1000 (max 0.0 (- worst spyglass-lag-period)))
                                      (* 1000 spyglass-lag--longest-call)
                                      (* 1000 (- gc-elapsed gc))
                                      (let ((error (car (last spyglass-lag--errors))))
                                        (if error (truncate-string-to-width error 70) "")))
                   (spyglass-disconnect connection)
                   (when cleanup (funcall cleanup))
                   (run-with-timer 0.2 nil #'spyglass-lag--run (cdr scenarios) finish))))))
      (setq connection (spyglass-connect (funcall make) :name name)))))

(defun spyglass-lag--write-log ()
  "Write the results to SPYGLASS_LAG_LOG, if set."
  (let ((log (getenv "SPYGLASS_LAG_LOG")))
    (when (and log (not (string-empty-p log)))
      (let ((coding-system-for-write 'utf-8))
        (write-region (concat (mapconcat #'identity (reverse spyglass-lag--results) "\n") "\n")
                      nil log nil 'silent)))))

(defun spyglass-lag-main ()
  "Run the scenarios, print the results and exit."
  (spyglass-bench-start-guard)
  (run-with-timer 500 nil (lambda ()
                            (spyglass-lag--say "ABORT: scenarios still running after 500 s")
                            (spyglass-lag--write-log)
                            (kill-emacs 4)))
  (setq spyglass-reconnect-delays '(0.3))
  (when (boundp 'spyglass-connect-timeout)
    (set 'spyglass-connect-timeout 2.0))
  (add-hook 'spyglass-error-functions #'spyglass-lag--note-error)
  (advice-add 'spyglass--reconnect :around #'spyglass-lag--time-call)
  (advice-add 'spyglass-connect :around #'spyglass-lag--time-call)
  (setq spyglass-lag--server-file (make-temp-file "spyglass-lag-" nil ".json"))
  (spyglass-server-start :file spyglass-lag--server-file)
  (spyglass-lag--say "spyglass lag, %s Emacs %s, %s"
                     (if noninteractive "batch" "graphical") emacs-version system-type)
  (spyglass-lag--say "%-28s %-9s %11s %11s %11s  %s" "scenario" "outcome" "max lag"
                     "longest call" "GC" "last error")
  (let ((finished nil))
    (spyglass-lag--run
     (spyglass-lag--scenarios)
     (lambda ()
       (spyglass-server-stop)
       (when (file-exists-p spyglass-lag--server-file)
         (delete-file spyglass-lag--server-file))
       (spyglass-lag--write-log)
       (setq finished t)
       (unless noninteractive
         (kill-emacs 0))))
    (when noninteractive
      (while (not finished)
        (spyglass-bench-step 0.005))
      (kill-emacs 0))))

(provide 'spyglass-lag)
;;; spyglass-lag.el ends here
