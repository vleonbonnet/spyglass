;;; spyglass-conn-test.el --- Frames written whole on a stream  -*- lexical-binding: t; -*-

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

;; A write the stream cannot take at once makes Emacs wait inside
;; `process-send-string', running timers and process filters: what they
;; send must not go between the parts of the frame being written.  This
;; Emacs talks to itself through 127.0.0.1: frames far larger than the
;; sockets' buffers make writes wait while the other end's pings,
;; acknowledgments and channel messages arrive, and every frame must
;; decode whole on the other side.

;;; Code:

(require 'spyglass-test-helpers)

(defvar spyglass-conn-test--frames nil
  "Frames of type \"tst\" received, newest first, as (CONNECTION HEADER PAYLOAD).")

(defun spyglass-conn-test--record (connection header payload)
  "Record a test frame, HEADER and PAYLOAD, received on CONNECTION."
  (push (list connection header payload) spyglass-conn-test--frames))

(defmacro spyglass-conn-test--with (vars &rest body)
  "Run BODY with VARS, (CLIENT SERVER), the two ends of a connection to itself.
Frames of type \"tst\" go to `spyglass-conn-test--frames' meanwhile."
  (declare (indent 1) (debug (sexp body)))
  `(let ((spyglass-conn-test--frames nil)
         (spyglass-test-errors nil))
     (spyglass--register-handler "tst" #'spyglass-conn-test--record)
     (unwind-protect
         (spyglass-test-with-connection ,(car vars)
           (let ((,(cadr vars) (spyglass-test-wait
                                (lambda () (spyglass-test-other-end ,(car vars))))))
             (should ,(cadr vars))
             ,@body))
       (remhash "tst" spyglass--handlers))))

(defvar spyglass-conn-test--writing nil
  "Non-nil while a test writes its large frame.")

(defconst spyglass-conn-test--part-size (* 8 1024 1024)
  "Bytes of each of the two parts of the large frame, far above sockets' buffers.")

(defun spyglass-conn-test--write-large (client prepare)
  "Write a large frame in parts on CLIENT, then a small one; PREPARE runs first.
What PREPARE arranges, a frame from the other end for CLIENT's
filter to answer or a timer, happens while the write waits.  The
large frame is numbered 1, the small one 2.  Return non-nil when
CLIENT's filter ran while the large frame was written: else the write
never waited, and the test proves nothing."
  (let* ((nested nil)
         (watch (lambda (connection)
                  (when (and spyglass-conn-test--writing (eq connection client))
                    (setq nested t)))))
    (add-hook 'spyglass--after-input-functions watch)
    (unwind-protect
        (progn
          (funcall prepare)
          (setq spyglass-conn-test--writing t)
          (should (spyglass--send-frame
                   client (spyglass-wire-frame-parts
                           (list :t "tst" :n 1)
                           (list (make-string spyglass-conn-test--part-size ?a)
                                 (make-string spyglass-conn-test--part-size ?b)))))
          (setq spyglass-conn-test--writing nil)
          (should (spyglass--send client (list :t "tst" :n 2))))
      (setq spyglass-conn-test--writing nil)
      (remove-hook 'spyglass--after-input-functions watch))
    nested))

(defun spyglass-conn-test--check-large (server &optional order)
  "Check that SERVER received the test frames whole, in ORDER.
ORDER is the list of their numbers, (1 2) by default."
  (spyglass-test-wait (lambda () (>= (length spyglass-conn-test--frames) (length (or order '(1 2)))))
                      30)
  (should-not spyglass-test-errors)
  (let* ((frames (reverse spyglass-conn-test--frames))
         (large (cl-find 1 frames :key (lambda (f) (plist-get (nth 1 f) :n))))
         (payload (nth 2 large))
         (half spyglass-conn-test--part-size))
    (should (equal (mapcar (lambda (f) (plist-get (nth 1 f) :n)) frames) (or order '(1 2))))
    (should (eq (car large) server))
    (should (= (length payload) (* 2 half)))
    (should (string= (substring payload 0 half) (make-string half ?a)))
    (should (string= (substring payload half) (make-string half ?b)))))

(ert-deftest spyglass-conn-frame-whole-while-filter-answers ()
  "A pong answered while a frame in parts waits to be written follows it whole.
The other end's ping waits to be read as the frame is written: Emacs
reads it, and answers it, while the write waits."
  (spyglass-conn-test--with (client server)
    (setf (spyglass-connection-rtt server) nil)
    (should (spyglass-conn-test--write-large client (lambda () (spyglass--ping server))))
    (spyglass-conn-test--check-large server)
    (spyglass-test-wait (lambda () (spyglass-connection-rtt server)) 5)
    (should (spyglass-connection-rtt server))
    (should (spyglass-connection-live-p client))))

(ert-deftest spyglass-conn-frame-whole-while-mirror-acknowledges ()
  "A mirror acknowledging an update while a frame in parts is written waits for it.
The update reaches the mirror while its end writes the frame."
  (spyglass-conn-test--with (client server)
    (let ((buffer (generate-new-buffer "ack")))
      (unwind-protect
          (let* ((mirror (spyglass-test-mirror client buffer))
                 (sub (car (spyglass-share-subs (spyglass-share-of buffer)))))
            (should (spyglass-conn-test--write-large
                     client (lambda ()
                              (with-current-buffer buffer (insert "acknowledged\n"))
                              ;; The owner writes the update now.
                              (spyglass--flush))))
            (spyglass-conn-test--check-large server)
            (spyglass-test-converge buffer mirror)
            (spyglass-test-wait (lambda () (= (spyglass--sub-acked sub) (spyglass--sub-seq sub))) 5)
            (should (= (spyglass--sub-acked sub) (spyglass--sub-seq sub))))
        (kill-buffer buffer)))))

(ert-deftest spyglass-conn-frame-whole-while-timer-sends ()
  "A frame a timer sends while a frame in parts is written goes right after it."
  (spyglass-conn-test--with (client server)
    (let ((fired nil))
      (spyglass-conn-test--write-large
       client (lambda ()
                (run-at-time 0 nil (lambda ()
                                     (setq fired spyglass-conn-test--writing)
                                     (spyglass--send client (list :t "tst" :n 3))))))
      ;; Else the timer ran after the write, and the test proves nothing.
      (should fired)
      (spyglass-conn-test--check-large server '(1 3 2)))))

(ert-deftest spyglass-conn-frame-whole-while-channel-answers ()
  "A channel's answer to a request read while a frame in parts is written follows it."
  (spyglass-conn-test--with (client server)
    (let ((answer nil) (deferred nil))
      (spyglass-register-channel "tst" #'ignore
                                 :request (lambda (c _m reply)
                                            (when (and spyglass-conn-test--writing (eq c client))
                                              (setq deferred (spyglass--writing-p c)))
                                            (funcall reply '(:ok t))))
      (unwind-protect
          (progn
            (should (spyglass-conn-test--write-large
                     client (lambda ()
                              (spyglass-channel-request server "tst" '(:op "now")
                                                        (lambda (reply error)
                                                          (setq answer (list reply error)))))))
            (should deferred)
            (spyglass-conn-test--check-large server)
            (spyglass-test-wait (lambda () answer) 5)
            (should (equal answer '((:ok t) nil))))
        (spyglass-unregister-channel "tst")))))

(ert-deftest spyglass-conn-frames-handled-in-order ()
  "Frames read while a handler's write waits are handled after those before them.
A handler of the first of two frames read together writes a frame far
larger than the sockets' buffers; while it waits, a third frame
arrives, and Emacs runs the filter again: it must wait for the second."
  (spyglass-conn-test--with (client server)
    (let ((order nil) (fired nil))
      (spyglass--register-handler
       "ord" (lambda (connection header _payload)
               (when (eq connection client)
                 (push (plist-get header :n) order)
                 (when (eql (plist-get header :n) 1)
                   (run-at-time 0 nil (lambda ()
                                        (setq fired t)
                                        (spyglass--send server (list :t "ord" :n 3))))
                   (spyglass--send-frame
                    client (spyglass-wire-frame-parts
                            (list :t "tst" :n 1)
                            (list (make-string spyglass-conn-test--part-size ?a)
                                  (make-string spyglass-conn-test--part-size ?b))))))))
      (unwind-protect
          (progn
            ;; One write: both frames reach the client in one chunk.
            (should (spyglass--send-frame server
                                          (concat (spyglass-wire-frame (list :t "ord" :n 1))
                                                  (spyglass-wire-frame (list :t "ord" :n 2)))))
            (spyglass-test-wait (lambda () (>= (length order) 3)) 30)
            (should fired)
            (should (equal (reverse order) '(1 2 3)))
            (spyglass-conn-test--check-large server '(1)))
        (remhash "ord" spyglass--handlers)))))

(ert-deftest spyglass-conn-queue-bounded ()
  "A peer making frames wait past `spyglass-queue-limit' loses its stream.
It sends pings while not reading the frame written to it."
  (spyglass-conn-test--with (client server)
    (let* ((spyglass-queue-limit 2000)
           (states nil)
           (spyglass-connection-state-functions
            (cons (lambda (c state) (when (eq c client) (push state states)))
                  spyglass-connection-state-functions)))
      (dotimes (_ 100) (spyglass--ping server))
      (spyglass--send-frame client (spyglass-wire-frame-parts
                                    (list :t "tst" :n 1)
                                    (list (make-string spyglass-conn-test--part-size ?a))))
      (should (seq-some (lambda (m) (string-match-p "wait to be sent" m)) spyglass-test-errors))
      (should (memq 'reconnecting states))
      ;; Not reported: the stream was dropped on purpose.
      (should-not (seq-some (lambda (m) (string-match-p "Sending failed" m)) spyglass-test-errors))
      (spyglass-test-wait (lambda () (spyglass-connection-live-p client)) 10)
      (should (spyglass-connection-live-p client)))))

(ert-deftest spyglass-conn-inbox-bounded ()
  "A peer sending past `spyglass-queue-limit' while its frames wait loses its stream.
They wait while a handler's write waits for the peer to read."
  (spyglass-conn-test--with (client server)
    (let* ((spyglass-queue-limit 20000)
           (states nil)
           (spyglass-connection-state-functions
            (cons (lambda (c state) (when (eq c client) (push state states)))
                  spyglass-connection-state-functions)))
      (spyglass--register-handler
       "ord" (lambda (connection _header _payload)
               (when (eq connection client)
                 (run-at-time 0 nil (lambda ()
                                      (dotimes (n 100)
                                        (spyglass--send server (list :t "ord" :n n)
                                                        (make-string 1000 ?x)))))
                 (spyglass--send-frame
                  client (spyglass-wire-frame-parts
                          (list :t "tst" :n 1)
                          (list (make-string spyglass-conn-test--part-size ?a)))))))
      (unwind-protect
          (progn
            (spyglass--send server (list :t "ord" :n 0))
            (spyglass-test-wait (lambda () (memq 'reconnecting states)) 30)
            (should (seq-some (lambda (m) (string-match-p "wait to be handled" m))
                              spyglass-test-errors))
            (should (memq 'reconnecting states)))
        (remhash "ord" spyglass--handlers)))))

(ert-deftest spyglass-conn-failed-write-drops-stream ()
  "A write that fails drops its stream, which comes back; the mirror resumes."
  (spyglass-conn-test--with (client _server)
    (let ((buffer (generate-new-buffer "failed-write")))
      (unwind-protect
          (let* ((mirror (spyglass-test-mirror client buffer))
                 (process (spyglass-connection-process client))
                 (send (symbol-function 'process-send-string))
                 (failed nil))
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (proc string)
                         (if (and (eq proc process) (not failed))
                             (progn (setq failed t)
                                    (signal 'file-error
                                            (list "Writing to process" "Input/output error")))
                           (funcall send proc string)))))
              (should-not (spyglass--send client (list :t "ping" :t0 (float-time)))))
            (should failed)
            (should (seq-some (lambda (m) (string-match-p "Sending failed" m)) spyglass-test-errors))
            ;; At once, not when a heartbeat times out.
            (should-not (process-live-p process))
            (should (eq (spyglass-connection-state client) 'reconnecting))
            (spyglass-test-wait (lambda () (spyglass-connection-live-p client)))
            (should (spyglass-connection-live-p client))
            (with-current-buffer buffer (insert "after the failure\n"))
            (spyglass-test-converge buffer mirror)
            (should (eq (spyglass-mirror-state mirror) 'live)))
        (kill-buffer buffer)))))

(ert-deftest spyglass-conn-reconnect ()
  "`spyglass-reconnect' breaks a stream on either end; mirrors resume."
  (spyglass-conn-test--with (client server)
    (let ((buffer (generate-new-buffer "reconnect")))
      (unwind-protect
          (let ((mirror (spyglass-test-mirror client buffer)))
            ;; The connecting end.
            (let ((process (spyglass-connection-process client)))
              (should (spyglass-reconnect client))
              (should-not (process-live-p process))
              (spyglass-test-wait (lambda () (spyglass-connection-live-p client)))
              (should (spyglass-connection-live-p client)))
            ;; Waiting to connect again: now.
            (let ((spyglass-reconnect-delays '(5)))
              (spyglass-connection-drop-stream client))
            (should (eq (spyglass-connection-state client) 'reconnecting))
            (let ((start (float-time)))
              (should (spyglass-reconnect client))
              (spyglass-test-wait (lambda () (spyglass-connection-live-p client)) 4)
              (should (spyglass-connection-live-p client))
              (should (< (- (float-time) start) 4)))
            ;; The accepting end: the peer comes back, the stream is adopted.
            (setq server (spyglass-test-wait (lambda () (spyglass-test-other-end client))))
            (should (spyglass-reconnect server))
            (should-not (spyglass-reconnect server))
            (spyglass-test-wait (lambda () (spyglass-connection-live-p server)) 8)
            (should (spyglass-connection-live-p server))
            (with-current-buffer buffer (insert "after reconnecting\n"))
            (spyglass-test-converge buffer mirror)
            (should (eq (spyglass-mirror-state mirror) 'live))
            (should (>= (plist-get (spyglass-mirror-stats mirror) :resumes) 2))
            (spyglass-disconnect client)
            (should-not (spyglass-reconnect client)))
        (kill-buffer buffer)))))

(defun spyglass-conn-test--fill (buffer lines tag)
  "Add LINES lines tagged TAG to BUFFER, with faces now and then.
Lines go at the end and, every few, at places spread through the
buffer, so that frames carry several ranges: several parts."
  (with-current-buffer buffer
    (dotimes (i lines)
      (goto-char (if (and (zerop (% i 50)) (> (buffer-size) 0))
                     (save-excursion
                       (goto-char (1+ (% (* i 7919) (buffer-size))))
                       (line-beginning-position))
                   (point-max)))
      (let ((start (point)))
        (insert (format "%s %07d %s\n" tag i (make-string 90 (+ ?a (% i 26)))))
        (when (zerop (% i 7))
          (put-text-property start (+ start 5) 'face 'bold))))))

(ert-deftest spyglass-conn-mirrors-both-ways-slow-readers ()
  "Each end mirrors the other's large buffer over one stream, reading slowly.
Each end stops reading for a while in turn, so that the other's
updates, written in parts, wait for the stream; meanwhile each end
pings, acknowledges what it applied and sends channel messages, which
the waiting end's filter and timers answer.  Every frame must decode:
the mirrors end exact, without an error or the stream ever breaking."
  (let ((spyglass-window (* 32 1024 1024)))
    (spyglass-conn-test--with (client server)
      (let* ((a (generate-new-buffer "both-a"))
             (b (generate-new-buffer "both-b"))
             (states nil)
             (spyglass-connection-state-functions
              (cons (lambda (c state) (when (memq c (list client server)) (push state states)))
                    spyglass-connection-state-functions))
             (received 0)
             (turn 0)
             (pinger (run-at-time 0 0.002 (lambda ()
                                            (spyglass--ping client)
                                            (spyglass--ping server)
                                            (spyglass-channel-send client "tst" '(:n 1))
                                            (spyglass-channel-send server "tst" '(:n 2)))))
             ;; One end stops reading for 30 ms, then the other.
             (reader (run-at-time 0 0.03 (lambda ()
                                           (let ((ends (list (spyglass-connection-process client)
                                                             (spyglass-connection-process server))))
                                             (dolist (p ends)
                                               (when (process-live-p p) (continue-process p)))
                                             (let ((p (nth (% (cl-incf turn) 3) ends)))
                                               (when (process-live-p p) (stop-process p))))))))
        (spyglass-register-channel "tst" (lambda (_c _m) (cl-incf received)))
        (unwind-protect
            (progn
              (spyglass-conn-test--fill a 20000 "a")
              (spyglass-conn-test--fill b 20000 "b")
              (spyglass-share a)
              (spyglass-share b)
              ;; Both ends are this Emacs: owners key subscriptions by the
              ;; viewer's instance and identifier, which must differ.
              (setf (spyglass-connection-next-sub server) 1000)
              (let ((mirror-a (spyglass-open client (buffer-name a)))
                    (mirror-b (spyglass-open server (buffer-name b))))
                (dotimes (_ 20)
                  (spyglass-conn-test--fill a 4000 "a")
                  (spyglass-conn-test--fill b 4000 "b")
                  (spyglass-test-settle 0.05))
                (cancel-timer reader)
                (dolist (p (list (spyglass-connection-process client)
                                 (spyglass-connection-process server)))
                  (when (process-live-p p) (continue-process p)))
                (spyglass-test-converge a mirror-a 60)
                (spyglass-test-converge b mirror-b 60)
                (should (> received 0))
                (should-not spyglass-test-errors)
                (should-not states)
                (should (spyglass-connection-live-p client))))
          (cancel-timer pinger)
          (cancel-timer reader)
          (spyglass-unregister-channel "tst")
          (dolist (buffer (list a b))
            (when (buffer-live-p buffer)
              (ignore-errors (spyglass-unshare buffer))
              (kill-buffer buffer))))))))

(ert-deftest spyglass-conn-heartbeat-settings ()
  "Heartbeats take integers and floats, and refuse what makes no heartbeat.
A connection is looked at every half of the shorter of the interval
and what the timeout leaves after it: integers halved in integer
arithmetic gave 0 for 1, a timer that never repeated."
  (should (= (spyglass--heartbeat-period 1.0 4.0) 0.5))
  (should (= (spyglass--heartbeat-period 10.0 40.0) 5.0))
  (should (= (spyglass--heartbeat-period 3.0 4.0) 0.5))
  (spyglass-conn-test--with (client server)
    (spyglass-connection-set-heartbeat client 1 4)
    (should (eql (spyglass--heartbeat-interval client) 1.0))
    (should (eql (spyglass--heartbeat-timeout client) 4.0))
    (should (eql (timer--repeat-delay (spyglass-connection-timer client)) 0.5))
    (spyglass-connection-set-heartbeat client 3 nil)
    (should (eql (spyglass--heartbeat-timeout client) (float spyglass-heartbeat-timeout)))
    (should (eql (timer--repeat-delay (spyglass-connection-timer client)) 1.5))
    (dolist (bad '((0 4) (-1 4) (1 0) (4 4) (5 4) ("1" 4) (1 "4") (nil 5)))
      (ert-info ((format "%S" bad))
        (should-error (apply #'spyglass-connection-set-heartbeat client bad))))
    ;; Refused settings leave the last good ones.
    (should (eql (spyglass--heartbeat-interval client) 3.0))
    ;; Options set wrong give way to their standard values, and say so.
    (let ((spyglass-heartbeat-interval 50)
          (spyglass-test-errors nil))
      (should (equal (spyglass--heartbeat-settings server) '(10.0 . 40.0)))
      (spyglass--schedule-heartbeat server)
      (should (seq-some (lambda (m) (string-match-p "not below timeout" m)) spyglass-test-errors))
      (should (eql (timer--repeat-delay (spyglass-connection-timer server)) 5.0)))))

(defun spyglass-conn-test--start-peer (dir)
  "Start another batch Emacs serving spyglass, registered in DIR.
Return (PROCESS . PID): PROCESS runs it under `timeout', PID is that
Emacs's own, from its registration.  It exits by itself after a
minute."
  (let* ((file (expand-file-name "server.json" dir))
         (source (file-name-directory (locate-library "spyglass")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (form (format "(progn (spyglass-server-start :file %S) (let ((end (+ (float-time) 60))) (while (< (float-time) end) (accept-process-output nil 0.05))))"
                       file))
         (process (make-process
                   :name "spyglass-peer" :connection-type 'pipe :noquery t
                   :buffer (generate-new-buffer " *spyglass-peer*")
                   :command (list "sh" "-c"
                                  (concat "trap '' PIPE; exec timeout 90 "
                                          (mapconcat #'shell-quote-argument
                                                     (list emacs "-Q" "--batch" "-L" source
                                                           "--eval" "(setq load-prefer-newer t)"
                                                           "-l" "spyglass" "--eval" form)
                                                     " "))))))
    (spyglass-test-wait (lambda () (or (file-exists-p file) (not (process-live-p process)))) 30)
    (let ((pid (and (file-exists-p file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (plist-get (json-parse-string (buffer-string) :object-type 'plist) :pid)))))
      (unless pid
        (error "The peer did not start: %s"
               (with-current-buffer (process-buffer process) (buffer-string))))
      (cons process pid))))

(ert-deftest spyglass-conn-heartbeat-drops-frozen-peer ()
  "A peer stopped by SIGSTOP is dropped within the timeout, given in integers.
Its stream stays open to the system: only heartbeats tell.  With
`spyglass-connection-set-heartbeat' given 1 and 4, the connection was
looked at once, after (/ 1 2) seconds, and never again, so that a
frozen peer stayed connected for as long as it was stopped."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (skip-unless (executable-find "timeout"))
  (let* ((dir (make-temp-file "spyglass-peer-" t))
         (peer (spyglass-conn-test--start-peer dir))
         (pid (cdr peer))
         (spyglass-test-errors nil)
         (connection nil)
         (stopped nil))
    (unwind-protect
        (progn
          (setq connection (spyglass-connect (spyglass-file-transport
                                              (expand-file-name "server.json" dir))
                                             :name "frozen"))
          (spyglass-test-wait (lambda () (spyglass-connection-live-p connection)) 20)
          (should (spyglass-connection-live-p connection))
          (spyglass-connection-set-heartbeat connection 1 4)
          (should (eql (timer--repeat-delay (spyglass-connection-timer connection)) 0.5))
          ;; Heartbeats keep a peer that is silent but running.
          (spyglass-test-settle 3)
          (should (spyglass-connection-live-p connection))
          (signal-process pid 'SIGSTOP)
          (setq stopped t)
          (let ((start (float-time)))
            (spyglass-test-wait (lambda () (not (spyglass-connection-live-p connection))) 15)
            (should-not (spyglass-connection-live-p connection))
            (let ((elapsed (- (float-time) start)))
              ;; The timeout and a look past it; not before the peer
              ;; was silent long enough, heard last at most a heartbeat
              ;; and a look before it stopped.
              (should (< elapsed 5.5))
              (should (> elapsed 2.0))))
          (should (seq-some (lambda (m) (string-match-p "No word from frozen for 4s" m))
                            spyglass-test-errors))
          (signal-process pid 'SIGCONT)
          (setq stopped nil)
          (spyglass-test-wait (lambda () (spyglass-connection-live-p connection)) 20)
          (should (spyglass-connection-live-p connection)))
      (when stopped (signal-process pid 'SIGCONT))
      (when connection (spyglass-disconnect connection))
      (signal-process pid 'SIGTERM)
      (spyglass-test-wait (lambda () (not (process-live-p (car peer)))) 10)
      (let ((buffer (process-buffer (car peer))))
        (when (process-live-p (car peer)) (delete-process (car peer)))
        (kill-buffer buffer))
      (delete-directory dir t))))

(provide 'spyglass-conn-test)
;;; spyglass-conn-test.el ends here
