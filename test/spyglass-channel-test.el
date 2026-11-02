;;; spyglass-channel-test.el --- Channels of messages between peers  -*- lexical-binding: t; -*-

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

;; This Emacs talks to itself through 127.0.0.1 on a channel: messages
;; small and chunked, requests answered, refused and lost, the hooks of
;; streams coming and going, limits, and heartbeats of a connection's
;; own.

;;; Code:

(require 'spyglass-test-helpers)

(defvar spyglass-channel-test--received nil
  "Messages the test channel received, newest first, as (CONNECTION . MESSAGE).")

(defvar spyglass-channel-test--events nil
  "Attach and detach events of the test channel, newest first.")

(defmacro spyglass-channel-test--with (vars &rest body)
  "Run BODY with VARS, (CLIENT SERVER), the two ends of a connection to itself.
The channel \"test\" is registered meanwhile, its messages going to
`spyglass-channel-test--received', its requests answered by
`spyglass-channel-test--answer'."
  (declare (indent 1) (debug (sexp body)))
  `(let ((spyglass-channel-test--received nil)
         (spyglass-channel-test--events nil)
         (spyglass-test-errors nil))
     (spyglass-register-channel
      "test" (lambda (c m) (push (cons c m) spyglass-channel-test--received))
      :request #'spyglass-channel-test--answer
      :attach (lambda (c) (push (list 'attach c) spyglass-channel-test--events))
      :detach (lambda (c final) (push (list 'detach c final) spyglass-channel-test--events)))
     (unwind-protect
         (spyglass-test-with-connection ,(car vars)
           (let ((,(cadr vars) nil))
             (spyglass-test-wait (lambda () (setq ,(cadr vars)
                                                  (spyglass-test-other-end ,(car vars)))))
             (should ,(cadr vars))
             ,@body))
       (spyglass-unregister-channel "test"))))

(defun spyglass-channel-test--answer (_connection message reply)
  "Answer MESSAGE, a request of the test channel, through REPLY."
  (pcase (plist-get message :op)
    ("echo" (funcall reply (list :echo (plist-get message :v))))
    ("fail" (funcall reply nil "refused on purpose"))
    ("signal" (error "Broken on purpose"))
    ("later" (run-at-time 0.05 nil (lambda () (funcall reply (list :late t)))))
    ("never" nil)
    ("empty" (funcall reply nil))))

(defun spyglass-channel-test--wait-messages (count)
  "Wait until the test channel received COUNT messages; return them, oldest first."
  (spyglass-test-wait (lambda () (>= (length spyglass-channel-test--received) count)) 20)
  (reverse spyglass-channel-test--received))

(ert-deftest spyglass-channel-messages-both-ways ()
  "A message reaches the other end of a connection, either way, as data."
  (spyglass-channel-test--with (client server)
    (should (spyglass-channel-send client "test" '(:t "hello" :n 1 :list [1 "two" nil :false]
                                                    :nested (:a (:b 2)))))
    (should (spyglass-channel-send server "test" '(:t "back")))
    (let ((received (spyglass-channel-test--wait-messages 2)))
      (should (equal (mapcar #'cdr (seq-sort-by (lambda (r) (plist-get (cdr r) :t)) #'string< received))
                     '((:t "back")
                       (:t "hello" :n 1 :list [1 "two" nil :false] :nested (:a (:b 2))))))
      (should (eq (car (cl-find "back" received :key (lambda (r) (plist-get (cdr r) :t))
                                :test #'equal))
                  client))
      (should (eq (car (cl-find "hello" received :key (lambda (r) (plist-get (cdr r) :t))
                                :test #'equal))
                  server)))))

(ert-deftest spyglass-channel-strings-made-safe ()
  "Text JSON cannot carry is replaced, and the rest arrives unchanged."
  (spyglass-channel-test--with (client _server)
    (let ((text (concat "é😀\0\r\n" (string #x3fff80) (string #xd800) "end")))
      (should (spyglass-channel-send client "test" (list :s text :v (vector text))))
      (let ((message (cdar (spyglass-channel-test--wait-messages 1)))
            (safe (concat "é😀\0\r\n" (string #xfffd) (string #xfffd) "end")))
        (should (equal (plist-get message :s) safe))
        (should (equal (plist-get message :v) (vector safe)))))
    (should-error (spyglass-channel-encode (list :f (lambda ()))))))

(ert-deftest spyglass-channel-chunks-in-order ()
  "A large message goes in chunks, within the window, and keeps its place."
  (spyglass-channel-test--with (client _server)
    (let* ((spyglass-channel-chunk-size 65536)
           (spyglass-channel-window 200000)
           (big (make-string 1000000 ?x))
           (peak 0)
           (watch (run-at-time 0 0.001 (lambda ()
                                         (setq peak (max peak (plist-get (spyglass-channel-stats client)
                                                                         :inflight)))))))
      (unwind-protect
          (progn
            (should (spyglass-channel-send client "test" '(:t "before")))
            (should (spyglass-channel-send client "test" (list :t "big" :s big)))
            (should (spyglass-channel-send client "test" '(:t "after")))
            (let ((received (spyglass-channel-test--wait-messages 3)))
              (should (equal (mapcar (lambda (r) (plist-get (cdr r) :t)) received)
                             '("before" "big" "after")))
              (should (equal (plist-get (cdr (nth 1 received)) :s) big))))
        (cancel-timer watch))
      (should (<= peak 200000))
      (should (>= (plist-get (spyglass-channel-stats client) :chunks-out) 16))
      (spyglass-test-wait (lambda () (zerop (plist-get (spyglass-channel-stats client) :inflight))))
      (should (zerop (plist-get (spyglass-channel-stats client) :inflight))))))

(ert-deftest spyglass-channel-chunks-both-ways-at-once ()
  "Both ends sending large messages at once each get the other's."
  (spyglass-channel-test--with (client server)
    (let ((spyglass-channel-chunk-size 32768)
          (spyglass-channel-window 65536)
          (a (make-string 400000 ?a))
          (b (make-string 400000 ?b)))
      (should (spyglass-channel-send client "test" (list :s a)))
      (should (spyglass-channel-send server "test" (list :s b)))
      (let ((received (spyglass-channel-test--wait-messages 2)))
        (should (equal (sort (mapcar (lambda (r) (plist-get (cdr r) :s)) received) #'string<)
                       (list a b)))))))

(ert-deftest spyglass-channel-requests ()
  "Requests are answered, refused, failed, timed out, and fail without a channel."
  (spyglass-channel-test--with (client _server)
    (let ((answers nil))
      (cl-flet ((ask (message &optional timeout channel)
                  (spyglass-channel-request client (or channel "test") message
                                            (lambda (reply error)
                                              (push (list (plist-get message :op) reply error) answers))
                                            timeout)))
        (ask '(:op "echo" :v [1 2]))
        (ask '(:op "fail"))
        (ask '(:op "signal"))
        (ask '(:op "later"))
        (ask '(:op "never") 0.3)
        (ask '(:op "empty"))
        (ask '(:op "nobody") nil "nobody")
        (spyglass-test-wait (lambda () (>= (length answers) 7)) 10)
        (should (equal (assoc "echo" answers) '("echo" (:echo [1 2]) nil)))
        (should (equal (assoc "fail" answers) '("fail" nil "refused on purpose")))
        (should (equal (assoc "signal" answers) '("signal" nil "Broken on purpose")))
        (should (equal (assoc "later" answers) '("later" (:late t) nil)))
        (should (equal (assoc "never" answers) '("never" nil "no answer in time")))
        (should (equal (assoc "empty" answers) '("empty" nil nil)))
        (should (string-match-p "no channel" (nth 2 (assoc "nobody" answers))))))))

(ert-deftest spyglass-channel-request-lost-with-stream ()
  "A request whose stream ends fails at once; one on no stream fails at once too."
  (spyglass-channel-test--with (client _server)
    (let ((answer nil))
      (spyglass-channel-request client "test" '(:op "never")
                                (lambda (reply error) (setq answer (list :reply reply :error error))))
      (spyglass-connection-drop-stream client)
      (should (equal answer '(:reply nil :error "the connection ended")))
      (setq answer nil)
      (should-not (spyglass-channel-request client "test" '(:op "echo")
                                            (lambda (reply error)
                                              (setq answer (list :reply reply :error error)))))
      (should (equal answer '(:reply nil :error "the connection is not open"))))))

(ert-deftest spyglass-channel-attach-and-detach ()
  "Channels and hooks hear streams open, break and come back, and the end."
  (let ((public nil))
    (cl-flet ((attach (c) (push (list 'attach c) public))
              (detach (c final) (push (list 'detach c final) public)))
      (add-hook 'spyglass-connection-attach-functions #'attach)
      (add-hook 'spyglass-connection-detach-functions #'detach)
      (unwind-protect
          (spyglass-channel-test--with (client _server)
            (should (assoc 'attach spyglass-channel-test--events))
            (should (member (list 'attach client) spyglass-channel-test--events))
            (should (member (list 'attach client) public))
            (setq spyglass-channel-test--events nil)
            (spyglass-connection-drop-stream client)
            (should (member (list 'detach client nil) spyglass-channel-test--events))
            (spyglass-test-wait (lambda () (spyglass-connection-live-p client)))
            (should (member (list 'attach client) spyglass-channel-test--events))
            (spyglass-disconnect client)
            (should (member (list 'detach client t) spyglass-channel-test--events))
            (should (member (list 'detach client t) public)))
        (remove-hook 'spyglass-connection-attach-functions #'attach)
        (remove-hook 'spyglass-connection-detach-functions #'detach)))))

(ert-deftest spyglass-channel-needs-capability ()
  "Nothing goes on channels to a peer not announcing them, as one predating them."
  (let ((attached nil) (received nil) (answer nil))
    (spyglass-register-channel "test" (lambda (_c m) (push m received))
                               :attach (lambda (c) (push c attached)))
    (unwind-protect
        (let ((spyglass-wire-capabilities (remove "ch" spyglass-wire-capabilities)))
          (spyglass-test-with-connection client
            (should-not (spyglass-connection-capable-p client "ch"))
            (should-not (spyglass-channel-available-p client))
            (should-not attached)
            (should-not (spyglass-channel-send client "test" '(:t "lost")))
            (should-not (spyglass-channel-request client "test" '(:op "echo")
                                                  (lambda (reply error)
                                                    (setq answer (list reply error)))))
            (should (equal answer '(nil "the peer has no channels")))
            (spyglass-test-settle 0.1)
            (should-not received)
            (should-not (plist-get (spyglass-channel-stats client) :messages-out))))
      (spyglass-unregister-channel "test"))))

(ert-deftest spyglass-channel-limits ()
  "Messages too large are refused on both ends; malformed chunks are reported."
  (spyglass-channel-test--with (client server)
    (let ((spyglass-channel-max-size 1000))
      (should-not (spyglass-channel-send client "test" (list :s (make-string 2000 ?x))))
      (should (seq-some (lambda (m) (string-match-p "exceeds" m)) spyglass-test-errors))
      (setq spyglass-test-errors nil)
      ;; A peer announcing more than the limit in chunks.
      (let ((spyglass-channel-chunk-size 400))
        (dotimes (n 3)
          (spyglass--channel-receive server (list :t "ch" :ch "test" :k 7 :n n :of 3)
                                     (make-string 400 ?y))))
      (should (seq-some (lambda (m) (string-match-p "exceeds" m)) spyglass-test-errors))
      (should-not spyglass-channel-test--received)
      (spyglass--channel-receive server (list :t "ch" :ch "test" :k 8 :n 5 :of 2) "x")
      (should (seq-some (lambda (m) (string-match-p "Malformed chunk" m)) spyglass-test-errors))
      (should-not (gethash 8 (spyglass--link-partial (spyglass--link server)))))))

(ert-deftest spyglass-channel-unregistered-drops ()
  "A message for a channel not registered is dropped and counted."
  (spyglass-channel-test--with (client server)
    (should (spyglass-channel-send client "elsewhere" '(:t "lost")))
    (spyglass-test-wait (lambda () (plist-get (spyglass-channel-stats server) :unrouted)) 5)
    (should (eql (plist-get (spyglass-channel-stats server) :unrouted) 1))
    (should-not spyglass-channel-test--received)))

(ert-deftest spyglass-channel-heartbeat-of-a-connection ()
  "A connection's own heartbeat drops a silent peer sooner than the default."
  (spyglass-channel-test--with (client server)
    (spyglass-connection-set-heartbeat server 0.2 0.6)
    (should (= (spyglass--heartbeat-interval server) 0.2))
    (should (= (spyglass--heartbeat-timeout client) spyglass-heartbeat-timeout))
    ;; The client end stops reading, and so answering.
    (stop-process (spyglass-connection-process client))
    (let ((start (float-time)))
      (spyglass-test-wait (lambda () (not (spyglass-connection-live-p server))) 5)
      (should-not (spyglass-connection-live-p server))
      (should (< (- (float-time) start) 3)))
    (continue-process (spyglass-connection-process client))))

(provide 'spyglass-channel-test)
;;; spyglass-channel-test.el ends here
