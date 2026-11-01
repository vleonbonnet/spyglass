;;; spyglass-conn.el --- Connections between spyglass peers  -*- lexical-binding: t; -*-

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

;; A connection joins this Emacs to one peer over any byte stream: a
;; local TCP socket, or a pipe through `ssh -W 127.0.0.1:PORT HOST'.
;; Both ends are peers: either may share buffers and open the other's,
;; over the same connection.  The end that connects holds a transport,
;; a function making the stream (see `spyglass-connect'), and connects
;; again when the stream breaks; the end that listens, a server bound to
;; 127.0.0.1 (see `spyglass-server-start'), checks the token the other
;; end presents.
;;
;; The handshake: each end sends `hello' with the protocol version and
;; an identifier of its Emacs instance, the connecting end the token
;; too.  Then requests (`req'/`res'), the owner's messages (`open',
;; `close', `ack', `in', `fl' to it; `u', `closed' from it), heartbeats
;; (`ping'/`pong', which also estimate the clock offset) and `bye'.
;; Handlers for message types are registered in `spyglass--handlers'
;; by the owner and mirror modules.
;;
;; Nothing here signals into a filter, sentinel or timer: errors go to
;; `spyglass-error-functions' and the connection's own callback.

;;; Code:

(require 'cl-lib)
(require 'spyglass-wire)

(defcustom spyglass-heartbeat-interval 10.0
  "Seconds of silence after which a connection sends a heartbeat."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-heartbeat-timeout 40.0
  "Seconds without hearing from a peer after which its connection is dropped.
A connection made through a transport then connects again."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-reconnect-delays '(0.2 1 2 5 10 30)
  "Seconds to wait before each attempt to connect again.
After the last, attempts repeat at its delay until
`spyglass-disconnect'."
  :group 'spyglass
  :type '(repeat number))

(defcustom spyglass-handshake-timeout 20.0
  "Seconds a new stream may take to complete the handshake."
  :group 'spyglass
  :type 'number)

(defvar spyglass-instance-id
  (substring (spyglass-wire-random-token) 0 24)
  "Identifier of this Emacs instance, fresh each session.
Owners key subscriptions by it, so that a viewer connecting again
resumes its mirrors.")

(defcustom spyglass-instance-name nil
  "Name this Emacs gives itself to peers, or nil for `system-name'."
  :group 'spyglass
  :type '(choice (const nil) string))

(defvar spyglass-error-functions nil
  "Abnormal hook run with (CONNECTION MESSAGE) on every error.
CONNECTION may be nil for errors outside connections.")

(defvar spyglass-connection-state-functions nil
  "Abnormal hook run with (CONNECTION STATE) when a connection changes state.
STATE is one of `connecting', `open', `reconnecting' and `closed'.")

(defvar spyglass-shares-changed-functions nil
  "Abnormal hook run with (CONNECTION SHARES) when a peer's shares change.
SHARES is the list returned by `spyglass-connection-shares'.")

(cl-defstruct (spyglass-connection (:constructor spyglass--make-connection)
                                   (:copier nil))
  "A connection to a peer, which outlives its streams."
  (name nil :documentation "Label given by the user, such as a host name.")
  (transport nil :documentation "Function making a stream, or nil for an accepted one.")
  (process nil :documentation "The current stream.")
  (state 'connecting :documentation "connecting, open, reconnecting or closed.")
  (peer-id nil :documentation "The peer's `spyglass-instance-id'.")
  (peer-name nil :documentation "The name the peer gave.")
  (reader nil :documentation "Buffer of bytes received.")
  (token nil :documentation "Token presented when connecting.")
  (server nil :documentation "Server that accepted the stream, if any.")
  (authenticated nil :documentation "Non-nil once the handshake checked out.")
  (next-rid 0 :documentation "Last request identifier.")
  (requests nil :documentation "Alist of request identifiers and callbacks.")
  (shares nil :documentation "The peer's shares, as last announced.")
  (mirrors nil :documentation "Hash table of subscription identifiers and mirror buffers.")
  (next-sub 0 :documentation "Last subscription identifier.")
  (last-recv 0.0 :documentation "When the peer was last heard from.")
  (last-send 0.0 :documentation "When something was last sent.")
  (timer nil :documentation "Heartbeat or reconnection timer.")
  (attempts 0 :documentation "Connection attempts since the last success.")
  (clock-offset nil :documentation "Peer clock minus ours, in seconds, or nil.")
  (rtt nil :documentation "Lowest round trip measured, in seconds.")
  (callback nil :documentation "Function called with (CONNECTION EVENT DATA).")
  (closing nil :documentation "Non-nil when closed on purpose.")
  (plist nil :documentation "Free for callers."))

(defvar spyglass--connections nil
  "Live connections, newest first.")

(defvar spyglass--handlers (make-hash-table :test #'equal)
  "Hash table of message types and their handlers.
A handler is called with (CONNECTION HEADER PAYLOAD).")

(defun spyglass--register-handler (type function)
  "Handle messages of TYPE, a string, with FUNCTION."
  (puthash type function spyglass--handlers))

(defvar spyglass--attach-functions nil
  "Functions called with a connection once its handshake succeeded.")

(defvar spyglass--detach-functions nil
  "Functions called with a connection whose stream ended.
Called with (CONNECTION FINAL): FINAL non-nil when the connection
will not come back.")

;;;; Errors and callbacks

(defun spyglass--report (connection format-string &rest args)
  "Report an error on CONNECTION, formatted from FORMAT-STRING and ARGS.
Runs `spyglass-error-functions' and the connection's callback,
neither of which may signal back here."
  (let ((message (apply #'format-message format-string args)))
    (condition-case err
        (run-hook-with-args 'spyglass-error-functions connection message)
      (error (message "spyglass: error in spyglass-error-functions: %S" err)))
    (when connection
      (spyglass--callback connection 'error message))
    message))

(defun spyglass--callback (connection event data)
  "Call CONNECTION's callback with EVENT and DATA, catching its errors."
  (when-let* ((callback (spyglass-connection-callback connection)))
    (condition-case err
        (funcall callback connection event data)
      (error (message "spyglass: error in connection callback: %S" err)))))

(defmacro spyglass--guard (connection &rest body)
  "Run BODY, reporting any error on CONNECTION instead of signaling it."
  (declare (indent 1) (debug t))
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         (progn ,@body)
       (error (spyglass--report ,connection "%s" (error-message-string ,err))
              nil))))

(defun spyglass--set-state (connection state)
  "Set CONNECTION's STATE and tell those who listen."
  (unless (eq (spyglass-connection-state connection) state)
    (setf (spyglass-connection-state connection) state)
    (condition-case err
        (run-hook-with-args 'spyglass-connection-state-functions connection state)
      (error (message "spyglass: error in state hook: %S" err)))
    (spyglass--callback connection 'state state)))

;;;; Sending

(defun spyglass-connection-live-p (connection)
  "Return non-nil when CONNECTION is open, its handshake done."
  (and (eq (spyglass-connection-state connection) 'open)
       (process-live-p (spyglass-connection-process connection))))

(defun spyglass--send (connection header &optional payload)
  "Send HEADER and PAYLOAD to CONNECTION's peer.
Return the number of bytes sent, or nil when the stream is gone."
  (let ((process (spyglass-connection-process connection)))
    (when (process-live-p process)
      (let ((frame (spyglass-wire-frame header payload)))
        (spyglass--send-frame connection frame)))))

(defun spyglass--send-frame (connection frame &optional quiet)
  "Send FRAME, a unibyte string, on CONNECTION's stream.
Return its length, or nil when the stream is gone.  A failure is
reported unless QUIET."
  (let ((process (spyglass-connection-process connection)))
    (when (process-live-p process)
      (condition-case err
          (progn
            (process-send-string process frame)
            (setf (spyglass-connection-last-send connection) (float-time))
            (length frame))
        (error
         (unless quiet
           (spyglass--report connection "Sending failed: %s" (error-message-string err)))
         nil)))))

(defun spyglass--request (connection op callback &rest args)
  "Send request OP with ARGS, a plist, to CONNECTION's peer.
CALLBACK is called with the reply plist, or with nil when the
connection ends first."
  (let ((rid (cl-incf (spyglass-connection-next-rid connection))))
    (push (cons rid callback) (spyglass-connection-requests connection))
    (unless (spyglass--send connection (append (list :t "req" :rid rid :op op) args))
      (spyglass--answer connection rid nil))
    rid))

(defun spyglass--answer (connection rid reply)
  "Call the callback waiting for request RID of CONNECTION with REPLY."
  (when-let* ((cell (assq rid (spyglass-connection-requests connection))))
    (setf (spyglass-connection-requests connection)
          (delq cell (spyglass-connection-requests connection)))
    (condition-case err
        (funcall (cdr cell) reply)
      (error (spyglass--report connection "Error in reply callback: %S" err)))))

;;;; Receiving

(defun spyglass--filter (process chunk)
  "Feed CHUNK received on PROCESS to its connection."
  (let ((connection (process-get process 'spyglass-connection)))
    (when (and connection (eq process (spyglass-connection-process connection)))
      (setf (spyglass-connection-last-recv connection) (float-time))
      (let ((frames
             (condition-case err
                 (spyglass-wire-feed (spyglass-connection-reader connection) chunk
                                     (if (spyglass-connection-authenticated connection)
                                         spyglass-max-frame-size
                                       spyglass-wire-handshake-frame-size))
               (spyglass-wire-error
                (spyglass--report connection "Protocol error: %s" (cadr err))
                (spyglass--drop connection)
                nil))))
        ;; The handshake may move the stream to another connection.
        (dolist (frame frames)
          (let ((connection (process-get process 'spyglass-connection)))
            (when (and connection (eq process (spyglass-connection-process connection)))
              (spyglass--guard connection
                (spyglass--dispatch connection (car frame) (cdr frame))))))
        (let ((connection (process-get process 'spyglass-connection)))
          (when (and frames connection
                     (eq process (spyglass-connection-process connection)))
            (spyglass--guard connection
              (run-hook-with-args 'spyglass--after-input-functions connection))))))))

(defvar spyglass--after-input-functions nil
  "Functions called with a connection after a chunk's frames were handled.
The mirror module acknowledges updates there, once per chunk.")

(defun spyglass--dispatch (connection header payload)
  "Handle the message HEADER with PAYLOAD from CONNECTION's peer."
  (let ((type (plist-get header :t)))
    (cond
     ((not (spyglass-connection-authenticated connection))
      (spyglass--handshake connection header))
     ((equal type "ping")
      (spyglass--send connection (list :t "pong" :t0 (plist-get header :t0)
                                       :t1 (float-time))))
     ((equal type "pong") (spyglass--pong connection header))
     ((equal type "res")
      (spyglass--answer connection (plist-get header :rid) header))
     ((equal type "bye")
      (setf (spyglass-connection-closing connection) 'peer)
      (spyglass--drop connection))
     ((equal type "error")
      (spyglass--report connection "Peer error: %s" (plist-get header :msg)))
     (t
      (let ((handler (gethash type spyglass--handlers)))
        (if handler
            (funcall handler connection header payload)
          (spyglass--report connection "Unknown message type %S" type)))))))

;;;; Handshake

(defun spyglass--hello (connection)
  "Send CONNECTION's peer our hello."
  (spyglass--send connection
                  (append (list :t "hello" :proto spyglass-wire-protocol
                                :app "spyglass" :id spyglass-instance-id
                                :name (or spyglass-instance-name (system-name)))
                          (and (spyglass-connection-token connection)
                               (list :token (spyglass-connection-token connection))))))

(defun spyglass--handshake (connection header)
  "Check HEADER, the first message from CONNECTION's peer."
  (let* ((server (spyglass-connection-server connection))
         (expected (and server (spyglass-server-token server)))
         (proto (plist-get header :proto))
         (problem
          (cond
           ((not (equal (plist-get header :t) "hello")) "expected hello")
           ((not (equal (plist-get header :app) "spyglass")) "not a spyglass peer")
           ((not (integerp proto)) "no protocol version")
           ((/= proto spyglass-wire-protocol)
            (format "protocol %s, expected %s" proto spyglass-wire-protocol))
           ((and server
                 (not (spyglass-wire-token-equal (plist-get header :token) expected)))
            "authentication failed")
           ((not (stringp (plist-get header :id))) "no instance identifier"))))
    (if problem
        (progn
          ;; A server refuses the stream for good; a client tries again
          ;; later, the server having perhaps restarted with a new token.
          (if server
              (progn
                (spyglass--send connection (list :t "error" :msg problem))
                (spyglass--report connection "Handshake refused: %s" problem)
                (setf (spyglass-connection-closing connection) 'refused))
            (spyglass--report connection "Handshake failed: %s"
                              (if (equal (plist-get header :t) "error")
                                  (format "%s" (plist-get header :msg))
                                problem)))
          (spyglass--drop connection))
      (let ((peer-id (plist-get header :id))
            (name (plist-get header :name)))
        (setq connection (spyglass--adopt connection peer-id))
        (setf (spyglass-connection-authenticated connection) t
              (spyglass-connection-peer-id connection) peer-id
              (spyglass-connection-peer-name connection)
              (if (stringp name) name "peer")
              (spyglass-connection-attempts connection) 0)
        (when server
          (spyglass--hello connection))
        (spyglass--set-state connection 'open)
        (spyglass--ping connection)
        (spyglass--schedule-heartbeat connection)
        (dolist (f spyglass--attach-functions)
          (spyglass--guard connection (funcall f connection)))))))

(defun spyglass--adopt (connection peer-id)
  "Return the connection that should own CONNECTION's stream from PEER-ID.
An accepted stream from a peer whose earlier accepted connection is
waiting to come back is moved to that connection, so that its mirrors
resume."
  (let ((old (and (spyglass-connection-server connection)
                  (cl-find-if (lambda (c)
                                (and (not (eq c connection))
                                     (spyglass-connection-server c)
                                     (equal (spyglass-connection-peer-id c) peer-id)
                                     (memq (spyglass-connection-state c)
                                           '(reconnecting connecting))))
                              spyglass--connections))))
    (if (not old)
        connection
      (let ((process (spyglass-connection-process connection)))
        (process-put process 'spyglass-connection old)
        (spyglass-wire-free-reader (spyglass-connection-reader old))
        (setf (spyglass-connection-process old) process
              (spyglass-connection-reader old) (spyglass-connection-reader connection)
              (spyglass-connection-last-recv old) (float-time)
              (spyglass-connection-process connection) nil
              (spyglass-connection-reader connection) nil
              (spyglass-connection-closing connection) 'adopted)
        (setq spyglass--connections (delq connection spyglass--connections))
        (spyglass--cancel-timer connection)
        old))))

;;;; Heartbeats and clocks

(defun spyglass--ping (connection)
  "Send CONNECTION's peer a heartbeat stamped with our clock."
  (spyglass--send connection (list :t "ping" :t0 (float-time))))

(defun spyglass--pong (connection header)
  "Update CONNECTION's round trip and clock offset from pong HEADER."
  (let ((t0 (plist-get header :t0)) (t1 (plist-get header :t1))
        (t2 (float-time)))
    (when (and (numberp t0) (numberp t1))
      (let ((rtt (- t2 t0)))
        (when (or (null (spyglass-connection-rtt connection))
                  (<= rtt (spyglass-connection-rtt connection)))
          (setf (spyglass-connection-rtt connection) rtt
                (spyglass-connection-clock-offset connection)
                (- t1 (/ (+ t0 t2) 2))))))))

(defun spyglass--cancel-timer (connection)
  "Cancel CONNECTION's heartbeat or reconnection timer."
  (when (spyglass-connection-timer connection)
    (cancel-timer (spyglass-connection-timer connection))
    (setf (spyglass-connection-timer connection) nil)))

(defun spyglass--schedule-heartbeat (connection)
  "Check CONNECTION's liveness every few seconds."
  (spyglass--cancel-timer connection)
  (setf (spyglass-connection-timer connection)
        (run-with-timer (/ spyglass-heartbeat-interval 2) (/ spyglass-heartbeat-interval 2)
                        #'spyglass--heartbeat connection)))

(defun spyglass--heartbeat (connection)
  "Send a heartbeat on CONNECTION, or drop it when the peer went silent."
  (spyglass--guard connection
    (let ((now (float-time)))
      (cond
       ((not (eq (spyglass-connection-state connection) 'open))
        (spyglass--cancel-timer connection))
       ((> (- now (spyglass-connection-last-recv connection)) spyglass-heartbeat-timeout)
        (spyglass--report connection "No word from %s for %ds"
                          (spyglass-connection-label connection)
                          (round spyglass-heartbeat-timeout))
        (spyglass--drop connection))
       ((or (> (- now (spyglass-connection-last-send connection))
               spyglass-heartbeat-interval)
            (> (- now (spyglass-connection-last-recv connection))
               spyglass-heartbeat-interval))
        (spyglass--ping connection))))))

;;;; Streams

(defun spyglass--attach-process (connection process)
  "Make PROCESS CONNECTION's stream."
  (set-process-coding-system process 'binary 'binary)
  (set-process-query-on-exit-flag process nil)
  (set-process-filter process #'spyglass--filter)
  (set-process-sentinel process #'spyglass--sentinel)
  (process-put process 'spyglass-connection connection)
  (spyglass-wire-free-reader (spyglass-connection-reader connection))
  (setf (spyglass-connection-process connection) process
        (spyglass-connection-reader connection) (spyglass-wire-make-reader)
        (spyglass-connection-authenticated connection) nil
        (spyglass-connection-last-recv connection) (float-time)
        (spyglass-connection-rtt connection) nil))

(defun spyglass--sentinel (process _event)
  "Notice that PROCESS, a connection's stream, ended."
  (let ((connection (process-get process 'spyglass-connection)))
    (when (and connection (not (process-live-p process))
               (eq process (spyglass-connection-process connection)))
      (spyglass--guard connection
        (spyglass--lost connection)))))

(defun spyglass--drop (connection)
  "End CONNECTION's current stream; it comes back if it has a transport."
  (let ((process (spyglass-connection-process connection)))
    (when (process-live-p process)
      (process-put process 'spyglass-connection nil)
      (delete-process process))
    (when (eq process (spyglass-connection-process connection))
      (spyglass--lost connection))))

(defun spyglass--discard-process (process)
  "Delete PROCESS, a stream, and the sink of its standard error.
Return what it wrote to standard error, trimmed, or nil."
  (let* ((sink (process-get process 'spyglass-stderr))
         (buffer (and sink (process-buffer sink)))
         (text (and (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (string-trim (buffer-string))))))
    (process-put process 'spyglass-connection nil)
    (when (process-live-p process) (delete-process process))
    (when sink
      (when (process-live-p sink) (delete-process sink))
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (and text (not (string-empty-p text)) text)))

(defun spyglass--lost (connection)
  "Handle the end of CONNECTION's stream."
  (let ((process (spyglass-connection-process connection)))
    (when process
      (let ((stderr (spyglass--discard-process process)))
        (when stderr
          (spyglass--report connection "%s: %s"
                            (spyglass-connection-label connection) stderr))))
    (setf (spyglass-connection-process connection) nil
          (spyglass-connection-authenticated connection) nil)
    (spyglass-wire-free-reader (spyglass-connection-reader connection))
    (setf (spyglass-connection-reader connection) nil)
    (spyglass--cancel-timer connection)
    (let ((requests (spyglass-connection-requests connection)))
      (setf (spyglass-connection-requests connection) nil)
      (dolist (cell requests)
        (condition-case nil (funcall (cdr cell) nil) (error nil))))
    ;; A connection made through a transport keeps trying, even when the
    ;; first attempt failed; an accepted one waits for its peer to come
    ;; back, as an orphan the next accepted stream from it adopts.
    (cond
     ((and (spyglass-connection-transport connection)
           (not (spyglass-connection-closing connection)))
      (dolist (f spyglass--detach-functions)
        (spyglass--guard connection (funcall f connection nil)))
      (spyglass--set-state connection 'reconnecting)
      (spyglass--schedule-reconnect connection))
     ((and (spyglass-connection-server connection)
           (not (spyglass-connection-closing connection))
           (spyglass-connection-peer-id connection))
      (dolist (f spyglass--detach-functions)
        (spyglass--guard connection (funcall f connection nil)))
      (spyglass--set-state connection 'reconnecting)
      (setf (spyglass-connection-timer connection)
            (run-with-timer spyglass-heartbeat-timeout nil
                            #'spyglass--expire connection)))
     (t
      (spyglass--close-final connection)))))

(defun spyglass--expire (connection)
  "Close CONNECTION, an accepted one whose peer did not come back."
  (when (eq (spyglass-connection-state connection) 'reconnecting)
    (spyglass--close-final connection)))

(defun spyglass--close-final (connection)
  "Close CONNECTION for good."
  (spyglass--cancel-timer connection)
  (setq spyglass--connections (delq connection spyglass--connections))
  (dolist (f spyglass--detach-functions)
    (spyglass--guard connection (funcall f connection t)))
  (spyglass--set-state connection 'closed))

(defun spyglass--schedule-reconnect (connection)
  "Try to connect CONNECTION again after the next delay."
  (let* ((n (spyglass-connection-attempts connection))
         (delays spyglass-reconnect-delays)
         (delay (if (< n (length delays)) (nth n delays) (car (last delays)))))
    (spyglass--cancel-timer connection)
    (setf (spyglass-connection-timer connection)
          (run-with-timer delay nil #'spyglass--reconnect connection))))

(defun spyglass--reconnect (connection)
  "Make a new stream for CONNECTION through its transport."
  (setf (spyglass-connection-timer connection) nil)
  (unless (or (spyglass-connection-closing connection)
              (eq (spyglass-connection-state connection) 'closed))
    (cl-incf (spyglass-connection-attempts connection))
    (spyglass--open-stream connection)))

(defun spyglass--open-stream (connection)
  "Ask CONNECTION's transport for a stream, and start the handshake on it."
  (let ((transport (spyglass-connection-transport connection))
        (done nil))
    (condition-case err
        (funcall
         transport
         (lambda (process &optional token)
           (if done
               (when (processp process) (delete-process process))
             (setq done t)
             (spyglass--guard connection
               (cond
                ((or (spyglass-connection-closing connection)
                     (eq (spyglass-connection-state connection) 'closed))
                 (when (processp process) (delete-process process)))
                ((not (processp process))
                 (spyglass--report connection "Cannot connect to %s: %s"
                                   (spyglass-connection-label connection)
                                   (or token "no stream"))
                 (spyglass--set-state connection 'reconnecting)
                 (spyglass--schedule-reconnect connection))
                (t
                 (setf (spyglass-connection-token connection) token)
                 (spyglass--attach-process connection process)
                 (spyglass--hello connection)
                 (spyglass--watch-handshake connection process)))))))
      (error
       (setq done t)
       (spyglass--report connection "Transport failed: %s" (error-message-string err))
       (spyglass--set-state connection 'reconnecting)
       (spyglass--schedule-reconnect connection)))))

(defun spyglass--watch-handshake (connection process)
  "Drop PROCESS if CONNECTION's handshake on it does not finish in time."
  (run-with-timer spyglass-handshake-timeout nil
                  (lambda ()
                    (when (and (eq process (spyglass-connection-process connection))
                               (not (spyglass-connection-authenticated connection)))
                      (spyglass--report connection "Handshake timed out")
                      (spyglass--drop connection)))))

;;;; Public functions

(defun spyglass-connection-label (connection)
  "Return a short label for CONNECTION: its name, or its peer's."
  (or (spyglass-connection-name connection)
      (spyglass-connection-peer-name connection)
      "peer"))

(defun spyglass-connections ()
  "Return the live connections, newest first."
  spyglass--connections)

(cl-defun spyglass-connect (transport &key name callback plist)
  "Connect to a peer through TRANSPORT and return the connection.
TRANSPORT is a function called with one argument, a function DONE,
whenever a stream is needed: at first and after each loss.  It must
call DONE, synchronously or later, with a process yielding a byte
stream to the peer's spyglass server and the server's token, or with
nil and a message explaining the failure.  See
`spyglass-tcp-transport' and `spyglass-ssh-transport'.  The process
must not have received output before DONE is called.

NAME labels the connection.  CALLBACK, if non-nil, is called with
\(CONNECTION EVENT DATA) for events of this connection: `state' with
the new state, `error' with a message, `shares' with the peer's
shares.  PLIST is kept for the caller in
`spyglass-connection-plist'.

The connection is made asynchronously, and made again when the stream
breaks, until `spyglass-disconnect'.  Nothing here prompts."
  (let ((connection (spyglass--make-connection
                     :name name :transport transport :callback callback
                     :plist plist :mirrors (make-hash-table))))
    (push connection spyglass--connections)
    (spyglass--set-state connection 'connecting)
    (spyglass--open-stream connection)
    connection))

(defun spyglass-disconnect (connection)
  "Close CONNECTION for good, telling its peer.
Its mirrors close and the subscriptions of its peer end."
  (unless (eq (spyglass-connection-state connection) 'closed)
    (setf (spyglass-connection-closing connection) 'local)
    ;; The peer may be gone already; it needs no farewell then.
    (when (spyglass-connection-live-p connection)
      (spyglass--send-frame connection (spyglass-wire-frame (list :t "bye")) t))
    (let ((process (spyglass-connection-process connection)))
      (when (process-live-p process)
        (process-put process 'spyglass-connection nil)
        (delete-process process))
      (spyglass--lost connection))))

(defun spyglass-connection-drop-stream (connection)
  "Break CONNECTION's stream without telling the peer, as a network failure would.
A connection with a transport connects again.  Meant for tests."
  (spyglass--drop connection))

;;;; Server

(cl-defstruct (spyglass-server (:constructor spyglass--make-server)
                               (:copier nil))
  "A listener on 127.0.0.1."
  (process nil :documentation "The listening process.")
  (port nil :documentation "The port listened on.")
  (token nil :documentation "The token clients must present.")
  (file nil :documentation "The registration file written, or nil."))

(defvar spyglass--server nil
  "The server of this Emacs, or nil.")

(defcustom spyglass-registration-file
  (locate-user-emacs-file "spyglass/server.json")
  "File where the server writes its port and token for clients.
Readable by its owner only.  Clients on another host read it through
the same SSH connection they forward the port through."
  :group 'spyglass
  :type 'file)

(defun spyglass--write-registration (file port token)
  "Write FILE telling clients PORT and TOKEN, readable by its owner only."
  (let ((dir (file-name-directory (expand-file-name file))))
    (with-file-modes #o700 (make-directory dir t))
    (let ((temp (make-temp-name (expand-file-name ".spyglass-" dir)))
          (coding-system-for-write 'utf-8-unix))
      (with-file-modes #o600
        (write-region (concat (json-serialize
                               (list :proto spyglass-wire-protocol :port port
                                     :token token :pid (emacs-pid)
                                     :id spyglass-instance-id
                                     :name (or spyglass-instance-name (system-name))))
                              "\n")
                      nil temp nil 'silent))
      (rename-file temp file t))))

(cl-defun spyglass-server-start (&key (port 0) (file spyglass-registration-file))
  "Start listening on 127.0.0.1 and return the server.
PORT 0, the default, lets the system choose one.  The port and a
fresh random token are written to FILE, unless FILE is nil.  When a
server already runs, return it."
  (or (and spyglass--server
           (process-live-p (spyglass-server-process spyglass--server))
           spyglass--server)
      (let* ((token (spyglass-wire-random-token))
             (server (spyglass--make-server :token token))
             (process (make-network-process
                       :name "spyglass-server" :server t :host "127.0.0.1"
                       :service port :family 'ipv4 :coding 'binary :noquery t
                       :filter #'spyglass--filter :sentinel #'spyglass--sentinel
                       :log (lambda (_server client _message)
                              (spyglass--accept server client)))))
        (setf (spyglass-server-process server) process
              (spyglass-server-port server) (process-contact process :service))
        (when file
          (spyglass--write-registration file (spyglass-server-port server) token)
          (setf (spyglass-server-file server) file))
        (setq spyglass--server server))))

(defun spyglass--accept (server process)
  "Make a connection for PROCESS, accepted by SERVER."
  (let ((connection (spyglass--make-connection
                     :server server :mirrors (make-hash-table))))
    (push connection spyglass--connections)
    (spyglass--attach-process connection process)
    (spyglass--watch-handshake connection process)))

(defun spyglass-server-stop ()
  "Stop listening and remove the registration file.
Connections already accepted stay."
  (when spyglass--server
    (let ((process (spyglass-server-process spyglass--server))
          (file (spyglass-server-file spyglass--server)))
      (when (process-live-p process) (delete-process process))
      (when (and file (file-exists-p file))
        (ignore-errors (delete-file file))))
    (setq spyglass--server nil)))

(defun spyglass-server ()
  "Return the running server, or nil."
  (and spyglass--server
       (process-live-p (spyglass-server-process spyglass--server))
       spyglass--server))

;;;; Transports

(defun spyglass-tcp-transport (host port token)
  "Return a transport connecting to PORT on HOST with TOKEN.
HOST is usually \"127.0.0.1\", the server listening there only, or
the local end of a forwarded port."
  (lambda (done)
    (let ((process (condition-case err
                       (make-network-process
                        :name "spyglass" :host host :service port
                        :coding 'binary :noquery t :filter #'ignore)
                     (error (funcall done nil (error-message-string err)) nil))))
      (when process (funcall done process token)))))

(defun spyglass-parse-registration (text)
  "Return the port and token of registration TEXT as a cons, or nil."
  (let ((data (condition-case nil
                  (json-parse-string text :object-type 'plist)
                (error nil))))
    (when (and (consp data)
               (eql (plist-get data :proto) spyglass-wire-protocol)
               (natnump (plist-get data :port))
               (stringp (plist-get data :token)))
      (cons (plist-get data :port) (plist-get data :token)))))

(defun spyglass-file-transport (&optional file)
  "Return a transport to the server of this host registered in FILE.
FILE defaults to `spyglass-registration-file'; it is read again at
each connection, so that a restarted server is found."
  (lambda (done)
    (let ((reg (condition-case nil
                   (with-temp-buffer
                     (insert-file-contents (or file spyglass-registration-file))
                     (spyglass-parse-registration (buffer-string)))
                 (error nil))))
      (if (not reg)
          (funcall done nil "no registration file")
        (funcall (spyglass-tcp-transport "127.0.0.1" (car reg) (cdr reg)) done)))))

(defcustom spyglass-ssh-program "ssh"
  "Program run to reach other hosts, with OpenSSH's options."
  :group 'spyglass
  :type 'string)

(defcustom spyglass-remote-registration-file "~/.emacs.d/spyglass/server.json"
  "Registration file read on other hosts, as their shell names it."
  :group 'spyglass
  :type 'string)

(defun spyglass--stderr-sink (name)
  "Return a pipe process collecting a stream's standard error under NAME."
  (make-pipe-process :name (concat name "-stderr") :noquery t
                     :buffer (generate-new-buffer (format " *%s-stderr*" name) t)
                     :sentinel #'ignore))

(cl-defun spyglass-ssh-transport (host &key (program spyglass-ssh-program)
                                       (file spyglass-remote-registration-file)
                                       args)
  "Return a transport to the spyglass server of HOST through SSH.
At each connection, read the server's registration FILE on HOST with
`PROGRAM ARGS HOST cat FILE', then open a stream with `PROGRAM ARGS -W
127.0.0.1:PORT HOST', SSH forwarding standard input and output to the
port.  PROGRAM defaults to `spyglass-ssh-program'; ARGS are extra
options, such as (\"-p\" \"2222\").  FILE goes to the remote shell as
it is, so that it expands `~'; that shell must know `cat', as POSIX
shells and PowerShell do."
  (lambda (done)
    (let* ((out (generate-new-buffer " *spyglass-registration*" t))
           (sink (spyglass--stderr-sink "spyglass-registration"))
           (command (append (list program) args
                            (list "-o" "BatchMode=yes" host (concat "cat " file)))))
      (condition-case err
          (make-process
           :name "spyglass-registration" :buffer out :command command
           :coding 'utf-8 :noquery t :connection-type 'pipe :stderr sink
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let ((reg (and (eq (process-status process) 'exit)
                               (zerop (process-exit-status process))
                               (with-current-buffer out
                                 (spyglass-parse-registration (buffer-string))))))
                 (process-put process 'spyglass-stderr sink)
                 (let ((stderr (spyglass--discard-process process)))
                   (kill-buffer out)
                   (if (not reg)
                       (funcall done nil
                                (format "no spyglass server registered on %s%s" host
                                        (if stderr (concat ": " stderr) "")))
                     (spyglass--ssh-stream host program args (car reg) (cdr reg)
                                           done)))))))
        (error (kill-buffer out)
               (spyglass--discard-process sink)
               (funcall done nil (error-message-string err)))))))

(defun spyglass--ssh-stream (host program args port token done)
  "Call DONE with a stream to PORT on HOST through PROGRAM and TOKEN.
ARGS are extra options for PROGRAM."
  (let ((sink (spyglass--stderr-sink (format "spyglass-%s" host))))
    (condition-case err
        (let ((stream (make-process
                       :name (format "spyglass-%s" host)
                       :command (append (list program) args
                                        (list "-o" "BatchMode=yes" "-W"
                                              (format "127.0.0.1:%d" port) host))
                       :coding 'binary :noquery t :connection-type 'pipe
                       :filter #'ignore :sentinel #'ignore :stderr sink)))
          (process-put stream 'spyglass-stderr sink)
          (funcall done stream token))
      (error (spyglass--discard-process sink)
             (funcall done nil (error-message-string err))))))

(provide 'spyglass-conn)
;;; spyglass-conn.el ends here
