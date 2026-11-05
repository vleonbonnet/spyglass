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
;; The handshake: each end sends `hello' with the protocol version, an
;; identifier of its Emacs instance and the optional messages it
;; handles (`spyglass-wire-capabilities'), the connecting end the token
;; too.  Then requests (`req'/`res'), the owner's messages (`open',
;; `close', `ack', `in', `fl', `sz' to it; `u', `closed' from it),
;; heartbeats (`ping'/`pong', which also estimate the clock offset) and
;; `bye'.  An optional message goes only to peers that announced it.
;; A stream both of whose ends' writes block, as Emacs's sockets on
;; Windows, is paced: acknowledgments of what was read (`rd') and frames
;; in fragments (`fr') keep what neither end has read within a window.
;; Handlers for message types are registered in `spyglass--handlers'
;; by the owner, mirror, window and channel modules.
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

(defcustom spyglass-queue-limit (* 64 1024 1024)
  "Bytes of frames that may wait on a stream while a write on it waits.
A write waiting runs timers and filters.  Frames they send are queued,
to be written after the frame in progress (see `spyglass--send-frame');
frames received while handling others are queued, to be handled after
them (see `spyglass--filter').  A peer making either queue grow past
this sends without reading, and its stream is dropped.  Keep it above
`spyglass-window': resuming may queue that much."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-pace-window (* 64 1024)
  "Bytes written on a paced stream and not yet read by the peer, at most.
Emacs on Windows writes to sockets without reading meanwhile: a write
the system cannot buffer waits until the peer reads, running no timer
nor filter.  Two such ends writing to each other, as two Emacs on one
Windows host, or one Emacs talking to itself, would each wait for the
other forever.  A stream both of whose ends write so is paced: frames
go in fragments of a quarter of this, and the peer acknowledges what
it read.  Keep it well below what the system buffers between two
sockets of one host, about 190 KB on Windows 11.  A stream takes the
value it has when the stream is made."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-w32-pipe-size (* 4 1024 1024)
  "Bytes of the pipes between Emacs on Windows and SSH, for its streams.
A write a pipe cannot take makes Emacs wait 20 ms or more before trying
again (`send_process'), so that what a stream carries is bounded by
what its pipe holds; Windows's pipes hold 4 KB unless asked for more
\(`w32-pipe-buffer-size').  Channels keep what they send and the peer
has not acknowledged below it.  Applies to the streams of
`spyglass-ssh-transport' started after it is set."
  :group 'spyglass
  :type 'natnum)

(defvar spyglass--pace-streams nil
  "Non-nil makes every stream made meanwhile write as on Windows, paced.
For tests: pacing is otherwise for streams whose writes block.")

(defcustom spyglass-handshake-timeout 20.0
  "Seconds a new stream may take to complete the handshake."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-connect-timeout 20.0
  "Seconds a transport may take to make a stream, before the handshake.
Transports resolve host names, connect sockets and read registrations
without Emacs waiting for them; an attempt still going after this
fails, and the connection tries again later."
  :group 'spyglass
  :type 'number)

(defvar spyglass-instance-id
  (substring (spyglass-wire-random-token) 0 24)
  "Identifier of this Emacs instance, fresh each session.
Owners key subscriptions by it, so that a viewer connecting again
resumes its mirrors.")

(defcustom spyglass-instance-name nil
  "Name this Emacs gives itself to peers, or nil for function `system-name'."
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

;;;; References

;; Emacs prints a value whole wherever nothing bounds it: an error's
;; data, what `format' and `prin1-to-string' are given, a value
;; evaluated, the list of timers.  Without `print-circle', its printer
;; prints a structure again from every path reaching it: when each of
;; many structures leads back to what holds them all, as each pending
;; request's timer led to its connection and so to every other request,
;; each prints them all, and printing them grows with the square of
;; their number.  What leads back up, from structures, closures, timers
;; and the properties of processes, goes through references: uninterned
;; symbols whose value is the target, which print as their name.

(defsubst spyglass--ref (object)
  "Return a reference to OBJECT, which prints as a name alone, or nil for nil.
`spyglass--deref' returns OBJECT from it.  Each reference is new."
  (when object
    (let ((ref (make-symbol "spyglass-ref")))
      (set ref object)
      ref)))

(defsubst spyglass--deref (ref)
  "Return what REF, made by `spyglass--ref', leads to, or nil.
What a spyglass predating references stored, the object itself, is
returned as it is, so that a running Emacs loading this one keeps
its connections."
  (if (and ref (symbolp ref)) (symbol-value ref) ref))

(defun spyglass--run-referenced (function ref)
  "Call FUNCTION with what REF leads to, unless that is gone."
  (when-let* ((object (spyglass--deref ref)))
    (funcall function object)))

(defun spyglass--run-with-timer (delay repeat function object)
  "Call FUNCTION with OBJECT after DELAY seconds, then every REPEAT if non-nil.
Return the timer, which holds OBJECT through a reference (see
`spyglass--ref'), so that printing the timer prints a name."
  (run-with-timer delay repeat #'spyglass--run-referenced function (spyglass--ref object)))

(cl-defstruct (spyglass-connection (:constructor spyglass--make-connection)
                                   (:copier nil))
  "A connection to a peer, which outlives its streams."
  (name nil :documentation "Label given by the user, such as a host name.")
  (transport nil :documentation "Function making a stream, or nil for an accepted one.")
  (process nil :documentation "The current stream.")
  (state 'connecting :documentation "connecting, open, reconnecting or closed.")
  (peer-id nil :documentation "The peer's `spyglass-instance-id'.")
  (peer-name nil :documentation "The name the peer gave.")
  (peer-caps nil :documentation "Optional messages the peer handles, as strings.")
  (reader nil :documentation "Buffer of bytes received.")
  (token nil :documentation "Token presented when connecting.")
  (server nil :documentation "Server that accepted the stream, if any.")
  (authenticated nil :documentation "Non-nil once the handshake checked out.")
  (next-rid 0 :documentation "Last request identifier.")
  (requests nil :documentation "Alist of request identifiers and callbacks.")
  (shares nil :documentation "The peer's shares, as last announced.")
  (mirrors nil :documentation "Hash table of subscription identifiers and mirrors.")
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

;;;; Samples

(defun spyglass--ring (&optional size)
  "Return an empty ring of SIZE (4096 by default) numbers.
Its vector grows as samples come, up to SIZE, so that a ring holds,
and prints, no more numbers than it was given."
  (let ((size (or size 4096)))
    (vector (make-vector (min 16 size) 0.0) 0 0 size)))

(defun spyglass--ring-add (ring value)
  "Add VALUE to RING."
  (let* ((v (aref ring 0)) (i (aref ring 1))
         ;; A ring made before rings grew is full size already.
         (size (if (> (length ring) 3) (aref ring 3) (length v))))
    (when (= i (length v))
      (setq v (vconcat v (make-vector (min (length v) (- size (length v))) 0.0)))
      (aset ring 0 v))
    (aset v i value)
    (aset ring 1 (% (1+ i) size))
    (aset ring 2 (min size (1+ (aref ring 2))))))

(defun spyglass-ring-summary (ring)
  "Return the count, median, 99th percentile and maximum of RING, as a plist."
  (let* ((n (aref ring 2))
         (sorted (sort (cl-subseq (aref ring 0) 0 n) #'<)))
    (if (zerop n)
        (list :n 0)
      (list :n n
            :p50 (aref sorted (/ n 2))
            :p99 (aref sorted (min (1- n) (floor (* n 0.99))))
            :max (aref sorted (1- n))))))

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

(cl-defstruct (spyglass--queue (:constructor spyglass--make-queue)
                              (:copier nil))
  "Frames waiting on a stream while a write on it waits."
  (head nil :documentation "Items queued, oldest first, as (ITEM . SIZE).")
  (tail nil :documentation "The last cons of the queue.")
  (bytes 0 :documentation "Bytes queued."))

(defun spyglass--queue-push (connection queue item size what)
  "Queue ITEM, SIZE bytes, in QUEUE, one of CONNECTION's stream's.
WHAT says what the items wait for, for messages.  Return SIZE.  Past
`spyglass-queue-limit', the peer keeps sending without reading what
it is sent: drop the stream and return nil."
  (if (> (+ (spyglass--queue-bytes queue) size) spyglass-queue-limit)
      (progn
        (spyglass--report connection "%s sends without reading: %d bytes wait to be %s"
                          (spyglass-connection-label connection)
                          (+ (spyglass--queue-bytes queue) size) what)
        (spyglass--drop connection)
        nil)
    (let ((cell (list (cons item size))))
      (if (spyglass--queue-tail queue)
          (setcdr (spyglass--queue-tail queue) cell)
        (setf (spyglass--queue-head queue) cell))
      (setf (spyglass--queue-tail queue) cell)
      (cl-incf (spyglass--queue-bytes queue) size)
      size)))

(defun spyglass--queue-pop (queue)
  "Remove the oldest item of QUEUE and return it, or nil when empty."
  (let ((next (pop (spyglass--queue-head queue))))
    (when next
      (unless (spyglass--queue-head queue)
        (setf (spyglass--queue-tail queue) nil))
      (cl-decf (spyglass--queue-bytes queue) (cdr next))
      (car next))))

(defun spyglass--writing-p (connection)
  "Return non-nil while a frame is being written on CONNECTION's stream.
Frames sent meanwhile wait in a queue: see `spyglass--send-frame'."
  (let ((process (spyglass-connection-process connection)))
    (and process (process-get process 'spyglass-writer) t)))

(defun spyglass--send-frame (connection frame &optional quiet)
  "Send FRAME, a unibyte string or a list of them, on CONNECTION's stream.
Return its length, or nil when the stream is gone or the write failed.
A failure is reported unless QUIET.

Every frame goes through here, whole and in order.  A write the
stream cannot take at once makes Emacs wait inside
`process-send-string', running timers and process filters: a frame
they sent then would go between the parts of the frame being written.
It is queued instead, and written right after that frame by the same
call, not by a timer.  Senders that may send much, as owners'
updates and channels, rather wait while `spyglass--writing-p'."
  (let ((process (spyglass-connection-process connection)))
    (when (process-live-p process)
      (let ((size (if (stringp frame) (length frame) (apply #'+ (mapcar #'length frame))))
            (writer (process-get process 'spyglass-writer)))
        (if writer
            (spyglass--queue-push connection writer frame size "sent")
          (spyglass--write connection process frame size quiet))))))

(defun spyglass--write (connection process frame size quiet)
  "Write FRAME, SIZE bytes, on PROCESS, CONNECTION's stream; then its queue.
Return SIZE once FRAME is written, or nil when its write failed,
reported unless QUIET.  Frames queued meanwhile are written after it.
FRAME nil writes only the acknowledgment a paced stream owes, and what
is queued meanwhile.  A failed write may have left part of a frame on
the stream, which no later frame can follow: the stream is dropped,
with what was queued, and comes back as after any failure, unless the
connection is being closed."
  (let ((writer (spyglass--make-queue))
        (pace (process-get process 'spyglass-pace))
        (written nil))
    (process-put process 'spyglass-writer writer)
    (unwind-protect
        (condition-case err
            (let ((next frame))
              (spyglass--pace-ack process pace)
              (unless next
                (setq next (spyglass--queue-pop writer)))
              (while next
                (if (and pace (spyglass--pace-interval pace))
                    (spyglass--pace-frame connection process pace next)
                  (dolist (part (if (stringp next) (list next) next))
                    (spyglass--put process pace part)))
                (setf (spyglass-connection-last-send connection) (float-time))
                (unless written (setq written size))
                (spyglass--pace-ack process pace)
                (setq next (spyglass--queue-pop writer))))
          (error
           ;; A stream deleted on purpose meanwhile fails quietly.
           (when (eq process (spyglass-connection-process connection))
             (unless (and quiet (not written))
               (spyglass--report connection "Sending failed: %s" (error-message-string err)))
             (unless (spyglass-connection-closing connection)
               (spyglass--drop connection)))))
      (process-put process 'spyglass-writer nil))
    written))

;;;; Paced streams

;; Emacs on Windows makes every write to a socket block (`sys_write' in
;; w32.c): a write the system cannot buffer waits until the peer reads,
;; and Emacs runs no timer nor filter meanwhile, where elsewhere it
;; reads while it waits.  When both ends of a stream write so, each may
;; wait for the other to read, forever.  Such a stream is paced: each
;; end keeps the bytes it wrote and its peer has not read below
;; `spyglass-pace-window', waiting for the peer's acknowledgments
;; (`rd') with timers and filters running, as Emacs waits elsewhere;
;; frames larger than a quarter of it go in fragments (`fr'), between
;; which the acknowledgments the writer owes go too.  Both ends announce
;; in their hello that their writes block (`:pace', with how often to be
;; acknowledged); a stream is paced when both did, and neither end
;; pays for it otherwise.

(cl-defstruct (spyglass--pace (:constructor spyglass--make-pace)
                              (:copier nil))
  "What a stream counts of the bytes it carries, and its pacing."
  (written 0 :documentation "Bytes written.")
  (read 0 :documentation "Bytes read.")
  (acked 0 :documentation "Bytes written that the peer said it read.")
  (reported 0 :documentation "Bytes read that the peer was told of.")
  (window nil :documentation "Bytes written and not read, at most, when paced.")
  (interval nil :documentation "Bytes read after which to tell the peer, once paced.")
  (fragments nil :documentation "Reader of the frames arriving in fragments."))

(defun spyglass--writes-block-p (process)
  "Return non-nil when writing to PROCESS waits without reading.
So Emacs writes to sockets on Windows."
  (or spyglass--pace-streams
      (and (eq system-type 'windows-nt)
           (eq (process-type process) 'network))))

(defconst spyglass--pipe-capacity (* 64 1024)
  "Bytes a pipe holds elsewhere than on Windows: 64 KB on macOS and Linux.")

(defvar w32-pipe-buffer-size)

(defun spyglass--write-capacity (process)
  "Return the bytes PROCESS takes before a write to it waits, or nil.
Nil when it takes many: a socket, whose buffers the system sizes to
the traffic.  A pipe holds `spyglass--pipe-capacity', or on Windows
what it was made with (see `spyglass-w32-pipe-size'); a write it
cannot take makes Emacs wait 20 ms or more before trying again.  So
channels keep the bytes in flight below it (see spyglass-channel.el)."
  (when (eq (process-type process) 'real)
    (if (eq system-type 'windows-nt)
        (or (process-get process 'spyglass-pipe-size)
            (and (natnump w32-pipe-buffer-size) (> w32-pipe-buffer-size 0)
                 w32-pipe-buffer-size)
            4096)
      spyglass--pipe-capacity)))

(defun spyglass--put (process pace string)
  "Write STRING, part of a frame, on PROCESS, counting it in PACE."
  (process-send-string process string)
  (when pace
    (cl-incf (spyglass--pace-written pace) (length string))))

(defun spyglass--pace-ack (process pace)
  "Tell PROCESS's peer what was read, when due on its paced stream PACE.
Only between frames, by the writer of PROCESS."
  (let ((interval (and pace (spyglass--pace-interval pace))))
    (when (and interval
               (>= (- (spyglass--pace-read pace) (spyglass--pace-reported pace)) interval))
      (setf (spyglass--pace-reported pace) (spyglass--pace-read pace))
      (spyglass--put process pace
                     (spyglass-wire-frame (list :t "rd" :n (spyglass--pace-read pace)))))))

(defun spyglass--pace-acknowledge (connection process)
  "Tell the peer what was read on PROCESS, CONNECTION's paced stream, if due.
When a frame is being written, the writer tells it between frames,
unless it waits for the peer to read, between frames too."
  (let ((pace (process-get process 'spyglass-pace)))
    (when (and pace (spyglass--pace-interval pace))
      (cond
       ((not (process-get process 'spyglass-writer))
        (spyglass--write connection process nil 0 nil))
       ((process-get process 'spyglass-pace-waiting)
        ;; Not again while this write waits, if it does.
        (process-put process 'spyglass-pace-waiting nil)
        (unwind-protect
            (spyglass--pace-ack process pace)
          (process-put process 'spyglass-pace-waiting t)))))))

(defun spyglass--pace-wait (connection process pace size)
  "Wait until PROCESS, CONNECTION's paced stream PACE, may take SIZE bytes more.
Timers and filters run meanwhile, as when Emacs waits to write
elsewhere.  Signal an error when the stream ends first."
  (spyglass--pace-ack process pace)
  (while (let ((unread (- (spyglass--pace-written pace) (spyglass--pace-acked pace))))
           (and (> unread 0) (> (+ unread size) (spyglass--pace-window pace))))
    (unless (and (process-live-p process)
                 (eq process (spyglass-connection-process connection)))
      (error "The stream ended"))
    (process-put process 'spyglass-pace-waiting t)
    (unwind-protect
        ;; Waiting on PROCESS returns as soon as it reads, the
        ;; acknowledgment perhaps; waiting on no process would wait the
        ;; whole timeout.  Other processes are read meanwhile.
        (accept-process-output process 0.05)
      (process-put process 'spyglass-pace-waiting nil))
    (spyglass--pace-ack process pace)))

(defun spyglass--pace-frame (connection process pace frame)
  "Write FRAME, a string or a list of them, on PROCESS, CONNECTION's paced stream.
PACE is its pacing.  A frame larger than a quarter of the window goes
in fragments."
  (let* ((parts (if (stringp frame) (list frame) frame))
         (size (apply #'+ (mapcar #'length parts)))
         (piece (/ (spyglass--pace-window pace) 4)))
    (if (<= size piece)
        (progn
          (spyglass--pace-wait connection process pace size)
          (dolist (part parts)
            (spyglass--put process pace part)))
      (let ((pending nil) (filled 0))
        (cl-flet ((emit ()
                    (let ((fragment (spyglass-wire-frame
                                     '(:t "fr") (apply #'concat (nreverse pending)))))
                      (setq pending nil filled 0)
                      (spyglass--pace-wait connection process pace (length fragment))
                      (spyglass--put process pace fragment))))
          (dolist (part parts)
            (let ((start 0) (end (length part)))
              (while (< start end)
                (let ((take (min (- end start) (- piece filled))))
                  (push (substring part start (+ start take)) pending)
                  (setq filled (+ filled take)
                        start (+ start take))
                  (when (= filled piece)
                    (emit))))))
          (when pending
            (emit)))))))

(defun spyglass--pace-start (connection header)
  "Pace CONNECTION's stream if both ends' writes block.
HEADER is the peer's hello, which says whether its writes block."
  (let* ((process (spyglass-connection-process connection))
         (pace (process-get process 'spyglass-pace))
         (interval (plist-get header :pace)))
    (when (and pace (natnump interval)
               (spyglass-connection-capable-p connection "pc")
               (spyglass--writes-block-p process))
      ;; How often the peer asks to be told: within reason.
      (setf (spyglass--pace-interval pace)
            (min (max interval 1024) spyglass-max-frame-size)))))

(defun spyglass--pace-receive (connection header payload)
  "Handle HEADER and PAYLOAD from CONNECTION's peer if they pace the stream.
Return non-nil when they did: an acknowledgment, or a fragment, whose
frames, once whole, are handled in their turn."
  (let* ((type (plist-get header :t))
         (process (spyglass-connection-process connection))
         (pace (and process (process-get process 'spyglass-pace))))
    (cond
     ((not (and pace (member type '("rd" "fr")))) nil)
     ((equal type "rd")
      (spyglass--pace-read-by-peer pace (plist-get header :n))
      t)
     (t
      (let ((frames
             (condition-case err
                 (spyglass-wire-feed (or (spyglass--pace-fragments pace)
                                         (setf (spyglass--pace-fragments pace)
                                               (spyglass-wire-make-reader)))
                                     (or payload "") spyglass-max-frame-size)
               (spyglass-wire-error
                (spyglass--report connection "Protocol error: %s" (cadr err))
                (spyglass--drop connection)
                nil))))
        (dolist (frame frames)
          (when (eq process (spyglass-connection-process connection))
            (if (equal (plist-get (car frame) :t) "fr")
                (progn
                  (spyglass--report connection "Protocol error: a fragment within a fragment")
                  (spyglass--drop connection))
              (spyglass--guard connection
                (spyglass--dispatch connection (car frame) (cdr frame)))))))
      t))))

(defun spyglass--pace-read-by-peer (pace n)
  "Note that the peer of the stream PACE paces read N bytes of it."
  (when (natnump n)
    (setf (spyglass--pace-acked pace)
          (max (spyglass--pace-acked pace) (min n (spyglass--pace-written pace))))))

(defun spyglass--pace-take-acks (process frames)
  "Apply the acknowledgments among FRAMES, read on PROCESS; return the others.
They are applied as soon as read, never queued behind other frames: a
write waiting for them may be what those frames wait for."
  (let ((pace (process-get process 'spyglass-pace)))
    (if (not (and pace (spyglass--pace-interval pace)))
        frames
      (cl-remove-if (lambda (frame)
                      (when (equal (plist-get (car frame) :t) "rd")
                        (spyglass--pace-read-by-peer pace (plist-get (car frame) :n))
                        t))
                    frames))))

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

(defun spyglass--process-connection (process)
  "Return the connection whose stream PROCESS is, or nil.
The process holds it through a reference: see `spyglass--ref'."
  (spyglass--deref (process-get process 'spyglass-connection)))

(defun spyglass--filter (process chunk)
  "Feed CHUNK received on PROCESS to its connection.
Frames are handled in the order received.  A handler may write, and
a write waiting runs filters, this one too: frames it reads then are
queued, and handled after those read before, by the call handling
those."
  (let ((connection (spyglass--process-connection process)))
    (when (and connection (eq process (spyglass-connection-process connection)))
      (setf (spyglass-connection-last-recv connection) (float-time))
      (when-let* ((pace (process-get process 'spyglass-pace)))
        (cl-incf (spyglass--pace-read pace) (length chunk)))
      (let ((frames
             (condition-case err
                 (spyglass-wire-feed (spyglass-connection-reader connection) chunk
                                     (if (spyglass-connection-authenticated connection)
                                         spyglass-max-frame-size
                                       spyglass-wire-handshake-frame-size))
               (spyglass-wire-error
                (spyglass--report connection "Protocol error: %s" (cadr err))
                (spyglass--drop connection)
                nil)))
            (inbox (process-get process 'spyglass-inbox)))
        (when (spyglass-connection-authenticated connection)
          (setq frames (spyglass--pace-take-acks process frames)))
        (when (eq process (spyglass-connection-process connection))
          (spyglass--pace-acknowledge connection process))
        (cond
         ((null frames))
         (inbox (spyglass--queue-push connection inbox frames (length chunk) "handled"))
         (t (spyglass--handle process frames)))))))

(defun spyglass--handle (process frames)
  "Handle FRAMES received on PROCESS, then those queued meanwhile, in order."
  (let ((inbox (spyglass--make-queue)))
    (process-put process 'spyglass-inbox inbox)
    (unwind-protect
        (while frames
          (dolist (frame frames)
            ;; The handshake may move the stream to another connection.
            (let ((connection (spyglass--process-connection process)))
              (when (and connection (eq process (spyglass-connection-process connection)))
                (spyglass--guard connection
                  (spyglass--dispatch connection (car frame) (cdr frame))))))
          (let ((connection (spyglass--process-connection process)))
            (when (and connection (eq process (spyglass-connection-process connection)))
              (spyglass--guard connection
                (run-hook-with-args 'spyglass--after-input-functions connection))))
          (setq frames (spyglass--queue-pop inbox)))
      (process-put process 'spyglass-inbox nil))))

(defvar spyglass--after-input-functions nil
  "Functions called with a connection after a chunk's frames were handled.
The mirror module acknowledges updates there, once per chunk.")

(defun spyglass--dispatch (connection header payload)
  "Handle the message HEADER with PAYLOAD from CONNECTION's peer."
  (let ((type (plist-get header :t)))
    (cond
     ((not (spyglass-connection-authenticated connection))
      (spyglass--handshake connection header))
     ((spyglass--pace-receive connection header payload))
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
  "Send CONNECTION's peer our hello.
It says how often to acknowledge what the peer reads when our writes
on the stream block, for pacing it (see `spyglass--pace-start')."
  (let* ((process (spyglass-connection-process connection))
         (pace (and process (process-get process 'spyglass-pace))))
    (spyglass--send connection
                    (append (list :t "hello" :proto spyglass-wire-protocol
                                  :app "spyglass" :id spyglass-instance-id
                                  :name (or spyglass-instance-name (system-name))
                                  :caps (vconcat spyglass-wire-capabilities))
                            (and pace (spyglass--writes-block-p process)
                                 (list :pace (/ (spyglass--pace-window pace) 4)))
                            (and (spyglass-connection-token connection)
                                 (list :token (spyglass-connection-token connection)))))))

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
            (name (plist-get header :name))
            (caps (plist-get header :caps)))
        (setq connection (spyglass--adopt connection peer-id))
        ;; Transports may trust what made a stream that authenticated.
        (process-put (spyglass-connection-process connection) 'spyglass-authenticated t)
        (setf (spyglass-connection-authenticated connection) t
              (spyglass-connection-peer-id connection) peer-id
              (spyglass-connection-peer-name connection)
              (if (stringp name) name "peer")
              ;; Peers predating capabilities send none.
              (spyglass-connection-peer-caps connection)
              (and (vectorp caps) (cl-remove-if-not #'stringp (append caps nil)))
              (spyglass-connection-attempts connection) 0)
        (spyglass--pace-start connection header)
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
        (process-put process 'spyglass-connection (spyglass--ref old))
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

(defvar spyglass--heartbeats (make-hash-table :test #'eq :weakness 'key)
  "Heartbeat settings of connections, as (INTERVAL . TIMEOUT), by connection.")

(defun spyglass--heartbeat-problem (interval timeout)
  "Return why INTERVAL and TIMEOUT, in seconds, make no heartbeat, or nil.
Both must be positive numbers, and INTERVAL below TIMEOUT: a peer that
is silent until it is sent a heartbeat would be dropped first."
  (cond
   ((not (and (numberp interval) (> interval 0)))
    (format "interval %S is not a positive number" interval))
   ((not (and (numberp timeout) (> timeout 0)))
    (format "timeout %S is not a positive number" timeout))
   ((>= interval timeout)
    (format "interval %s is not below timeout %s" interval timeout))))

(defun spyglass-connection-set-heartbeat (connection interval timeout)
  "Have CONNECTION send heartbeats every INTERVAL seconds and drop after TIMEOUT.
They replace `spyglass-heartbeat-interval' and
`spyglass-heartbeat-timeout' for CONNECTION alone, for instance so that
an application notices a silent peer sooner on a local network.  Nil
for either keeps the default.  Both are numbers of seconds, integers
or floats, and INTERVAL must be below TIMEOUT: signal an error
otherwise."
  (when-let* ((problem (spyglass--heartbeat-problem
                        (or interval spyglass-heartbeat-interval)
                        (or timeout spyglass-heartbeat-timeout))))
    (error "Spyglass heartbeat of %s: %s" (spyglass-connection-label connection) problem))
  (puthash connection (cons (and interval (float interval)) (and timeout (float timeout)))
           spyglass--heartbeats)
  (when (eq (spyglass-connection-state connection) 'open)
    (spyglass--schedule-heartbeat connection)))

(defvar spyglass--resume-windows (make-hash-table :test #'eq :weakness 'key)
  "Seconds accepted connections wait for their peer to come back, by connection.")

(defun spyglass-connection-set-resume (connection seconds)
  "Have CONNECTION, once its stream ends, wait SECONDS for its peer to come back.
An accepted connection whose stream ended waits for the same peer
instance to connect again, which adopts its new stream and resumes its
mirrors, for `spyglass-heartbeat-timeout' seconds by default, then
closes for good.  A peer that comes and goes, as a phone closing its
streams when its application leaves the screen, may be given longer, so
that its mirrors resume rather than start over.  SECONDS nil restores
the default; otherwise it must be a positive number of seconds, else
signal an error."
  (unless (or (null seconds) (and (numberp seconds) (> seconds 0)))
    (error "Spyglass resume window of %s: %S is not a positive number"
           (spyglass-connection-label connection) seconds))
  (if seconds
      (puthash connection (float seconds) spyglass--resume-windows)
    (remhash connection spyglass--resume-windows)))

(defun spyglass--resume-window (connection)
  "Return the seconds CONNECTION waits for its peer once its stream ended."
  (or (gethash connection spyglass--resume-windows) spyglass-heartbeat-timeout))

(defun spyglass--heartbeat-settings (connection)
  "Return CONNECTION's heartbeat interval and timeout, (INTERVAL . TIMEOUT).
Seconds, as floats: those `spyglass-connection-set-heartbeat' gave,
else `spyglass-heartbeat-interval' and `spyglass-heartbeat-timeout'.
When these make no heartbeat (see `spyglass--heartbeat-problem'), as
when an option was set wrong, the options' standard values instead."
  (let* ((own (gethash connection spyglass--heartbeats))
         (interval (or (car own) spyglass-heartbeat-interval))
         (timeout (or (cdr own) spyglass-heartbeat-timeout)))
    (if (spyglass--heartbeat-problem interval timeout)
        (cons (float (eval (car (get 'spyglass-heartbeat-interval 'standard-value)) t))
              (float (eval (car (get 'spyglass-heartbeat-timeout 'standard-value)) t)))
      (cons (float interval) (float timeout)))))

(defun spyglass--heartbeat-interval (connection)
  "Return the seconds of silence after which CONNECTION sends a heartbeat."
  (car (spyglass--heartbeat-settings connection)))

(defun spyglass--heartbeat-timeout (connection)
  "Return the seconds without word after which CONNECTION is dropped."
  (cdr (spyglass--heartbeat-settings connection)))

(defun spyglass--heartbeat-period (interval timeout)
  "Return how often to look at a connection of INTERVAL and TIMEOUT, in seconds.
Half the shorter of INTERVAL and of what TIMEOUT leaves after it: a
heartbeat goes within half that after INTERVAL of silence, and its
answer has the other half to come before TIMEOUT; a silent peer is
dropped within that after TIMEOUT."
  (/ (min interval (- timeout interval)) 2.0))

(defun spyglass--schedule-heartbeat (connection)
  "Check CONNECTION's liveness every few seconds."
  (spyglass--cancel-timer connection)
  (let* ((own (gethash connection spyglass--heartbeats))
         (problem (spyglass--heartbeat-problem
                   (or (car own) spyglass-heartbeat-interval)
                   (or (cdr own) spyglass-heartbeat-timeout)))
         (settings (spyglass--heartbeat-settings connection))
         (period (spyglass--heartbeat-period (car settings) (cdr settings))))
    (when problem
      (spyglass--report connection "Heartbeat of %s: %s; using %gs and %gs"
                        (spyglass-connection-label connection) problem
                        (car settings) (cdr settings)))
    (setf (spyglass-connection-timer connection)
          (spyglass--run-with-timer period period #'spyglass--heartbeat connection))))

(defun spyglass--heartbeat (connection)
  "Send a heartbeat on CONNECTION, or drop it when the peer went silent."
  (spyglass--guard connection
    (let* ((now (float-time))
           (settings (spyglass--heartbeat-settings connection))
           (interval (car settings))
           (timeout (cdr settings)))
      (cond
       ((not (eq (spyglass-connection-state connection) 'open))
        (spyglass--cancel-timer connection))
       ((> (- now (spyglass-connection-last-recv connection)) timeout)
        (spyglass--report connection "No word from %s for %gs"
                          (spyglass-connection-label connection) timeout)
        (spyglass--drop connection))
       ((or (> (- now (spyglass-connection-last-send connection)) interval)
            (> (- now (spyglass-connection-last-recv connection)) interval))
        (spyglass--ping connection))))))

;;;; Streams

(defun spyglass--attach-process (connection process)
  "Make PROCESS CONNECTION's stream."
  (set-process-coding-system process 'binary 'binary)
  (set-process-query-on-exit-flag process nil)
  (set-process-filter process #'spyglass--filter)
  (set-process-sentinel process #'spyglass--sentinel)
  (process-put process 'spyglass-connection (spyglass--ref connection))
  (process-put process 'spyglass-pace
               (spyglass--make-pace :window (max 4096 spyglass-pace-window)))
  (spyglass-wire-free-reader (spyglass-connection-reader connection))
  (setf (spyglass-connection-process connection) process
        (spyglass-connection-reader connection) (spyglass-wire-make-reader)
        (spyglass-connection-authenticated connection) nil
        (spyglass-connection-last-recv connection) (float-time)
        (spyglass-connection-rtt connection) nil))

(defun spyglass--sentinel (process _event)
  "Notice that PROCESS, a connection's stream, ended."
  (let ((connection (spyglass--process-connection process)))
    (when (and connection (not (process-live-p process))
               (eq process (spyglass-connection-process connection)))
      (spyglass--guard connection
        (spyglass--lost connection)))))

(defvar spyglass--endings (make-hash-table :test #'eq :weakness 'key)
  "How the last stream of each connection ended, a line, by connection.")

(defun spyglass-connection-ending (connection)
  "Return how the last stream of CONNECTION ended, a line, or nil.
Such as \"exited with status 255\" for an SSH process, \"closed\" for a
socket the peer closed, or \"dropped by this end\" for a stream this
Emacs ended, its write having failed or its peer fallen silent; nil
while no stream of it ended."
  (gethash connection spyglass--endings))

(defun spyglass--process-ending (process)
  "Return how PROCESS, a stream that ended, ended, a line."
  (cond ((process-get process 'spyglass-dropped) "dropped by this end")
        (t (pcase (process-status process)
             ('exit (format "exited with status %d" (process-exit-status process)))
             ('signal (format "killed by signal %d" (process-exit-status process)))
             ('failed "failed")
             (_ "closed")))))

(defun spyglass--drop (connection)
  "End CONNECTION's current stream; it comes back if it has a transport."
  (let ((process (spyglass-connection-process connection)))
    (when (process-live-p process)
      (process-put process 'spyglass-dropped t)
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
    (when-let* ((pace (process-get process 'spyglass-pace)))
      (spyglass-wire-free-reader (spyglass--pace-fragments pace))
      (setf (spyglass--pace-fragments pace) nil))
    (when sink
      (when (process-live-p sink) (delete-process sink))
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (and text (not (string-empty-p text)) text)))

(defun spyglass--lost (connection)
  "Handle the end of CONNECTION's stream."
  (let ((process (spyglass-connection-process connection)))
    (when process
      (puthash connection (spyglass--process-ending process) spyglass--endings)
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
            (spyglass--run-with-timer (spyglass--resume-window connection) nil
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

(defvar spyglass--reconnect-delays (make-hash-table :test #'eq :weakness 'key)
  "Delays of reconnection of connections given their own, by connection.")

(defun spyglass--delays-problem (delays)
  "Return why DELAYS, a list of seconds, cannot space attempts, or nil."
  (unless (and (consp delays) (proper-list-p delays)
               (cl-every (lambda (delay) (and (numberp delay) (>= delay 0) (< delay 1.0e+INF)))
                         delays)
               (> (car (last delays)) 0))
    "a list of seconds, none negative, the last positive, is expected"))

(defun spyglass-connection-set-reconnect-delays (connection delays)
  "Have CONNECTION wait DELAYS, seconds, before each attempt to connect again.
They replace `spyglass-reconnect-delays' for CONNECTION alone: the Nth
attempt after a stream broke waits the Nth delay, attempts after the
last repeating at its delay, as an application backing off a peer that
stays unreachable needs.  Nil keeps the default.  Signal an error when
DELAYS is not a list of numbers, none negative, the last positive.  An
attempt already scheduled keeps its time."
  (when-let* ((problem (and delays (spyglass--delays-problem delays))))
    (error "Spyglass reconnection of %s: %s" (spyglass-connection-label connection) problem))
  (if delays
      (puthash connection (copy-sequence delays) spyglass--reconnect-delays)
    (remhash connection spyglass--reconnect-delays)))

(defun spyglass--connection-delays (connection)
  "Return the delays of CONNECTION's attempts to connect again.
Those `spyglass-connection-set-reconnect-delays' gave, else
`spyglass-reconnect-delays', else, when that option was set wrong, its
standard value."
  (or (gethash connection spyglass--reconnect-delays)
      (and (not (spyglass--delays-problem spyglass-reconnect-delays)) spyglass-reconnect-delays)
      (eval (car (get 'spyglass-reconnect-delays 'standard-value)) t)))

(defun spyglass-connection-next-attempt (connection)
  "Return the seconds before CONNECTION next tries to connect, or nil.
Nil while it is connected, connecting, accepted (its peer connects
again), or closed."
  (let ((timer (spyglass-connection-timer connection)))
    (and (eq (spyglass-connection-state connection) 'reconnecting)
         (spyglass-connection-transport connection)
         (not (spyglass-connection-process connection))
         (timerp timer)
         (max 0.0 (float-time (time-subtract (timer--time timer) nil))))))

(defun spyglass--schedule-reconnect (connection)
  "Try to connect CONNECTION again after the next delay."
  (let* ((n (spyglass-connection-attempts connection))
         (delays (spyglass--connection-delays connection))
         (delay (if (< n (length delays)) (nth n delays) (car (last delays)))))
    (spyglass--cancel-timer connection)
    (setf (spyglass-connection-timer connection)
          (spyglass--run-with-timer delay nil #'spyglass--reconnect connection))))

(defun spyglass--reconnect (connection)
  "Make a new stream for CONNECTION through its transport."
  (setf (spyglass-connection-timer connection) nil)
  (unless (or (spyglass-connection-closing connection)
              (eq (spyglass-connection-state connection) 'closed))
    (cl-incf (spyglass-connection-attempts connection))
    (spyglass--open-stream connection)))

(defun spyglass--open-stream (connection)
  "Ask CONNECTION's transport for a stream, and start the handshake on it.
The function the transport is given, which a process of its may hold
until it calls it, holds CONNECTION through a reference."
  (let ((transport (spyglass-connection-transport connection))
        (ref (spyglass--ref connection))
        (done nil))
    (condition-case err
        (funcall
         transport
         (lambda (process &optional token)
           (if done
               (when (processp process) (delete-process process))
             (setq done t)
             (let ((connection (spyglass--deref ref)))
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
                   (spyglass--watch-handshake connection process))))))))
      (error
       (setq done t)
       (spyglass--report connection "Transport failed: %s" (error-message-string err))
       (spyglass--set-state connection 'reconnecting)
       (spyglass--schedule-reconnect connection)))))

(defun spyglass--watch-handshake (connection process)
  "Drop PROCESS if CONNECTION's handshake on it does not finish in time."
  (let ((ref (spyglass--ref connection)))
    (run-with-timer spyglass-handshake-timeout nil
                    (lambda ()
                      (let ((connection (spyglass--deref ref)))
                        (when (and (eq process (spyglass-connection-process connection))
                                   (not (spyglass-connection-authenticated connection)))
                          (spyglass--report connection "Handshake timed out")
                          (spyglass--drop connection)))))))

;;;; Public functions

(defun spyglass-connection-label (connection)
  "Return a short label for CONNECTION: its name, or its peer's."
  (or (spyglass-connection-name connection)
      (spyglass-connection-peer-name connection)
      "peer"))

(defun spyglass-connection-capable-p (connection capability)
  "Return non-nil when CONNECTION's peer announced CAPABILITY, a string.
See `spyglass-wire-capabilities'."
  (and (member capability (spyglass-connection-peer-caps connection)) t))

(defun spyglass-connections ()
  "Return the live connections, newest first."
  spyglass--connections)

(cl-defun spyglass-connect (transport &key name callback plist)
  "Connect to a peer through TRANSPORT and return the connection.
TRANSPORT is a function called with one argument, a function DONE,
whenever a stream is needed: at first and after each loss.  It must
call DONE, synchronously or later, with a process yielding a byte
stream to the peer's spyglass server and the server's token, or with
nil and a message explaining the failure.  It should not have Emacs
wait meanwhile, for the network nor for another host.  See
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

(defun spyglass-reconnect (connection &optional fresh)
  "Break CONNECTION's stream and have it come back, as after a failure.
For applications finding the peer unresponsive, a request timing out
for instance: mirrors, subscriptions and channels resume on the next
stream as after a network failure; the peer is not told.  A
connection made through a transport connects again, at once if it was
waiting to; one the server accepted waits for its peer, which
connects again when it notices the broken stream.  A failed write
drops the stream by itself.  With FRESH, the delays of the attempts
that may follow start over, as for a stream that just broke: for an
application that has reason to think its peer is back, the network
having changed for instance.  Return non-nil when a stream was broken
or an attempt to connect is under way, nil when CONNECTION is closed
or waits for its peer."
  (let ((process (spyglass-connection-process connection))
        (transport (spyglass-connection-transport connection)))
    (when fresh
      (setf (spyglass-connection-attempts connection) 0))
    (cond
     ((eq (spyglass-connection-state connection) 'closed) nil)
     ((process-live-p process)
      (spyglass--drop connection)
      t)
     ((not transport) nil)
     ((spyglass-connection-timer connection)
      (spyglass--cancel-timer connection)
      (spyglass--reconnect connection)
      t)
     ;; The transport is making a stream already.
     (t t))))

(defun spyglass-connection-drop-stream (connection)
  "Break CONNECTION's stream without telling the peer, as a network failure would.
A connection with a transport connects again.  Meant for tests;
applications call `spyglass-reconnect'."
  (spyglass--drop connection))

;;;; Server

(defun spyglass--nodelay (process)
  "Disable Nagle's algorithm on PROCESS, a socket.
Updates and acknowledgments are small and answer each other: Nagle's
algorithm and delayed acknowledgments would hold them back tens of
milliseconds."
  (ignore-errors (set-network-process-option process :nodelay t)))

(cl-defstruct (spyglass-server (:constructor spyglass--make-server)
                               (:copier nil))
  "A listener on 127.0.0.1."
  (process nil :documentation "The listening process.")
  (port nil :documentation "The port listened on.")
  (token nil :documentation "The token clients must present.")
  (file nil :documentation "The registration file written, or nil."))

(defvar spyglass--server nil
  "The server of this Emacs, or nil.")

(defvar spyglass--server-stopped nil
  "When a server last stopped listening on Windows, as a float time, or nil.")

(defconst spyglass--listener-linger 0.6
  "Seconds after which a listener deleted on Windows leaves its descriptor alone.
Emacs on Windows waits for connections in a thread per listener, which
looks every 500 ms whether its listener still lives (`_sys_wait_accept'
in w32.c).  Until it does, a listener made meanwhile, given the same
descriptor, accepts one connection, then none: the old thread cancels
its notifications.")

(defun spyglass--wait-listener-linger ()
  "Wait until a server stopped on Windows no longer disturbs a new one."
  (when spyglass--server-stopped
    (let ((left (- (+ spyglass--server-stopped spyglass--listener-linger) (float-time))))
      (when (> left 0)
        (sleep-for left)))
    (setq spyglass--server-stopped nil)))

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
server already runs, return it.  On Windows, a server started less
than `spyglass--listener-linger' seconds after the last one stopped
waits for them to pass first."
  (or (and spyglass--server
           (process-live-p (spyglass-server-process spyglass--server))
           spyglass--server)
      (let* ((token (spyglass-wire-random-token))
             (server (spyglass--make-server :token token))
             (process (progn
                        (spyglass--wait-listener-linger)
                        (make-network-process
                         :name "spyglass-server" :server t :host "127.0.0.1"
                         :service port :family 'ipv4 :coding 'binary :noquery t
                         :filter #'spyglass--filter :sentinel #'spyglass--sentinel
                         :log (lambda (_server client _message)
                                (spyglass--accept server client))))))
        (setf (spyglass-server-process server) process
              (spyglass-server-port server) (process-contact process :service))
        (when file
          (spyglass--write-registration file (spyglass-server-port server) token)
          (setf (spyglass-server-file server) file))
        (setq spyglass--server server))))

(defun spyglass--accept (server process)
  "Make a connection for PROCESS, accepted by SERVER."
  (spyglass--nodelay process)
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
      (when (process-live-p process)
        (delete-process process)
        (when (eq system-type 'windows-nt)
          (setq spyglass--server-stopped (float-time))))
      (when (and file (file-exists-p file))
        (ignore-errors (delete-file file))))
    (setq spyglass--server nil)))

(defun spyglass-server ()
  "Return the running server, or nil."
  (and spyglass--server
       (process-live-p (spyglass-server-process spyglass--server))
       spyglass--server))

;;;; Transports

;; A transport never has Emacs wait for the network: sockets connect
;; with `:nowait', host names are resolved by another Emacs (Emacs
;; resolves them synchronously, `:nowait' or not, except with glibc's
;; getaddrinfo_a), registrations are read by processes whose sentinels
;; take it from there, and a timer ends an attempt still going after
;; `spyglass-connect-timeout'.  What Emacs cannot avoid is starting a
;; process: it waits for the system to start it, which takes a
;; millisecond or less, but took up to a second in an Emacs running for
;; hours on a Mac with an endpoint security agent.  Hence the SSH
;; transport starts one process per attempt once it knows where the
;; server listens.

(defun spyglass--spawn (&rest arguments)
  "Start a process with `make-process' ARGUMENTS, from the home directory.
Emacs starts processes in the current buffer's directory, having
checked that it exists: the buffer current when a timer runs may be
any, its directory on a network file system slow to answer, or gone."
  (let ((default-directory (file-name-as-directory (expand-file-name "~"))))
    (apply #'make-process arguments)))

(defun spyglass--numeric-address (host)
  "Return HOST when it is a numeric address, which needs no resolving, else nil."
  (and (stringp host)
       ;; Only addresses look so, and their check reports no error.
       (string-match-p "\\`\\(?:[0-9.]+\\|.*:.*\\)\\'" host)
       (let ((inhibit-message t) (message-log-max nil))
         (network-lookup-address-info host nil 'numeric))
       host))

(defconst spyglass--resolver
  "(let ((addresses (network-lookup-address-info (getenv \"SPYGLASS_RESOLVE\"))))
     (dolist (address addresses)
       (princ (format-network-address address t))
       (terpri))
     (kill-emacs (if addresses 0 1)))"
  "Program of the Emacs resolving the host name in SPYGLASS_RESOLVE.
It prints the addresses of the host, one per line, or Emacs's error.")

(defun spyglass--resolve (host callback)
  "Resolve HOST in another Emacs, then call CALLBACK with its addresses.
CALLBACK gets a list of numeric addresses, as strings, or nil and a
message.  Return the resolving process, or nil when it could not start,
CALLBACK having been told."
  (let ((buffer (generate-new-buffer " *spyglass-resolve*" t))
        (process-environment (cons (concat "SPYGLASS_RESOLVE=" host) process-environment)))
    (condition-case err
        (spyglass--spawn
         :name "spyglass-resolve" :buffer buffer :noquery t :connection-type 'pipe
         :coding 'utf-8
         :command (list (expand-file-name invocation-name invocation-directory)
                        "-Q" "--batch" "--eval" spyglass--resolver)
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let* ((text (and (buffer-live-p buffer)
                               (with-current-buffer buffer (string-trim (buffer-string)))))
                    (addresses (and text (eq (process-status process) 'exit)
                                    (zerop (process-exit-status process))
                                    (delq nil (mapcar #'spyglass--numeric-address
                                                      (split-string text "[\r\n]+" t))))))
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (if addresses
                   (funcall callback addresses)
                 (funcall callback nil
                          (format "cannot resolve %s%s" host
                                  (if (and text (not (string-empty-p text)))
                                      (concat ": " (if (string-prefix-p (concat host " ") text)
                                                       (substring text (1+ (length host)))
                                                     text))
                                    ""))))))))
      (error (kill-buffer buffer)
             (funcall callback nil (error-message-string err))
             nil))))

(defun spyglass--connect-socket (host service done)
  "Connect to SERVICE on HOST without waiting, then call DONE.
DONE is called once: with the connected process and nil, or with nil
and a message when no address of HOST accepts, HOST cannot be resolved,
or `spyglass-connect-timeout' seconds pass first.  A numeric HOST is
connected to at once, a host name resolved first (`spyglass--resolve')
and its addresses tried in turn."
  (let ((pending nil) (timer nil) (finished nil))
    (cl-labels
        ((finish (process message)
           (when timer
             (cancel-timer timer)
             (setq timer nil))
           (setq pending nil)
           (if finished
               (when (processp process) (delete-process process))
             (setq finished t)
             (funcall done process message)))
         (connect (addresses problem)
           (if (null addresses)
               (finish nil problem)
             (let ((address (car addresses)) (rest (cdr addresses)))
               (condition-case err
                   (let ((process (make-network-process
                                   :name "spyglass" :host address :service service
                                   :coding 'binary :noquery t :nowait t :filter #'ignore
                                   :sentinel (lambda (process event)
                                               (settle process event address rest)))))
                     (setq pending process)
                     ;; A connection may complete at once, without telling.
                     (settle process nil address rest))
                 (error (connect rest (format "%s: %s" address (error-message-string err))))))))
         (settle (process event address rest)
           (when (and (eq process pending) (not finished))
             (pcase (process-status process)
               ('connect nil)
               ('open
                (set-process-sentinel process #'ignore)
                (finish process nil))
               (status
                (setq pending nil)
                (delete-process process)
                (connect rest (format "%s port %s: %s" address service
                                      (if event (string-trim event) status))))))))
      (setq timer (run-with-timer
                   spyglass-connect-timeout nil
                   (lambda ()
                     (setq timer nil)
                     (let ((process pending))
                       (finish nil (format "connecting to %s timed out after %gs"
                                           host spyglass-connect-timeout))
                       (when (process-live-p process) (delete-process process))))))
      (if (spyglass--numeric-address host)
          (connect (list host) nil)
        (let ((resolver (spyglass--resolve host (lambda (addresses &optional message)
                                                  (unless finished
                                                    (setq pending nil)
                                                    (if addresses
                                                        (connect addresses nil)
                                                      (finish nil message)))))))
          (unless finished
            (setq pending resolver)))))))

(defun spyglass-tcp-transport (host port token)
  "Return a transport connecting to PORT on HOST with TOKEN.
HOST is usually \"127.0.0.1\", the server listening there only, or
the local end of a forwarded port.  A host name is resolved by another
Emacs, Emacs's own resolving making it wait; give an address rather.
The stream comes once connected, Emacs not waiting meanwhile, or not
after `spyglass-connect-timeout'."
  (lambda (done)
    (spyglass--connect-socket host port
                              (lambda (process message)
                                (if (not process)
                                    (funcall done nil message)
                                  ;; Not `:nodelay' when made: Emacs 30's byte compiler refuses it.
                                  (spyglass--nodelay process)
                                  (funcall done process token))))))

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
each connection, so that a restarted server is found.  It is read as a
local file, without file name handlers: a remote one, which would have
Emacs wait for its host, is not found."
  (lambda (done)
    (let ((reg (condition-case nil
                   (with-temp-buffer
                     (let ((file-name-handler-alist nil)
                           (coding-system-for-read 'utf-8))
                       (insert-file-contents (or file spyglass-registration-file)))
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

(defun spyglass--discard-sink (sink)
  "Delete SINK, made by `spyglass--stderr-sink' for no process, and its buffer."
  (let ((buffer (process-buffer sink)))
    (when (process-live-p sink) (delete-process sink))
    (when (buffer-live-p buffer) (kill-buffer buffer))
    nil))

(cl-defun spyglass-ssh-transport (host &key (program spyglass-ssh-program)
                                       (file spyglass-remote-registration-file)
                                       args)
  "Return a transport to the spyglass server of HOST through SSH.
To connect, read the server's registration FILE on HOST with `PROGRAM
ARGS HOST cat FILE', then open a stream with `PROGRAM ARGS -W
127.0.0.1:PORT HOST', SSH forwarding standard input and output to the
port.  Once a stream so made has authenticated, connecting again opens
one to the same port with the same token at once, and FILE is read
again only after such a stream did not authenticate: Emacs waits for
the system to start each process, which may be long.  It waits for
neither process otherwise, and an attempt fails after
`spyglass-connect-timeout'.

PROGRAM defaults to `spyglass-ssh-program'; ARGS are extra options,
such as (\"-p\" \"2222\").  FILE goes to the remote shell as it is, so
that it expands `~'; that shell must know `cat', as POSIX shells and
PowerShell do."
  ;; (PORT TOKEN STREAM): where the server was, and the last stream to it.
  (let ((known nil))
    (lambda (done)
      (cl-flet ((stream (port token)
                  (spyglass--ssh-stream host program args port token
                                        (lambda (process &optional token)
                                          (when (processp process)
                                            (setq known (list port token process)))
                                          (funcall done process token)))))
        (if (and known (process-get (nth 2 known) 'spyglass-authenticated))
            (stream (nth 0 known) (nth 1 known))
          (setq known nil)
          (spyglass--read-registration host program args file
                                       (lambda (reg message)
                                         (if reg
                                             (stream (car reg) (cdr reg))
                                           (funcall done nil message)))))))))

(defun spyglass--read-registration (host program args file callback)
  "Read the registration FILE on HOST through PROGRAM ARGS, then call CALLBACK.
CALLBACK is called once: with the server's port and token, as a cons,
and nil; or with nil and a message, at the latest after
`spyglass-connect-timeout'."
  (let ((out (generate-new-buffer " *spyglass-registration*" t))
        (sink (spyglass--stderr-sink "spyglass-registration"))
        (finished nil) (timer nil) (process nil))
    (cl-flet ((finish (reg message)
                ;; Deleting the process runs its sentinel at once.
                (setq finished t)
                (when timer
                  (cancel-timer timer)
                  (setq timer nil))
                (let ((stderr (if process
                                  (spyglass--discard-process process)
                                (spyglass--discard-sink sink))))
                  (when (buffer-live-p out) (kill-buffer out))
                  (funcall callback reg
                           (and message
                                (concat message (if stderr (concat ": " stderr) "")))))))
      (condition-case err
          (progn
            (setq process
                  (spyglass--spawn
                   :name "spyglass-registration" :buffer out
                   :command (append (list program) args
                                    (list "-o" "BatchMode=yes" host (concat "cat " file)))
                   :coding 'utf-8 :noquery t :connection-type 'pipe :stderr sink
                   :sentinel
                   (lambda (process _event)
                     (unless (or finished (process-live-p process))
                       (let ((reg (and (eq (process-status process) 'exit)
                                       (zerop (process-exit-status process))
                                       (buffer-live-p out)
                                       (with-current-buffer out
                                         (spyglass-parse-registration (buffer-string))))))
                         (if reg
                             (finish reg nil)
                           (finish nil (format "no spyglass server registered on %s" host))))))))
            (process-put process 'spyglass-stderr sink)
            (setq timer (run-with-timer
                         spyglass-connect-timeout nil
                         (lambda ()
                           (setq timer nil)
                           (unless finished
                             (finish nil (format "reading the registration on %s timed out after %gs"
                                                 host spyglass-connect-timeout)))))))
        (error (unless finished
                 (finish nil (error-message-string err))))))))

(defun spyglass--ssh-stream (host program args port token done)
  "Call DONE with a stream to PORT on HOST through PROGRAM and TOKEN.
ARGS are extra options for PROGRAM."
  (let ((sink (spyglass--stderr-sink (format "spyglass-%s" host))))
    (condition-case err
        (let* ((size spyglass-w32-pipe-size)
               (stream (let ((w32-pipe-buffer-size size))
                         (spyglass--spawn
                          :name (format "spyglass-%s" host)
                          :command (append (list program) args
                                           (list "-o" "BatchMode=yes" "-W"
                                                 (format "127.0.0.1:%d" port) host))
                          :coding 'binary :noquery t :connection-type 'pipe
                          :filter #'ignore :sentinel #'ignore :stderr sink))))
          (process-put stream 'spyglass-stderr sink)
          (when (eq system-type 'windows-nt)
            (process-put stream 'spyglass-pipe-size (if (> size 0) size 4096)))
          (funcall done stream token))
      (error (spyglass--discard-sink sink)
             (funcall done nil (error-message-string err))))))

(provide 'spyglass-conn)
;;; spyglass-conn.el ends here
