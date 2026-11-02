;;; spyglass-channel.el --- Named message streams between spyglass peers  -*- lexical-binding: t; -*-

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

;; A channel is a named stream of messages on spyglass connections,
;; for applications built on spyglass, such as a session manager
;; telling another Emacs what runs here, over the connection that
;; carries the mirrors of the same pair of hosts.  A message is a JSON
;; object, given as a plist: it travels in the payload of a frame of
;; type `ch', and is parsed with `json-parse-string' on arrival, never
;; given to `read'.
;;
;;   (spyglass-register-channel "notes" #'my-handler
;;                              :request #'my-answer :attach #'my-hello)
;;   (spyglass-channel-send connection "notes" '(:t "hi" :n 1))
;;   (spyglass-channel-request connection "notes" '(:op "count")
;;                             (lambda (reply error) ...))
;;
;; A message larger than `spyglass-channel-chunk-size' bytes goes in
;; chunks, at most `spyglass-channel-window' bytes of them sent and not
;; acknowledged, from timers giving way to the command loop, and is
;; reassembled before its handler sees it; none may exceed
;; `spyglass-channel-max-size'.  Messages of a connection arrive in the
;; order sent, while its stream lives: those not sent when it breaks
;; are dropped, and so are those half received.  Applications resume
;; themselves as the connection comes back (see :attach).

;;; Code:

(require 'cl-lib)
(require 'spyglass-wire)
(require 'spyglass-conn)

(defcustom spyglass-channel-chunk-size (* 256 1024)
  "Largest message, in bytes of JSON, sent in one frame; larger ones go in chunks."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-channel-max-size (* 32 1024 1024)
  "Largest message, in bytes of JSON, sent or accepted on a channel."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-channel-window (* 1024 1024)
  "Bytes of chunks sent on a connection and not acknowledged, at most."
  :group 'spyglass
  :type 'natnum)

(defcustom spyglass-channel-tick-budget 0.004
  "Seconds a timer sending queued messages runs before giving way."
  :group 'spyglass
  :type 'number)

(defcustom spyglass-channel-request-timeout 30.0
  "Seconds a request waits for its reply before failing."
  :group 'spyglass
  :type 'number)

(defconst spyglass-channel--max-partial 16
  "Most messages a connection may have half received at once.")

(defvar spyglass--channels (make-hash-table :test #'equal)
  "Registered channels: names and plists (:handler :request :attach :detach).")

(defvar spyglass-connection-attach-functions nil
  "Abnormal hook run with a connection once its handshake succeeded.
It runs at every stream a connection gets: after a reconnection too.")

(defvar spyglass-connection-detach-functions nil
  "Abnormal hook run with (CONNECTION FINAL) as a connection's stream ends.
FINAL is non-nil when the connection will not come back.")

;;;; Registering

(cl-defun spyglass-register-channel (name handler &key request attach detach)
  "Register the channel NAME, a string, whose messages HANDLER receives.
HANDLER is called with (CONNECTION MESSAGE) for each message the peer
sends with `spyglass-channel-send', MESSAGE a plist as
`json-parse-string' returns it: objects as plists, arrays as vectors,
null as nil and false as `:false'.  REQUEST, if non-nil, answers the
requests of `spyglass-channel-request': it is called with (CONNECTION
MESSAGE REPLY), and calls REPLY once, now or later, with the answer, a
message, or with nil and a string telling why it fails.  ATTACH is
called with a connection as its handshake succeeds, at each new
stream; DETACH with (CONNECTION FINAL) as its stream ends, FINAL
non-nil when it will not come back.  Both are called only for peers
handling channels (see `spyglass-channel-available-p').

Each runs from a process filter, sentinel or timer: an error it
signals is reported through `spyglass-error-functions'.  Registering
NAME again replaces it."
  (puthash name (list :handler handler :request request :attach attach :detach detach)
           spyglass--channels)
  name)

(defun spyglass-unregister-channel (name)
  "Forget the channel NAME: messages for it are dropped from now on."
  (remhash name spyglass--channels))

(defun spyglass-channel-registered-p (name)
  "Return non-nil when the channel NAME is registered."
  (and (gethash name spyglass--channels) t))

(defun spyglass-channel-available-p (connection)
  "Return non-nil when CONNECTION is open and its peer handles channels.
A peer of the same protocol version predating channels does not
announce the capability \"ch\": nothing is sent on channels to it."
  (and (spyglass-connection-live-p connection)
       (spyglass-connection-capable-p connection "ch")))

;;;; Per connection

(cl-defstruct (spyglass--link (:constructor spyglass--link-make)
                              (:copier nil))
  "What the channels of one connection keep."
  (control nil :documentation "Small frames sent before the queue, oldest first: acknowledgments.")
  (head nil :documentation "The queue of frames to send, oldest first.")
  (tail nil :documentation "The last cons of the queue.")
  (inflight 0 :documentation "Bytes of chunks sent and not acknowledged.")
  (unacked nil :documentation "Hash table of the sizes of the chunks not acknowledged.")
  (next-id 0 :documentation "The last identifier of a chunked message.")
  (partial nil :documentation "Hash table of the messages half received, by identifier.")
  (timer nil :documentation "Timer sending the queue.")
  (stats nil :documentation "Counters, a plist."))

(defvar spyglass--links (make-hash-table :test #'eq :weakness 'key)
  "What the channels keep of each connection.")

(defun spyglass--link (connection)
  "Return the channel state of CONNECTION, made if needed."
  (or (gethash connection spyglass--links)
      (puthash connection
               (spyglass--link-make :unacked (make-hash-table :test #'equal)
                                    :partial (make-hash-table :test #'eql))
               spyglass--links)))

(defun spyglass--link-count (link key &optional amount)
  "Add AMOUNT, 1 by default, to counter KEY of LINK."
  (setf (spyglass--link-stats link)
        (plist-put (spyglass--link-stats link) key
                   (+ (or (plist-get (spyglass--link-stats link) key) 0) (or amount 1)))))

(defun spyglass-channel-stats (connection)
  "Return the counters of the channels of CONNECTION, a plist.
Messages and bytes sent and received, chunks, messages queued now."
  (let ((link (gethash connection spyglass--links)))
    (append (list :queued (length (and link (spyglass--link-head link)))
                  :inflight (if link (spyglass--link-inflight link) 0))
            (and link (copy-sequence (spyglass--link-stats link))))))

(defun spyglass--link-reset (connection)
  "Forget what CONNECTION's channels queued and half received."
  (when-let* ((link (gethash connection spyglass--links)))
    (when (spyglass--link-timer link)
      (cancel-timer (spyglass--link-timer link)))
    (setf (spyglass--link-control link) nil
          (spyglass--link-head link) nil
          (spyglass--link-tail link) nil
          (spyglass--link-inflight link) 0
          (spyglass--link-timer link) nil)
    (clrhash (spyglass--link-unacked link))
    (clrhash (spyglass--link-partial link))))

;;;; Encoding

(defun spyglass--channel-clean (value depth)
  "Return VALUE with its strings made such as JSON carries them.
Objects (plists and alists) and arrays are walked DEPTH levels deep."
  (cond
   ((stringp value) (spyglass-wire-plain-string value))
   ((<= depth 0) value)
   ((vectorp value) (vconcat (mapcar (lambda (v) (spyglass--channel-clean v (1- depth))) value)))
   ((and (consp value) (keywordp (car value)) (proper-list-p value))
    (let ((out nil))
      (while value
        (push (pop value) out)
        (push (spyglass--channel-clean (pop value) (1- depth)) out))
      (nreverse out)))
   ((and (consp value) (proper-list-p value) (cl-every #'consp value))
    (mapcar (lambda (pair) (cons (car pair) (spyglass--channel-clean (cdr pair) (1- depth))))
            value))
   (t value)))

(defun spyglass-channel-encode (message)
  "Return MESSAGE, a plist, as the bytes of JSON it travels as.
Strings JSON cannot carry, with raw bytes or surrogates, have those
replaced by U+FFFD.  Signal an error when MESSAGE holds other values
`json-serialize' refuses."
  (condition-case nil
      (json-serialize message :null-object nil :false-object :false)
    (wrong-type-argument
     (json-serialize (spyglass--channel-clean message 32) :null-object nil :false-object :false))))

(defun spyglass-channel-decode (bytes)
  "Return the message of BYTES, its JSON, as a plist, or signal an error."
  (json-parse-string bytes :object-type 'plist :array-type 'array
                     :null-object nil :false-object :false))

;;;; Sending

(defun spyglass--channel-header (channel extra)
  "Return the header of a frame of CHANNEL with EXTRA, a plist."
  (append (list :t "ch" :ch channel) extra))

(defun spyglass--channel-enqueue (connection items)
  "Queue ITEMS, frames to send on CONNECTION, and send what may be sent.
Each item is (FRAME SIZE ACK-KEY): ACK-KEY non-nil for a chunk, counted
against `spyglass-channel-window' until acknowledged."
  (let ((link (spyglass--link connection))
        (cell (copy-sequence items)))
    (when cell
      (if (spyglass--link-tail link)
          (setcdr (spyglass--link-tail link) cell)
        (setf (spyglass--link-head link) cell))
      (setf (spyglass--link-tail link) (last cell)))
    (spyglass--channel-pump connection)))

(defun spyglass--channel-schedule (connection &optional delay)
  "Send CONNECTION's queue from a timer, after DELAY seconds, 0 by default."
  (let ((link (spyglass--link connection)))
    (unless (spyglass--link-timer link)
      (setf (spyglass--link-timer link)
            (run-at-time (or delay 0) nil
                         (lambda ()
                           (setf (spyglass--link-timer link) nil)
                           (spyglass--guard connection
                             (spyglass--channel-pump connection))))))))

(defun spyglass--channel-pump (connection)
  "Send what CONNECTION's queue may send now, for a tick at most.
Control frames go first, then the queue in order; a chunk waits while
the window is full.  While a write waits on the stream, nothing is
sent: the connection would queue it apart from this queue, out of the
window's reach."
  (let ((link (spyglass--link connection)))
    (cond
     ((not (spyglass-channel-available-p connection))
      (spyglass--link-reset connection))
     ((spyglass--writing-p connection)
      (spyglass--channel-schedule connection 0.001))
     (t
      (let ((deadline (+ (float-time) spyglass-channel-tick-budget))
            (first t))
        (catch 'stop
          (while (spyglass--link-control link)
            (unless (spyglass--send-frame connection (pop (spyglass--link-control link)))
              (spyglass--link-reset connection)
              (throw 'stop nil)))
          (while (spyglass--link-head link)
            (pcase-let ((`(,frame ,size ,ack) (car (spyglass--link-head link))))
              (when (and ack (> (spyglass--link-inflight link) 0)
                         (> (+ (spyglass--link-inflight link) size) spyglass-channel-window))
                ;; An acknowledgment sends the rest.
                (throw 'stop nil))
              (when (and (not first) (> (float-time) deadline))
                (spyglass--channel-schedule connection)
                (throw 'stop nil))
              (setq first nil)
              (setf (spyglass--link-head link) (cdr (spyglass--link-head link)))
              (unless (spyglass--link-head link)
                (setf (spyglass--link-tail link) nil))
              (when ack
                ;; Counted before writing: its acknowledgment may come
                ;; while the write waits.
                (puthash ack size (spyglass--link-unacked link))
                (cl-incf (spyglass--link-inflight link) size))
              (unless (spyglass--send-frame connection frame)
                (spyglass--link-reset connection)
                (throw 'stop nil))
              (spyglass--link-count link :bytes-out size)))))))))

(defun spyglass--channel-control (connection header)
  "Send HEADER, a small frame without payload, on CONNECTION before its queue."
  (when (spyglass-channel-available-p connection)
    (let ((link (spyglass--link connection)))
      (setf (spyglass--link-control link)
            (append (spyglass--link-control link) (list (spyglass-wire-frame header))))
      (spyglass--channel-pump connection))))

(defun spyglass--channel-frames (channel extra bytes link)
  "Return the frames of a message of CHANNEL, BYTES of JSON, as queue items.
EXTRA is a plist of routing members every frame carries.  LINK numbers
chunked messages."
  (let ((size (length bytes)))
    (if (<= size spyglass-channel-chunk-size)
        (list (list (spyglass-wire-frame (spyglass--channel-header channel extra) bytes) size nil))
      (let* ((id (cl-incf (spyglass--link-next-id link)))
             (count (ceiling size (float spyglass-channel-chunk-size)))
             (items nil))
        (dotimes (n count)
          (let* ((start (* n spyglass-channel-chunk-size))
                 (piece (substring bytes start (min size (+ start spyglass-channel-chunk-size)))))
            (push (list (spyglass-wire-frame
                         (spyglass--channel-header channel (append (list :k id :n n :of count) extra))
                         piece)
                        (length piece) (cons id n))
                  items)))
        (spyglass--link-count link :chunks-out count)
        (nreverse items)))))

(defun spyglass--channel-send-bytes (connection channel extra bytes)
  "Send BYTES, a message's JSON, on CHANNEL of CONNECTION with EXTRA.
Return non-nil when sent or queued."
  (cond
   ((not (spyglass-channel-available-p connection)) nil)
   ((> (length bytes) spyglass-channel-max-size)
    (spyglass--report connection "Message of %d bytes on channel %s exceeds %d"
                      (length bytes) channel spyglass-channel-max-size)
    nil)
   (t
    (let ((link (spyglass--link connection)))
      (spyglass--link-count link :messages-out)
      (spyglass--channel-enqueue connection (spyglass--channel-frames channel extra bytes link))
      t))))

(defun spyglass-channel-send (connection channel message)
  "Send MESSAGE, a plist, to CONNECTION's peer on CHANNEL, a string.
MESSAGE is what `json-serialize' takes: keywords and values, objects as
plists, arrays as vectors, `:false' for false and nil for null; text JSON
cannot carry is replaced (see `spyglass-channel-encode').  Return
non-nil when the message is sent or queued, nil when the connection is
not open, its peer has no channels (`spyglass-channel-available-p') or
the message is too large.  Messages go in the order sent;
one not sent when the stream breaks is dropped."
  (spyglass--channel-send-bytes connection channel nil (spyglass-channel-encode message)))

(defun spyglass-channel-request (connection channel message callback &optional timeout)
  "Send MESSAGE as a request on CHANNEL to CONNECTION's peer.
CALLBACK is called once with (REPLY ERROR): REPLY the answer, a plist,
when ERROR is nil; ERROR a string when the peer refused or failed, when
no channel answers there, when TIMEOUT seconds passed, by default
`spyglass-channel-request-timeout', or when the stream ended first.
Return the request's identifier, or nil when it could not be sent, in
which case CALLBACK has been called already: the connection is not
open, its peer has no channels or the message is too large."
  (let* ((rid (cl-incf (spyglass-connection-next-rid connection)))
         (done nil)
         (timer nil)
         (deliver (lambda (reply error)
                    (unless done
                      (setq done t)
                      (when timer (cancel-timer timer))
                      (condition-case err
                          (funcall callback reply error)
                        (error (spyglass--report connection "Error in reply callback: %s"
                                                 (error-message-string err))))))))
    ;; Kept with spyglass's requests: a stream ending answers them nil.
    (push (cons rid (lambda (header)
                      (if (null header)
                          (funcall deliver nil "the connection ended")
                        (funcall deliver (plist-get header :reply) (plist-get header :error)))))
          (spyglass-connection-requests connection))
    (setq timer (run-at-time (or timeout spyglass-channel-request-timeout) nil
                             (lambda ()
                               (spyglass--answer connection rid
                                                 (list :error "no answer in time")))))
    (if (spyglass--channel-send-bytes connection channel (list :rq rid)
                                      (spyglass-channel-encode message))
        rid
      (spyglass--answer connection rid
                        (list :error (cond
                                      ((not (spyglass-connection-live-p connection))
                                       "the connection is not open")
                                      ((not (spyglass-connection-capable-p connection "ch"))
                                       "the peer has no channels")
                                      (t "the request is too large"))))
      nil)))

(defun spyglass--channel-reply (connection channel rid reply error)
  "Answer request RID of CHANNEL on CONNECTION with REPLY, or ERROR."
  (if error
      (when (spyglass-channel-available-p connection)
        (spyglass--link-count (spyglass--link connection) :messages-out)
        (spyglass--channel-enqueue
         connection
         (list (list (spyglass-wire-frame
                      (spyglass--channel-header
                       channel (list :rs rid :err (spyglass-wire-plain-string
                                                   (string-limit (format "%s" error) 4000)))))
                     0 nil))))
    (let ((bytes (spyglass-channel-encode reply)))
      (if (> (length bytes) spyglass-channel-max-size)
          (spyglass--channel-reply connection channel rid nil
                                   (format "reply of %d bytes too large" (length bytes)))
        (spyglass--channel-send-bytes connection channel (list :rs rid) bytes)))))

;;;; Receiving

(defun spyglass--channel-deliver (connection header bytes)
  "Hand the message of HEADER and BYTES, its JSON, received on CONNECTION."
  (let* ((channel (plist-get header :ch))
         (entry (and (stringp channel) (gethash channel spyglass--channels)))
         (rid (plist-get header :rq))
         (answer (plist-get header :rs)))
    (spyglass--link-count (spyglass--link connection) :messages-in)
    (cond
     ((natnump answer)
      (let ((error (plist-get header :err)))
        (if (stringp error)
            (spyglass--answer connection answer (list :error error))
          (let ((reply (condition-case err
                           (list :reply (spyglass-channel-decode (or bytes "null")))
                         (error (list :error (format "malformed reply: %s"
                                                     (error-message-string err)))))))
            (spyglass--answer connection answer reply)))))
     ((natnump rid)
      (let ((handler (plist-get entry :request))
            (answered nil))
        (if (not (functionp handler))
            (spyglass--channel-reply connection channel rid nil
                                     (format "no channel %s answers requests"
                                             (spyglass-wire-plain-string (format "%s" channel))))
          (condition-case err
              (funcall handler connection (spyglass-channel-decode (or bytes "null"))
                       (lambda (reply &optional error)
                         (unless answered
                           (setq answered t)
                           (when (spyglass-connection-live-p connection)
                             (spyglass--channel-reply connection channel rid reply error)))))
            (error
             (unless answered
               (setq answered t)
               (spyglass--channel-reply connection channel rid nil
                                        (error-message-string err))))))))
     ((functionp (plist-get entry :handler))
      (funcall (plist-get entry :handler) connection (spyglass-channel-decode (or bytes "null"))))
     (t (spyglass--link-count (spyglass--link connection) :unrouted)))))

(defun spyglass--channel-receive (connection header payload)
  "Handle a frame of a channel, HEADER and PAYLOAD, from CONNECTION's peer."
  (let ((link (spyglass--link connection))
        (id (plist-get header :k)))
    (spyglass--link-count link :bytes-in (length payload))
    (if (not id)
        (spyglass--channel-deliver connection header payload)
      (let ((n (plist-get header :n))
            (count (plist-get header :of))
            (partial (spyglass--link-partial link)))
        (spyglass--channel-control connection (list :t "cha" :k id :n n))
        (spyglass--link-count link :chunks-in)
        (cond
         ((not (and (natnump id) (natnump n) (natnump count) (< n count)
                    (<= (* (1- count) spyglass-channel-chunk-size) spyglass-channel-max-size)))
          (spyglass--report connection "Malformed chunk on channel %s"
                            (spyglass-wire-plain-string (format "%s" (plist-get header :ch)))))
         (t
          (let ((entry (gethash id partial)))
            (unless entry
              (when (>= (hash-table-count partial) spyglass-channel--max-partial)
                (spyglass--report connection "Too many messages half received; dropping them")
                (clrhash partial))
              (setq entry (list (make-vector count nil) 0 0))
              (puthash id entry partial))
            (let ((parts (car entry)))
              (when (and (= (length parts) count) (null (aref parts n)))
                (aset parts n (or payload ""))
                (setcar (cdr entry) (1+ (cadr entry)))
                (setcar (cddr entry) (+ (caddr entry) (length payload)))
                (cond
                 ((> (caddr entry) spyglass-channel-max-size)
                  (remhash id partial)
                  (spyglass--report connection "Message on channel %s exceeds %d bytes"
                                    (spyglass-wire-plain-string (format "%s" (plist-get header :ch)))
                                    spyglass-channel-max-size))
                 ((= (cadr entry) count)
                  (remhash id partial)
                  (spyglass--channel-deliver connection header
                                             (apply #'concat (append parts nil))))))))))))))

(defun spyglass--channel-ack (connection header _payload)
  "Release the chunk CONNECTION's peer acknowledges in HEADER."
  (when-let* ((link (gethash connection spyglass--links))
              (key (cons (plist-get header :k) (plist-get header :n)))
              (size (gethash key (spyglass--link-unacked link))))
    (remhash key (spyglass--link-unacked link))
    (setf (spyglass--link-inflight link) (max 0 (- (spyglass--link-inflight link) size)))
    (when (spyglass--link-head link)
      (spyglass--channel-pump connection))))

;;;; Connections coming and going

(defun spyglass--channel-attach (connection)
  "Tell the channels and the attach hook that CONNECTION's stream is open."
  (spyglass--link-reset connection)
  (run-hook-with-args 'spyglass-connection-attach-functions connection)
  (when (spyglass-connection-capable-p connection "ch")
    (maphash (lambda (_name entry)
               (when-let* ((attach (plist-get entry :attach)))
                 (spyglass--guard connection (funcall attach connection))))
             spyglass--channels)))

(defun spyglass--channel-detach (connection final)
  "Tell the channels and the detach hook that CONNECTION's stream ended.
FINAL is non-nil when it will not come back."
  (spyglass--link-reset connection)
  (when final
    (remhash connection spyglass--links))
  (spyglass--guard connection
    (run-hook-with-args 'spyglass-connection-detach-functions connection final))
  ;; The capabilities are still those of the stream that ended.
  (when (spyglass-connection-capable-p connection "ch")
    (maphash (lambda (_name entry)
               (when-let* ((detach (plist-get entry :detach)))
                 (spyglass--guard connection (funcall detach connection final))))
             spyglass--channels)))

(spyglass--register-handler "ch" #'spyglass--channel-receive)
(spyglass--register-handler "cha" #'spyglass--channel-ack)
(add-hook 'spyglass--attach-functions #'spyglass--channel-attach)
(add-hook 'spyglass--detach-functions #'spyglass--channel-detach)

(provide 'spyglass-channel)
;;; spyglass-channel.el ends here
