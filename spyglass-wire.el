;;; spyglass-wire.el --- Frames and safe values for spyglass  -*- lexical-binding: t; -*-

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

;; What travels between two Emacs instances, and nothing else: frames,
;; the Lisp values that may ride in them, and the sanitizers that keep
;; a peer's text properties from running code in this Emacs.
;;
;; A frame is a header, a JSON object, and a payload of raw bytes:
;;
;;   4 bytes  header length, big endian
;;   4 bytes  payload length, big endian
;;   header   UTF-8 JSON, parsed with `json-parse-string'
;;   payload  buffer text encoded in `utf-8-emacs', which carries every
;;            Emacs character, raw bytes included
;;
;; Nothing a peer sends is ever given to `read' or `eval'.  Lisp values
;; inside headers, such as faces and display specifications, are
;; encoded as JSON (see `spyglass-wire-encode'), decoded into data, and
;; then reduced by `spyglass-wire-sanitize-props' to the forms
;; redisplay shows without evaluating anything.

;;; Code:

(require 'cl-lib)

(defgroup spyglass nil
  "Mirror live buffers between Emacs instances."
  :group 'tools
  :prefix "spyglass-")

(defconst spyglass-wire-protocol 1
  "Version of the protocol spoken on the wire.
Peers whose major version differs refuse each other at the handshake.")

(defconst spyglass-wire-capabilities '("sz")
  "Optional messages this version handles, announced in its hello.
A peer sends such a message only to peers announcing it, so that
peers of the same protocol version but older work together: \"sz\",
the size of the windows showing a mirror.")

(defcustom spyglass-max-frame-size (* 64 1024 1024)
  "Largest frame, header and payload together, accepted from a peer.
A peer announcing a larger frame is disconnected.  Frames sent stay
far below it: updates are split into frames of
`spyglass-frame-chars' characters."
  :type 'natnum)

(defcustom spyglass-max-styles 8192
  "Most distinct property sets remembered per viewer before starting over."
  :type 'natnum)

(defconst spyglass-wire-handshake-frame-size (* 64 1024)
  "Largest frame accepted before a peer has authenticated.")

(define-error 'spyglass-wire-error "Spyglass protocol error")

(defun spyglass-window-cells (window)
  "Return the size of WINDOW in cells of its buffer's font, (COLUMNS . ROWS).
Measured as Emacs measures windows for the size of a process's
terminal (`window-adjust-process-window-size'): the unit of the sizes
viewers report (\"sz\") and owners give terminals."
  (cons (window-max-chars-per-line window)
        (with-selected-window window (floor (window-screen-lines)))))

;;;; Statistics

(defvar spyglass-wire-stats (list :encode-time 0.0 :decode-time 0.0
                                  :frames-out 0 :frames-in 0
                                  :bytes-out 0 :bytes-in 0)
  "Totals of the frames encoded and decoded in this Emacs.
A plist of seconds spent serializing and parsing headers and
encoding and decoding payloads, and of frames and bytes each way.")

(defun spyglass-wire-reset-stats ()
  "Reset `spyglass-wire-stats'."
  (setq spyglass-wire-stats (list :encode-time 0.0 :decode-time 0.0
                                  :frames-out 0 :frames-in 0
                                  :bytes-out 0 :bytes-in 0)))

(defmacro spyglass-wire--count (key amount)
  "Add AMOUNT to KEY of `spyglass-wire-stats'."
  `(plist-put spyglass-wire-stats ,key (+ (plist-get spyglass-wire-stats ,key)
                                          ,amount)))

;;;; Frames

(defun spyglass-wire--u32 (n)
  "Return N as four bytes, big endian."
  (unibyte-string (logand (ash n -24) 255) (logand (ash n -16) 255)
                  (logand (ash n -8) 255) (logand n 255)))

(defun spyglass-wire-encode-text (text)
  "Return TEXT as the bytes of a payload."
  (encode-coding-string text 'utf-8-emacs-unix t))

(defun spyglass-wire-decode-text (bytes)
  "Return the text whose payload is BYTES."
  (decode-coding-string bytes 'utf-8-emacs-unix t))

(defconst spyglass-wire--surrogates (format "[%c-%c]" #xd800 #xdfff)
  "Regexp matching surrogate code points.
They are Emacs characters, but no UTF-8 decoder returns them.")

(defun spyglass-wire-encode-payload (text)
  "Return TEXT as a payload, and the surrogates it held, as a cons.
The surrogates, a vector of offsets and characters, travel in the
header; the payload holds U+FFFD in their place."
  (let ((surrogates nil))
    (when (string-match-p spyglass-wire--surrogates text)
      (let ((start 0) pieces)
        (while (string-match spyglass-wire--surrogates text start)
          (let ((pos (match-beginning 0)))
            (push (vector pos (aref text pos)) surrogates)
            (push (substring text start pos) pieces)
            (push (string #xfffd) pieces)
            (setq start (1+ pos))))
        (push (substring text start) pieces)
        (setq text (apply #'concat (nreverse pieces)))))
    (cons (spyglass-wire-encode-text text)
          (and surrogates (vconcat (nreverse surrogates))))))

(defun spyglass-wire-decode-payload (bytes surrogates)
  "Return the text of payload BYTES, with its SURROGATES put back."
  (let ((text (spyglass-wire-decode-text bytes)))
    (if (not (vectorp surrogates))
        text
      (let ((start 0) pieces)
        (seq-doseq (s surrogates)
          (when (and (vectorp s) (= (length s) 2)
                     (natnump (aref s 0)) (<= start (aref s 0))
                     (< (aref s 0) (length text))
                     (integerp (aref s 1)) (<= #xd800 (aref s 1) #xdfff))
            (push (substring text start (aref s 0)) pieces)
            (push (string (aref s 1)) pieces)
            (setq start (1+ (aref s 0)))))
        (push (substring text start) pieces)
        (apply #'concat (nreverse pieces))))))

(defun spyglass-wire-frame (header &optional payload)
  "Return the frame of HEADER, a plist, and PAYLOAD, a unibyte string.
HEADER must hold only what `json-serialize' accepts."
  (let* ((start (float-time))
         (json (json-serialize header :null-object nil :false-object :false))
         (payload (or payload ""))
         (frame (concat (spyglass-wire--u32 (length json))
                        (spyglass-wire--u32 (length payload))
                        json payload)))
    (spyglass-wire--count :encode-time (- (float-time) start))
    (spyglass-wire--count :frames-out 1)
    (spyglass-wire--count :bytes-out (length frame))
    frame))

(defun spyglass-wire-frame-parts (header parts)
  "Return the frame of HEADER and a payload made of PARTS, as a list of strings.
PARTS are unibyte strings, sent one after the other rather than
concatenated, to spare a copy of the payload."
  (let* ((start (float-time))
         (json (json-serialize header :null-object nil :false-object :false))
         (plen (apply #'+ (mapcar #'length parts)))
         (head (concat (spyglass-wire--u32 (length json)) (spyglass-wire--u32 plen) json)))
    (spyglass-wire--count :encode-time (- (float-time) start))
    (spyglass-wire--count :frames-out 1)
    (spyglass-wire--count :bytes-out (+ (length head) plen))
    (cons head parts)))

(defun spyglass-wire-make-reader ()
  "Return a reader for the frames of one stream.
Give it to `spyglass-wire-feed' with each chunk received, and free it
with `spyglass-wire-free-reader'."
  (let ((buffer (generate-new-buffer " *spyglass-input*" t)))
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (setq buffer-undo-list t))
    buffer))

(defun spyglass-wire-free-reader (reader)
  "Free READER."
  (when (buffer-live-p reader)
    (kill-buffer reader)))

(defun spyglass-wire--read-u32 (pos)
  "Return the big endian number of four bytes at POS."
  (logior (ash (char-after pos) 24) (ash (char-after (+ pos 1)) 16)
          (ash (char-after (+ pos 2)) 8) (char-after (+ pos 3))))

(defun spyglass-wire-feed (reader chunk limit)
  "Add CHUNK, a unibyte string, to READER and return the frames now whole.
Each frame is a cons (HEADER . PAYLOAD): HEADER the parsed plist,
PAYLOAD a unibyte string or nil.  Signal `spyglass-wire-error' when a
frame is larger than LIMIT bytes or its header is not a JSON object;
the stream cannot be trusted after that."
  (with-current-buffer reader
    (goto-char (point-max))
    (insert chunk)
    (let (frames (whole t))
      (while (and whole (>= (buffer-size) 8))
        (let* ((hlen (spyglass-wire--read-u32 1))
               (plen (spyglass-wire--read-u32 5))
               (total (+ 8 hlen plen)))
          (when (or (> total limit) (zerop hlen))
            (signal 'spyglass-wire-error
                    (list (format "Frame of %d bytes (limit %d)" total limit))))
          (if (< (buffer-size) total)
              (setq whole nil)
            (let* ((start (float-time))
                   (header (condition-case nil
                               (json-parse-string
                                (buffer-substring-no-properties 9 (+ 9 hlen))
                                :object-type 'plist :array-type 'array
                                :null-object nil :false-object :false)
                             (error nil)))
                   (payload (and (> plen 0)
                                 (buffer-substring-no-properties
                                  (+ 9 hlen) (1+ total)))))
              (delete-region 1 (1+ total))
              (spyglass-wire--count :decode-time (- (float-time) start))
              (spyglass-wire--count :frames-in 1)
              (spyglass-wire--count :bytes-in total)
              (unless (and (consp header) (keywordp (car header)))
                (signal 'spyglass-wire-error (list "Malformed header")))
              (push (cons header payload) frames)))))
      (nreverse frames))))

;;;; Values

(defun spyglass-wire--ascii-p (string)
  "Return non-nil when STRING holds ASCII characters only."
  (not (string-match-p "[^[:ascii:]]" string)))

(defun spyglass-wire--json-string-p (string)
  "Return non-nil when STRING may go into a JSON header as it is."
  (if (multibyte-string-p string)
      (or (spyglass-wire--ascii-p string)
          (condition-case nil (and (json-serialize string) t) (error nil)))
    (spyglass-wire--ascii-p string)))

(defun spyglass-wire-plain-string (string)
  "Return STRING without properties, as JSON can carry it.
Characters JSON cannot carry, raw bytes and surrogates, become U+FFFD."
  (let ((plain (substring-no-properties string)))
    (if (spyglass-wire--json-string-p plain)
        plain
      (apply #'string
             (mapcar (lambda (c)
                       (if (or (> c #x10ffff) (<= #xd800 c #xdfff)) #xfffd c))
                     (string-to-multibyte plain))))))

(defun spyglass-wire-encode (value &optional depth)
  "Return VALUE as data `json-serialize' accepts.
VALUE is a sanitized property value: nil, t, numbers, symbols,
strings, possibly with properties, lists, conses and vectors of
those.  Anything else, and anything nested deeper than DEPTH (16 by
default), becomes nil."
  (let ((depth (or depth 16)))
    (cond
     ((null value) :false)
     ((eq value t) t)
     ((integerp value)
      (if (<= (abs value) 9007199254740991) value :false))
     ((floatp value)
      (if (or (isnan value) (= value 1.0e+INF) (= value -1.0e+INF)) :false value))
     ((stringp value)
      (let ((intervals (cl-remove-if-not #'caddr (object-intervals value)))
            (plain (substring-no-properties value)))
        (cond
         ((and (null intervals) (spyglass-wire--json-string-p plain))
          plain)
         (t
          (list :S (if (spyglass-wire--json-string-p plain)
                       plain
                     (list :B (base64-encode-string
                               (spyglass-wire-encode-text
                                (string-to-multibyte plain))
                               t)))
                :p (vconcat
                    (mapcar (lambda (iv)
                              (vector (car iv) (cadr iv)
                                      (spyglass-wire-encode (caddr iv)
                                                            (1- depth))))
                            intervals)))))))
     ((<= depth 0) :false)
     ((symbolp value) (list :y (symbol-name value)))
     ((vectorp value)
      (list :v (vconcat (mapcar (lambda (v) (spyglass-wire-encode v (1- depth)))
                                value))))
     ((consp value)
      (if (proper-list-p value)
          (vconcat (mapcar (lambda (v) (spyglass-wire-encode v (1- depth))) value))
        (list :c (vector (spyglass-wire-encode (car value) (1- depth))
                         (spyglass-wire-encode (cdr value) (1- depth))))))
     (t :false))))

(defun spyglass-wire-decode (data &optional depth)
  "Return the Lisp value DATA, a parsed JSON value, encodes.
The inverse of `spyglass-wire-encode'.  Data nested deeper than DEPTH
\(16 by default) and anything malformed decode to nil.  The result is
data only; sanitize it before it reaches a buffer."
  (let ((depth (or depth 16)))
    (cond
     ((eq data :false) nil)
     ((eq data t) t)
     ((numberp data) data)
     ((stringp data) data)
     ((<= depth 0) nil)
     ((vectorp data)
      (mapcar (lambda (d) (spyglass-wire-decode d (1- depth))) data))
     ((and (consp data) (keywordp (car data)))
      (let ((y (plist-get data :y)) (c (plist-get data :c))
            (v (plist-get data :v)) (s (plist-get data :S))
            (b (plist-get data :B)))
        (cond
         ((stringp y) (intern y))
         ((and (vectorp c) (= (length c) 2))
          (cons (spyglass-wire-decode (aref c 0) (1- depth))
                (spyglass-wire-decode (aref c 1) (1- depth))))
         ((vectorp v)
          (vconcat (mapcar (lambda (d) (spyglass-wire-decode d (1- depth))) v)))
         ((stringp b) (spyglass-wire--decode-b64 b))
         (s (let ((string (if (stringp s) (copy-sequence s)
                            (and (consp s) (stringp (plist-get s :B))
                                 (spyglass-wire--decode-b64 (plist-get s :B)))))
                  (runs (plist-get data :p)))
              (when string
                (when (vectorp runs)
                  (let ((len (length string)))
                    (seq-doseq (run runs)
                      (when (and (vectorp run) (= (length run) 3)
                                 (natnump (aref run 0)) (natnump (aref run 1))
                                 (< (aref run 0) (aref run 1))
                                 (<= (aref run 1) len))
                        (let ((props (spyglass-wire-decode (aref run 2) (1- depth))))
                          (when (and (consp props) (proper-list-p props)
                                     (cl-evenp (length props)))
                            (set-text-properties (aref run 0) (aref run 1)
                                                 props string)))))))
                string))))))
     (t nil))))

(defun spyglass-wire--decode-b64 (string)
  "Return the text STRING, base64 of a payload, encodes, or nil."
  (condition-case nil
      (spyglass-wire-decode-text (base64-decode-string string))
    (error nil)))

;;;; Sanitizing properties

(defconst spyglass-wire-face-attributes
  '(:family :foundry :width :height :weight :slant :foreground
    :distant-foreground :background :underline :overline :strike-through
    :box :inverse-video :extend :inherit)
  "Face attributes kept in faces from a peer.
Left out: `:stipple' and `:font', which name local resources, and
`:fontset'.")

(defun spyglass-wire--simple-p (value depth)
  "Return non-nil when VALUE is plain data no deeper than DEPTH.
Plain data is strings, numbers, symbols, and lists or conses of
those; no function object can hide in it."
  (cond
   ((or (stringp value) (numberp value) (symbolp value)) t)
   ((<= depth 0) nil)
   ((consp value)
    (and (spyglass-wire--simple-p (car value) (1- depth))
         (spyglass-wire--simple-p (cdr value) (1- depth))))
   (t nil)))

(defun spyglass-wire--sanitize-face-plist (plist)
  "Return the attributes of face PLIST safe to show, or nil."
  (let (out)
    (while (and (consp plist) (consp (cdr plist)))
      (let ((attr (car plist)) (value (cadr plist)))
        (when (and (memq attr spyglass-wire-face-attributes)
                   (not (eq value 'unspecified))
                   (pcase attr
                     ;; A function as height would run at redisplay.
                     (:height (numberp value))
                     (:inherit (or (symbolp value)
                                   (and (proper-list-p value)
                                        (cl-every #'symbolp value))))
                     ((or :foreground :background :distant-foreground
                          :family :foundry)
                      (stringp value))
                     (_ (spyglass-wire--simple-p value 4))))
          (setq out (cons value (cons attr out)))))
      (setq plist (cddr plist)))
    (nreverse out)))

(defun spyglass-wire-sanitize-face (face &optional depth)
  "Return FACE, a `face' property value, reduced to safe forms, or nil.
Keeps face names, attribute plists and lists of them, nested at most
DEPTH (4 by default) deep."
  (let ((depth (or depth 4)))
    (cond
     ((or (null face) (eq face t)) nil)
     ((symbolp face) (and (not (keywordp face)) face))
     ((<= depth 0) nil)
     ((not (consp face)) nil)
     ((memq (car face) '(foreground-color background-color))
      (and (stringp (cdr face))
           (list (if (eq (car face) 'foreground-color) :foreground :background)
                 (cdr face))))
     ((keywordp (car face))
      (spyglass-wire--sanitize-face-plist face))
     ((proper-list-p face)
      (let ((faces (delq nil (mapcar (lambda (f)
                                       (spyglass-wire-sanitize-face f (1- depth)))
                                     face))))
        (if (cdr faces) faces (car faces))))
     (t nil))))

(defun spyglass-wire--sanitize-pixel-spec (spec depth)
  "Return SPEC, a pixel specification, if it is safe, else nil.
DEPTH bounds nesting."
  (cond
   ((numberp spec) spec)
   ((symbolp spec) spec)
   ((<= depth 0) nil)
   ((and (consp spec) (memq (car spec) '(+ -)) (proper-list-p spec))
    (let ((args (mapcar (lambda (s) (spyglass-wire--sanitize-pixel-spec s (1- depth)))
                        (cdr spec))))
      (and (not (memq nil args)) (cons (car spec) args))))
   ((and (consp spec) (numberp (car spec))
         (or (null (cdr spec)) (symbolp (cdr spec))
             (numberp (cdr spec))))
    spec)
   ((and (consp spec) (symbolp (car spec)) (numberp (cdr spec)))
    spec)
   (t nil)))

(defun spyglass-wire--sanitize-space (plist)
  "Return the safe properties of a `space' display PLIST."
  (let (out)
    (while (and (consp plist) (consp (cdr plist)))
      (let ((key (car plist))
            (value (spyglass-wire--sanitize-pixel-spec (cadr plist) 4)))
        (when (and value (memq key '(:width :height :align-to :relative-width
                                            :relative-height :ascent)))
          (setq out (cons value (cons key out)))))
      (setq plist (cddr plist)))
    (cons 'space (nreverse out))))

(defun spyglass-wire--sanitize-one-display (spec depth)
  "Return display SPEC, a single specification, if safe, else nil.
DEPTH bounds the nesting of strings in strings."
  (cond
   ((stringp spec) (spyglass-wire-sanitize-string spec depth))
   ((not (consp spec)) nil)
   ((eq (car spec) 'space)
    (and (proper-list-p spec) (spyglass-wire--sanitize-space (cdr spec))))
   ((memq (car spec) '(raise space-width))
    (and (proper-list-p spec) (numberp (cadr spec)) (list (car spec) (cadr spec))))
   ((eq (car spec) 'height)
    ;; A symbol or a form as height runs code at redisplay.
    (let ((h (cadr spec)))
      (and (proper-list-p spec)
           (or (numberp h)
               (and (consp h) (memq (car h) '(+ -)) (numberp (cadr h))
                    (null (cddr h))))
           (list 'height h))))
   ((eq (car spec) 'min-width)
    (let ((w (spyglass-wire--sanitize-pixel-spec (cadr spec) 4)))
      (and w (list 'min-width w))))
   ((memq (car spec) '(left-fringe right-fringe))
    (and (proper-list-p spec) (cl-every #'symbolp (cdr spec))
         (<= (length spec) 3) spec))
   ((and (consp (car spec)) (eq (caar spec) 'margin))
    (let ((margin (car spec)))
      (and (proper-list-p margin) (= (length margin) 2)
           (memq (cadr margin) '(left-margin right-margin))
           (stringp (cadr spec))
           (list (list 'margin (cadr margin))
                 (spyglass-wire-sanitize-string (cadr spec) depth)))))
   (t nil)))

(defun spyglass-wire-sanitize-display (value &optional depth)
  "Return VALUE, a `display' property, reduced to safe forms, or nil.
Strings, spaces, raising, heights, margins and fringes stay; images,
`when' conditions and anything that would evaluate a form go.  DEPTH
\(2 by default) bounds strings nested in display strings."
  (let ((depth (or depth 2)))
    (cond
     ((stringp value) (spyglass-wire-sanitize-string value depth))
     ((vectorp value)
      (let ((specs (delq nil (mapcar (lambda (s)
                                       (spyglass-wire--sanitize-one-display s depth))
                                     value))))
        (and specs (vconcat specs))))
     ((not (consp value)) nil)
     ((and (consp (car value)) (not (eq (caar value) 'margin)))
      (and (proper-list-p value)
           (let ((specs (delq nil (mapcar (lambda (s)
                                            (spyglass-wire--sanitize-one-display
                                             s depth))
                                          value))))
             (if (cdr specs) specs (car specs)))))
     (t (spyglass-wire--sanitize-one-display value depth)))))

(defun spyglass-wire-sanitize-invisible (value)
  "Return VALUE, an `invisible' property, if it is a symbol or symbols."
  (cond
   ((symbolp value) value)
   ((and (proper-list-p value) (cl-every #'symbolp value)) value)
   (t nil)))

(defcustom spyglass-max-property-string 65536
  "Longest string kept in a property value from a peer, in characters."
  :type 'natnum)

(defun spyglass-wire-sanitize-string (string &optional depth)
  "Return a copy of STRING with only safe display properties, or nil.
Keeps faces, invisibility and, while DEPTH (1 by default) is above
zero, display specifications.  STRING longer than
`spyglass-max-property-string' is truncated."
  (when (stringp string)
    (let* ((depth (or depth 1))
           (copy (substring-no-properties
                  string 0 (min (length string) spyglass-max-property-string))))
      (dolist (iv (object-intervals string))
        (when (and (caddr iv) (< (car iv) (length copy)))
          (let* ((plist (caddr iv))
                 (face (spyglass-wire-sanitize-face (plist-get plist 'face)))
                 (invisible (spyglass-wire-sanitize-invisible
                             (plist-get plist 'invisible)))
                 (display (and (> depth 0)
                               (spyglass-wire-sanitize-display
                                (plist-get plist 'display) (1- depth))))
                 props)
            (when display (setq props (list 'display display)))
            (when invisible (setq props (cons 'invisible (cons invisible props))))
            (when face (setq props (cons 'face (cons face props))))
            (when props
              (set-text-properties (car iv) (min (cadr iv) (length copy))
                                   props copy)))))
      copy)))

(defconst spyglass-wire-text-properties
  '(face invisible display line-prefix wrap-prefix help-echo)
  "Text properties a mirror may receive, in the order they are kept.")

(defconst spyglass-wire-overlay-properties
  '(face invisible display before-string after-string help-echo priority)
  "Overlay properties a mirror may receive, in the order they are kept.")

(defconst spyglass-wire-style-properties
  '(face invisible display line-prefix wrap-prefix help-echo
         before-string after-string priority)
  "Every property a style may hold, text's or overlays', in the order kept.")

(defun spyglass-wire-sanitize-prop (prop value)
  "Return VALUE of property PROP reduced to safe forms, or nil."
  (pcase prop
    ('face (spyglass-wire-sanitize-face value))
    ('invisible (spyglass-wire-sanitize-invisible value))
    ('display (spyglass-wire-sanitize-display value))
    ((or 'line-prefix 'wrap-prefix)
     (if (stringp value)
         (spyglass-wire-sanitize-string value)
       (and (consp value) (eq (car value) 'space)
            (spyglass-wire--sanitize-one-display value 0))))
    ((or 'before-string 'after-string)
     (spyglass-wire-sanitize-string value 1))
    ('help-echo (and (stringp value) (spyglass-wire-sanitize-string value 0)))
    ('priority (and (integerp value) value))
    (_ nil)))

(defun spyglass-wire-sanitize-props (plist allowed)
  "Return the properties of PLIST among ALLOWED, sanitized.
ALLOWED lists property names in the order the result keeps; a
property whose value sanitizes to nil is dropped.  The result is a
fresh plist, equal for equal displays."
  (let (out)
    (dolist (prop allowed)
      (let ((value (plist-get plist prop)))
        (when value
          (let ((clean (spyglass-wire-sanitize-prop prop value)))
            (when clean
              (setq out (cons clean (cons prop out))))))))
    (nreverse out)))

;;;; Tokens

(defun spyglass-wire-random-token ()
  "Return a fresh random token of 64 hexadecimal digits.
It is read from the system's random device where there is one, or
else hashed from Emacs's random generator, which the system seeds
\(see `random'), and from what varies between instances."
  (let ((bytes
         (or (and (file-readable-p "/dev/urandom")
                  (condition-case nil
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally "/dev/urandom" nil 0 32)
                        (and (= (buffer-size) 32) (buffer-string)))
                    (error nil)))
             (progn
               (random t)
               (secure-hash 'sha256
                            (format "%S %S %S %S %S %S"
                                    (cl-loop repeat 16 collect (random))
                                    (time-convert nil t) (emacs-pid)
                                    (current-cpu-time) (memory-use-counts)
                                    (random))
                            nil nil t)))))
    (mapconcat (lambda (b) (format "%02x" b)) bytes "")))

(defun spyglass-wire-token-equal (a b)
  "Return non-nil when tokens A and B are equal, in constant time.
The time taken depends on the length of B, the expected token, only."
  (and (stringp a) (stringp b)
       (let ((diff (if (= (length a) (length b)) 0 1))
             (la (length a)))
         (dotimes (i (length b))
           (setq diff (logior diff (logxor (if (< i la) (aref a i) 0)
                                           (aref b i)))))
         (zerop diff))))

(provide 'spyglass-wire)
;;; spyglass-wire.el ends here
