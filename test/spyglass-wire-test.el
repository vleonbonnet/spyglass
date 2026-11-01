;;; spyglass-wire-test.el --- Tests of spyglass's frames and values  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'spyglass-test-helpers)

(defun spyglass-wire-test--roundtrip (header payload &optional chunk)
  "Return the frames read back from HEADER and PAYLOAD fed CHUNK bytes at a time."
  (let ((frame (spyglass-wire-frame header payload))
        (reader (spyglass-wire-make-reader))
        frames)
    (unwind-protect
        (let ((i 0) (chunk (or chunk (length frame))))
          (while (< i (length frame))
            (setq frames (append frames (spyglass-wire-feed
                                         reader (substring frame i (min (length frame)
                                                                        (+ i chunk)))
                                         spyglass-max-frame-size)))
            (setq i (+ i chunk)))
          frames)
      (spyglass-wire-free-reader reader))))

(ert-deftest spyglass-wire-frame-roundtrip ()
  "Frames come back whole, however the stream splits them."
  (let* ((text (concat "héllo\0wörld\r\n" (string #x3fff80 #x1f600) "end"))
         (payload (spyglass-wire-encode-text text)))
    (dolist (chunk '(1 3 7 1000))
      (let ((frames (spyglass-wire-test--roundtrip (list :t "u" :seq 3) payload chunk)))
        (should (= (length frames) 1))
        (should (equal (plist-get (caar frames) :t) "u"))
        (should (equal (spyglass-wire-decode-text (cdar frames)) text))))))

(ert-deftest spyglass-wire-several-frames-in-one-chunk ()
  "Several frames in one chunk are all read, in order."
  (let ((reader (spyglass-wire-make-reader)))
    (unwind-protect
        (let* ((bytes (concat (spyglass-wire-frame (list :t "a"))
                              (spyglass-wire-frame (list :t "b") "xyz")
                              (substring (spyglass-wire-frame (list :t "c")) 0 5)))
               (frames (spyglass-wire-feed reader bytes spyglass-max-frame-size)))
          (should (equal (mapcar (lambda (f) (plist-get (car f) :t)) frames) '("a" "b")))
          (should (equal (cdr (nth 1 frames)) "xyz")))
      (spyglass-wire-free-reader reader))))

(ert-deftest spyglass-wire-refuses-large-and-malformed-frames ()
  "A frame over the limit or with a header that is not an object is an error."
  (let ((reader (spyglass-wire-make-reader)))
    (unwind-protect
        (progn
          (should-error (spyglass-wire-feed reader (spyglass-wire-frame (list :t "x") "0123456789")
                                            20)
                        :type 'spyglass-wire-error)
          (spyglass-wire-free-reader reader)
          (setq reader (spyglass-wire-make-reader))
          (should-error (spyglass-wire-feed reader (concat (spyglass-wire--u32 2)
                                                           (spyglass-wire--u32 0) "[]")
                                            1000)
                        :type 'spyglass-wire-error)
          (spyglass-wire-free-reader reader)
          (setq reader (spyglass-wire-make-reader))
          (should-error (spyglass-wire-feed reader (concat (spyglass-wire--u32 3)
                                                           (spyglass-wire--u32 0) "{x}")
                                            1000)
                        :type 'spyglass-wire-error))
      (spyglass-wire-free-reader reader))))

(defun spyglass-wire-test--through-json (value)
  "Return VALUE encoded, serialized, parsed and decoded."
  (spyglass-wire-decode
   (plist-get (json-parse-string (json-serialize (list :v (spyglass-wire-encode value))
                                                 :null-object nil :false-object :false)
                                 :object-type 'plist :array-type 'array
                                 :null-object nil :false-object :false)
              :v)))

(ert-deftest spyglass-wire-values-roundtrip ()
  "Property values survive encoding."
  (dolist (value (list nil t 42 -7 2.5 'bold :foreground "plain" "ünïcode"
                       '(:foreground "red" :weight bold)
                       '(bold italic (:background "blue"))
                       '(foreground-color . "red")
                       '(space :width 3 :align-to (+ left 2))
                       [a b "c"]
                       (string #x3fff81 ?a)))
    (should (equal-including-properties (spyglass-wire-test--through-json value) value)))
  (let ((s (concat "a" (propertize "bc" 'face 'bold 'display "X") "d")))
    (should (equal-including-properties (spyglass-wire-test--through-json s) s))))

(ert-deftest spyglass-wire-sanitize-faces ()
  "Faces keep names and plain attributes, and lose what could run code."
  (should (eq (spyglass-wire-sanitize-face 'bold) 'bold))
  (should (equal (spyglass-wire-sanitize-face '(:foreground "red" :height my-height-fn))
                 '(:foreground "red")))
  (should (equal (spyglass-wire-sanitize-face '(:height 1.2 :stipple "x" :inherit (bold italic)))
                 '(:height 1.2 :inherit (bold italic))))
  (should (equal (spyglass-wire-sanitize-face '(bold (:underline (:color "red" :style wave))))
                 '(bold (:underline (:color "red" :style wave)))))
  (should (equal (spyglass-wire-sanitize-face '(foreground-color . "red"))
                 '(:foreground "red")))
  (should (null (spyglass-wire-sanitize-face (lambda () 1))))
  (should (null (spyglass-wire-sanitize-face '(:foreground (lambda () "red"))))))

(ert-deftest spyglass-wire-sanitize-display ()
  "Display specifications keep what shows and lose what evaluates."
  (should (equal (spyglass-wire-sanitize-display "x") "x"))
  (should (null (spyglass-wire-sanitize-display '(when (delete-file "/x") . "y"))))
  (should (null (spyglass-wire-sanitize-display '(image :type png :file "/etc/passwd"))))
  (should (null (spyglass-wire-sanitize-display '(height (my-function 2)))))
  (should (equal (spyglass-wire-sanitize-display '(height (+ 1))) '(height (+ 1))))
  (should (equal (spyglass-wire-sanitize-display '(space :width 4 :height (eval-me)))
                 '(space :width 4)))
  (should (equal (spyglass-wire-sanitize-display '((raise 0.5) (when t . "x") "y"))
                 '((raise 0.5) "y")))
  (should (equal (spyglass-wire-sanitize-display '((margin left-margin) "m"))
                 '((margin left-margin) "m")))
  (let ((s (propertize "a" 'face 'bold 'keymap '(keymap) 'display '(when t . "z"))))
    (should (equal-including-properties (spyglass-wire-sanitize-display s)
                                        (propertize "a" 'face 'bold))))
  (should (null (spyglass-wire-sanitize-prop 'help-echo #'ignore)))
  (should (equal (spyglass-wire-sanitize-prop 'invisible '(a b)) '(a b)))
  (should (null (spyglass-wire-sanitize-prop 'invisible "x"))))

(ert-deftest spyglass-wire-decode-is-data-only ()
  "Decoding never evaluates and drops malformed data."
  (should (null (spyglass-wire-decode (list :c [1]))))
  (should (equal (spyglass-wire-decode [1 (:y "foo") "x"]) '(1 foo "x")))
  (let ((deep 1))
    (dotimes (_ 100) (setq deep (vector deep)))
    (should (listp (spyglass-wire-decode deep))))
  (should (equal (spyglass-wire-decode (list :S "abc" :p [[0 9 [(:y "face") (:y "bold")]]]))
                 "abc")))

(ert-deftest spyglass-wire-tokens ()
  "Tokens are random and compared exactly."
  (let ((a (spyglass-wire-random-token)) (b (spyglass-wire-random-token)))
    (should (= (length a) 64))
    (should-not (equal a b))
    (should (spyglass-wire-token-equal a (copy-sequence a)))
    (should-not (spyglass-wire-token-equal b a))
    (should-not (spyglass-wire-token-equal (substring a 1) a))
    (should-not (spyglass-wire-token-equal nil a))))

(provide 'spyglass-wire-test)
;;; spyglass-wire-test.el ends here
