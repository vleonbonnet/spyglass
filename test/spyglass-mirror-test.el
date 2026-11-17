;;; spyglass-mirror-test.el --- Reading while mirrors update -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Valentin Leon
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replacements must preserve the text a viewer is selecting, including
;; repaints that resend unchanged text with different properties.

;;; Code:

(require 'spyglass-test-helpers)

(defmacro spyglass-mirror-test-with (text &rest body)
  "Run BODY in a live mirror initially holding TEXT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,text)
     (spyglass-mirror-mode)
     (let* ((connection (spyglass--make-connection))
            (mirror (spyglass--make-mirror
                     :connection-ref (spyglass--ref connection)
                     :buffer (current-buffer) :state 'live :point (point-max)
                     :styles (make-hash-table :test #'eql)
                     :faces (make-hash-table :test #'equal)
                     :overlays (make-hash-table :test #'eql)))
            (spyglass-mirror-show-cursor nil)
            (transient-mark-mode t))
       (setq spyglass--mirror mirror)
       ,@body)))

(defun spyglass-mirror-test-repaint (mirror text &optional style)
  "Replace MIRROR's contents with TEXT, using STYLE when given."
  (spyglass--mirror-apply
   mirror (list :seq (1+ (spyglass--mirror-seq mirror)) :pt (1+ (length text))
                :ops (vector (vector 1 (buffer-size) (length text)
                                     (if style (vector (length text) style) []))))
   (spyglass-wire-encode-text text)))

(ert-deftest spyglass-mirror-repaint-keeps-selected-text-and-properties ()
  "A redraw keeps the selected quote while replacing its styles."
  (spyglass-mirror-test-with "banner\nchosen quote\ntail\n"
    (goto-char 20)
    (set-mark 8)
    (setq mark-active t deactivate-mark nil)
    (puthash 1 '(face bold) (spyglass--mirror-styles mirror))
    (spyglass-mirror-test-repaint mirror (buffer-string) 1)
    (should (= (point) 20))
    (should (= (mark) 8))
    (should mark-active)
    (should-not deactivate-mark)
    (should (eq (get-text-property 8 'face) 'bold))
    (spyglass-mirror-test-repaint mirror "longer banner\nchosen quote\nnew tail\n")
    (should (equal (buffer-substring-no-properties (region-beginning) (region-end)) "chosen quote"))
    (should-not (get-text-property (mark) 'face))
    (should-not deactivate-mark)))

(ert-deftest spyglass-mirror-repaint-keeps-following-and-does-not-follow-a-region ()
  "Following survives a rewrite; an active selection does not start following."
  (spyglass-mirror-test-with "banner\nquote\n"
    (goto-char (point-max))
    (spyglass-mirror-test-repaint mirror "changed banner\nquote\nmore\n")
    (should (= (point) (point-max)))
    (set-mark 16)
    (setq mark-active t deactivate-mark nil)
    (let ((quote (buffer-substring-no-properties (mark) (point))))
      (spyglass-mirror-test-repaint mirror "changed banner\nquote\nmore\nnext\n")
      (should (equal (buffer-substring-no-properties (mark) (point)) quote))
      (should (< (point) (point-max)))
      (should-not deactivate-mark))))

(ert-deftest spyglass-mirror-replacement-timeout-keeps-reading-position ()
  "A bounded diff falling back to replacement does not send the reader to the top."
  (spyglass-mirror-test-with "banner\nchosen quote\ntail\n"
    (goto-char 20)
    (set-mark 8)
    (setq mark-active t deactivate-mark nil)
    (cl-letf (((symbol-function 'replace-region-contents)
               (lambda (beg end replacement &rest _)
                 (delete-region beg end)
                 (goto-char beg)
                 (insert (funcall replacement))
                 nil)))
      (spyglass-mirror-test-repaint mirror "BANNER\nchosen quote\nTAIL\n"))
    (should (= (point) 20))
    (should (= (mark) 8))
    (should mark-active)
    (should-not deactivate-mark)))

(provide 'spyglass-mirror-test)
;;; spyglass-mirror-test.el ends here
