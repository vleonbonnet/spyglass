;;; spyglass-owner-test.el --- Tests of spyglass's capture  -*- lexical-binding: t; -*-

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

;; The arithmetic of pending intervals, checked against strings: a
;; viewer's copy, brought up to date with the intervals composed from
;; random changes, must equal the buffer.

;;; Code:

(require 'spyglass-test-helpers)

(defun spyglass-owner-test--apply (viewer buffer intervals)
  "Return VIEWER updated from BUFFER, two strings, with INTERVALS.
Intervals are applied from the last, as the owner sends them."
  (let ((delta (spyglass--intervals-delta intervals)))
    (dolist (iv (reverse intervals))
      (let* ((b (nth 0 iv)) (bl (nth 1 iv)) (vl (nth 2 iv))
             (vbeg (- b (- delta (- bl vl)))))
        (setq viewer (concat (substring viewer 0 (1- vbeg))
                             (substring buffer (1- b) (+ (1- b) bl))
                             (substring viewer (+ (1- vbeg) vl))))
        (setq delta (- delta (- bl vl)))))
    viewer))

(defun spyglass-owner-test--random-text (n)
  "Return N random letters."
  (let ((s (make-string n ?a)))
    (dotimes (i n) (aset s i (+ ?a (random 26))))
    s))

(ert-deftest spyglass-owner-intervals-compose ()
  "Intervals composed from random changes bring a viewer up to date."
  (random "spyglass")
  (progn
    (dotimes (_run 500)
      (let* ((viewer (spyglass-owner-test--random-text (random 40)))
             (buffer viewer)
             (intervals nil)
             (max (1+ (random 6))))
        (dotimes (_step (1+ (random 30)))
          ;; A batch of disjoint changes, ascending, as a capture finds them.
          (let ((pos 1) changes (new buffer) (shift 0))
            (while (and (<= pos (1+ (length buffer))) (< (random 3) 2))
              (let* ((ob (+ pos (random (max 1 (- (+ 2 (length buffer)) pos)))))
                     (ob (min ob (1+ (length buffer))))
                     (ol (random (1+ (min 5 (- (1+ (length buffer)) ob)))))
                     (nl (random 6)))
                (push (list ob ol nl) changes)
                (setq new (concat (substring new 0 (+ (1- ob) shift))
                                  (spyglass-owner-test--random-text nl)
                                  (substring new (+ (1- ob) shift ol))))
                (setq shift (+ shift (- nl ol)))
                (setq pos (+ ob ol (random 2)))))
            (setq intervals (spyglass--intervals-cap
                             (spyglass--intervals-compose intervals (nreverse changes))
                             max)
                  buffer new)
            (when (zerop (random 4))
              (setq viewer (spyglass-owner-test--apply viewer buffer intervals)
                    intervals nil))))
        (should (equal (spyglass-owner-test--apply viewer buffer intervals) buffer))
        (should (<= (length intervals) max))))))

(ert-deftest spyglass-owner-intervals-to-changes ()
  "Intervals in the buffer convert to changes in the buffer as it was."
  (should (equal (spyglass--intervals-to-changes '((1 3 0) (10 2 5)))
                 '((1 0 3) (7 5 2)))))

(ert-deftest spyglass-owner-key-bytes ()
  "Keys translate to what a terminal sends."
  (should (equal (spyglass-key-bytes "RET") "\r"))
  (should (equal (spyglass-key-bytes "C-c") "\C-c"))
  (should (equal (spyglass-key-bytes "M-x") "\ex"))
  (should (equal (spyglass-key-bytes "<up>") "\e[A"))
  (should (equal (spyglass-key-bytes "é") (encode-coding-string "é" 'utf-8)))
  (should (null (spyglass-key-bytes "<mouse-1>"))))

(ert-deftest spyglass-owner-refuses-mirrors-and-duplicates ()
  "Mirrors cannot be shared, nor two buffers under one name."
  (unwind-protect
      (with-temp-buffer
        (setq-local spyglass--mirror t)
        (should-error (spyglass-share (current-buffer)))
        (kill-local-variable 'spyglass--mirror)
        (spyglass-share (current-buffer) :id "same")
        (with-temp-buffer
          (should-error (spyglass-share (current-buffer) :id "same"))))
    (spyglass-test-stop)))

(provide 'spyglass-owner-test)
;;; spyglass-owner-test.el ends here
