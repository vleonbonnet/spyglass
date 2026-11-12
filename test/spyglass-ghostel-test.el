;;; spyglass-ghostel-test.el --- Tests of the Ghostel adapter's advice  -*- lexical-binding: t; -*-

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

;; Ghostel's renderer, `ghostel--redraw', is a function of its native
;; module, which defines its functions with `fset' when it loads: after
;; the adapter, when the module was missing or stale as Ghostel loaded,
;; and again when it is loaded again.  These tests stand in for Ghostel
;; and its module, which they never load: a mode, its variables, and a
;; renderer defined with `fset' as the module does.

;;; Code:

(require 'spyglass-test-helpers)

(defvar ghostel--term nil)
(defvar ghostel--process nil)
(defvar ghostel--term-rows 24)
(defvar ghostel--term-cols 80)
(defvar ghostel--cursor-char-pos nil)
(defvar ghostel--repainted-region nil)
(defvar ghostel--pending-redraw nil)

(define-derived-mode ghostel-mode fundamental-mode "Ghostel"
  "A stand-in for Ghostel's mode, in tests only.")

(defun spyglass-ghostel-test--render (_term &rest _)
  "Render as Ghostel's module does: write a line without modification hooks.
Publish the range written in `ghostel--repainted-region'."
  (let ((inhibit-modification-hooks t)
        (start (point-max)))
    (save-excursion
      (goto-char start)
      (insert (format "rendered at %d\n" start)))
    (setq ghostel--repainted-region (cons start (point-max)))
    t))

(defun spyglass-ghostel-test--load-module ()
  "Define the renderer as Ghostel's module does when it loads, with `fset'."
  (fset 'ghostel--redraw #'spyglass-ghostel-test--render))

(defun spyglass-ghostel-test--advised-p ()
  "Return non-nil when the adapter's advice is in the renderer itself."
  (advice-function-member-p #'spyglass-ghostel--after-redraw
                            (symbol-function 'ghostel--redraw)))

;; As Ghostel loaded with its module missing: the adapter comes before
;; the renderer exists.
(unless (featurep 'ghostel)
  (require 'spyglass-ghostel))

(ert-deftest spyglass-ghostel-advises-a-module-loaded-later ()
  "Renders reach viewers through the advice, the module loaded after the adapter.
Ghostel's module came after the adapter, when it was missing as
Ghostel loaded, and defined the renderer with `fset': the advice
waited pending for good, and a terminal's renders were told to
spyglass no more, found only by polling.  Polling here never runs,
so that only the advice can tell them."
  (skip-unless (not (featurep 'ghostel)))
  (should-not (fboundp 'ghostel--redraw))
  (let ((spyglass-poll-interval 3600))
    (unwind-protect
        (progn
          (spyglass-ghostel-test--load-module)
          (should-not (spyglass-ghostel-test--advised-p))
          (spyglass-test-with-connection connection
                                         (let ((buffer (generate-new-buffer "*ghostel stand-in*")))
                                           (unwind-protect
                                               (with-current-buffer buffer
                                                 (ghostel-mode)
                                                 (insert "prompt\n")
                                                 (let ((mirror (spyglass-test-mirror connection buffer)))
                                                   ;; Sharing the terminal advised the renderer.
                                                   (should (spyglass-ghostel-test--advised-p))
                                                   (ghostel--redraw nil)
                                                   (spyglass-test-converge buffer mirror 5)
                                                   ;; The module loaded again drops the advice, which
                                                   ;; a poll gives back.
                                                   (spyglass-ghostel-test--load-module)
                                                   (should-not (spyglass-ghostel-test--advised-p))
                                                   (spyglass--poll)
                                                   (should (spyglass-ghostel-test--advised-p))
                                                   (ghostel--redraw nil)
                                                   (spyglass-test-converge buffer mirror 5)))
                                             (let ((kill-buffer-query-functions nil))
                                               (kill-buffer buffer))))))
      (advice-remove 'ghostel--redraw #'spyglass-ghostel--after-redraw)
      (fmakunbound 'ghostel--redraw))))

(provide 'spyglass-ghostel-test)
;;; spyglass-ghostel-test.el ends here
