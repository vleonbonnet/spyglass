;;; spyglass-vterm.el --- Share vterm terminals with spyglass  -*- lexical-binding: t; -*-

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

;; vterm's module redraws through ordinary insertions, deletions and
;; `put-text-property', which run modification hooks, so spyglass
;; captures it without help; its colors are `font-lock-face'
;; properties, mirrored as faces.  This adapter makes viewers type
;; through vterm's key translation.  Loaded with vterm by spyglass.el.

;;; Code:

(require 'spyglass-owner)

(defvar vterm--term)
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function vterm-send "vterm" (key))
(declare-function vterm-send-return "vterm" ())

(defun spyglass-vterm--input (kind value)
  "Send the terminal a viewer's input of KIND and VALUE."
  (unless vterm--term (user-error "The terminal has exited"))
  (cond
   ((eq kind 'string) (vterm-send-string value))
   ((member value '("RET" "<return>")) (vterm-send-return))
   (t (vterm-send value))))

(defun spyglass-vterm--setup (share)
  "Set the input of SHARE when it is a vterm terminal."
  (with-current-buffer (spyglass-share-buffer share)
    (when (derived-mode-p 'vterm-mode)
      (setq-local spyglass-input-function #'spyglass-vterm--input))))

(add-hook 'spyglass-share-functions #'spyglass-vterm--setup)

(defun spyglass-vterm-unload-function ()
  "Remove the hooks of spyglass-vterm."
  (remove-hook 'spyglass-share-functions #'spyglass-vterm--setup)
  nil)

(provide 'spyglass-vterm)
;;; spyglass-vterm.el ends here
