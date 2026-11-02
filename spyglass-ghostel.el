;;; spyglass-ghostel.el --- Share Ghostel terminals with spyglass  -*- lexical-binding: t; -*-

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

;; Ghostel's native renderer rewrites the buffer with
;; `inhibit-modification-hooks' bound, then publishes the range it
;; rewrote in `ghostel--repainted-region'.  After each render this
;; adapter passes that range to `spyglass-notify'; scrollback trimmed
;; at the top shows as a deletion anyway.  Viewers follow the
;; terminal's cursor and type through Ghostel's key encoder.
;;
;; Ghostel renders only buffers shown in a window: the adapter marks
;; its buffers as needing one, so that spyglass parks a shared
;; terminal no window shows while viewers watch it, and renders there
;; what arrived while it was hidden.  Ghostel sizes its terminal from
;; its windows through `window-adjust-process-window-size-function',
;; which spyglass wraps for viewers' sizes; the adapter makes it fit
;; again on their changes.  Loaded with Ghostel by spyglass.el.

;;; Code:

(require 'spyglass-owner)
(require 'spyglass-window)

(defvar ghostel--repainted-region)
(defvar ghostel--cursor-char-pos)
(defvar ghostel--term)
(defvar ghostel--process)
(defvar ghostel--term-rows)
(defvar ghostel--term-cols)
(defvar ghostel--pending-redraw)
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel--event-key-spec "ghostel" (event &optional meta))
(declare-function ghostel--windows "ghostel" (&optional buffer all-frames))
(declare-function ghostel--adjust-size "ghostel" (window &optional force))
(declare-function ghostel--redraw-now "ghostel" (buffer &optional force))

(defun spyglass-ghostel--after-redraw (&rest _)
  "Tell spyglass what Ghostel's renderer just rewrote in this buffer."
  (let ((share spyglass--share))
    (when (and share (spyglass-share-mode share))
      (let ((region (and (boundp 'ghostel--repainted-region) ghostel--repainted-region)))
        (if (consp region)
            (spyglass-notify (current-buffer) (car region) (cdr region))
          (spyglass-notify (current-buffer) (point-max) (point-max)))))))

(defun spyglass-ghostel--point ()
  "Return the terminal's cursor."
  (and (boundp 'ghostel--cursor-char-pos) ghostel--cursor-char-pos))

(defun spyglass-ghostel--input (kind value)
  "Send the terminal a viewer's input of KIND and VALUE."
  (if (eq kind 'string)
      (ghostel-send-string value)
    (seq-doseq (event (key-parse value))
      (let ((spec (ghostel--event-key-spec event)))
        (unless spec (user-error "Ghostel cannot encode %s" value))
        (ghostel-send-key (car spec) (cdr spec))))))

(defun spyglass-ghostel--size ()
  "Return the terminal's size, (COLUMNS . ROWS)."
  (and (natnump ghostel--term-cols) (natnump ghostel--term-rows)
       (cons ghostel--term-cols ghostel--term-rows)))

(defun spyglass-ghostel--refit ()
  "Resize the terminal to what its windows and viewers call for.
Ghostel resizes on a change of its windows; a viewer's size changes
none, so its resize is asked for here, on a window Ghostel renders
through."
  (when-let* (ghostel--term
              (windows (ghostel--windows (current-buffer) t))
              (size (funcall (default-value 'window-adjust-process-window-size-function)
                             ghostel--process windows)))
    (unless (equal size (spyglass-ghostel--size))
      ;; Forced: Ghostel defers a change of rows alone while the
      ;; minibuffer is active, for the owner's windows.
      (ghostel--adjust-size (car windows) t))))

(defun spyglass-ghostel--parked ()
  "Render what arrived while the terminal was hidden."
  (when ghostel--pending-redraw
    (ghostel--redraw-now (current-buffer))))

(defun spyglass-ghostel--setup (share)
  "Set the input, cursor, size and window needs of a Ghostel SHARE."
  (with-current-buffer (spyglass-share-buffer share)
    (when (derived-mode-p 'ghostel-mode)
      (setq-local spyglass-input-function #'spyglass-ghostel--input
                  spyglass-point-function #'spyglass-ghostel--point
                  spyglass-needs-window t
                  spyglass-park-function #'spyglass-ghostel--parked
                  spyglass-refit-function #'spyglass-ghostel--refit
                  spyglass-terminal-size-function #'spyglass-ghostel--size))))

(advice-add 'ghostel--redraw :after #'spyglass-ghostel--after-redraw)
(add-hook 'spyglass-share-functions #'spyglass-ghostel--setup)

(defun spyglass-ghostel-unload-function ()
  "Remove the advice and hooks of spyglass-ghostel."
  (advice-remove 'ghostel--redraw #'spyglass-ghostel--after-redraw)
  (remove-hook 'spyglass-share-functions #'spyglass-ghostel--setup)
  nil)

(provide 'spyglass-ghostel)
;;; spyglass-ghostel.el ends here
