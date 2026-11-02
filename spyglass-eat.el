;;; spyglass-eat.el --- Share Eat terminals with spyglass  -*- lexical-binding: t; -*-

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

;; Eat writes its terminal with `inhibit-modification-hooks' bound, in
;; `eat--process-output-queue' and when a window resizes it.  Around
;; those, this adapter tells spyglass where the writes went: from the
;; start of the terminal's display, as it was or is, to the end; the
;; scrollback Eat trims at the top shows as a deletion anyway.  It
;; also makes viewers follow the terminal's cursor and type through
;; Eat's input functions.  Eat sizes its terminal through its process's
;; `adjust-window-size-function', which asks
;; `window-adjust-process-window-size-function': viewers' sizes reach
;; it there, and `spyglass-refit-process' applies them to a terminal
;; no window shows.  Loaded with Eat by spyglass.el.

;;; Code:

(require 'spyglass-owner)
(require 'spyglass-window)

(defvar eat-terminal)
(declare-function eat-term-display-beginning "eat" (terminal))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat-term-size "eat" (terminal))
(declare-function eat-term-send-string "eat" (terminal string))
(declare-function eat-term-input-event "eat" (terminal n event &optional ref-pos))

(defun spyglass-eat--shared-p (buffer)
  "Return non-nil when BUFFER is an Eat terminal someone subscribes to."
  (and (buffer-live-p buffer)
       (let ((share (buffer-local-value 'spyglass--share buffer)))
         (and share (spyglass-share-mode share)))
       (buffer-local-value 'eat-terminal buffer)))

(defun spyglass-eat--around-write (function buffer &rest args)
  "Call FUNCTION with BUFFER and ARGS, then notify spyglass of BUFFER's change."
  (if (not (spyglass-eat--shared-p buffer))
      (apply function buffer args)
    (let ((start (with-current-buffer buffer
                   (copy-marker (eat-term-display-beginning eat-terminal)))))
      (unwind-protect
          (apply function buffer args)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((now (if eat-terminal
                           (eat-term-display-beginning eat-terminal)
                         (point-min))))
              ;; Eat joins the line before the display's old start.
              (spyglass-notify buffer (max (point-min) (1- (min start now)))
                               (point-max)))))
        (set-marker start nil)))))

(defun spyglass-eat--around-resize (function process windows)
  "Call FUNCTION with PROCESS and WINDOWS, notifying spyglass of the redraw."
  (spyglass-eat--around-write
   (lambda (_buffer) (funcall function process windows))
   (process-buffer process)))

(defun spyglass-eat--point ()
  "Return the terminal's cursor."
  (and eat-terminal (eat-term-display-cursor eat-terminal)))

(defun spyglass-eat--input (kind value)
  "Send the terminal a viewer's input of KIND and VALUE."
  (unless eat-terminal (user-error "The terminal has exited"))
  (if (eq kind 'string)
      (eat-term-send-string eat-terminal value)
    (let ((events (key-parse value)))
      (seq-doseq (event events)
        (eat-term-input-event eat-terminal 1 event)))))

(defun spyglass-eat--size ()
  "Return the terminal's size, (COLUMNS . ROWS)."
  (and eat-terminal (eat-term-size eat-terminal)))

(defun spyglass-eat--setup (share)
  "Set the input, cursor and size of SHARE when it is an Eat terminal."
  (with-current-buffer (spyglass-share-buffer share)
    (when (derived-mode-p 'eat-mode)
      (setq-local spyglass-input-function #'spyglass-eat--input
                  spyglass-point-function #'spyglass-eat--point
                  spyglass-refit-function #'spyglass-refit-process
                  spyglass-terminal-size-function #'spyglass-eat--size))))

(advice-add 'eat--process-output-queue :around #'spyglass-eat--around-write)
(advice-add 'eat--adjust-process-window-size :around #'spyglass-eat--around-resize)
(add-hook 'spyglass-share-functions #'spyglass-eat--setup)

(defun spyglass-eat-unload-function ()
  "Remove the advice and hooks of spyglass-eat."
  (advice-remove 'eat--process-output-queue #'spyglass-eat--around-write)
  (advice-remove 'eat--adjust-process-window-size #'spyglass-eat--around-resize)
  (remove-hook 'spyglass-share-functions #'spyglass-eat--setup)
  nil)

(provide 'spyglass-eat)
;;; spyglass-eat.el ends here
