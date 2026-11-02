;;; spyglass.el --- Mirror live buffers between Emacs instances  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Maintainer: Valentin Leon <valentin@leon.click>
;; Keywords: comm, terminals, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/vleonbonnet/spyglass

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

;; Spyglass mirrors live buffers from one Emacs into another, on the
;; same host or across SSH: an owner shares buffers, a viewer opens
;; read-only mirrors of them that follow every change, with faces,
;; overlays and the owner's cursor, and sends the keys typed in them
;; back.  It mirrors ordinary buffers, comint shells and terminals
;; (Eat, Ghostel and vterm, which write without modification hooks),
;; and resumes mirrors when a broken connection comes back.
;;
;; On the owner:
;;
;;   (spyglass-server-start)            ; listen on 127.0.0.1
;;   (spyglass-share (get-buffer "*shell*"))
;;
;; On the viewer, through SSH:
;;
;;   (setq c (spyglass-connect (spyglass-ssh-transport "host")))
;;   (spyglass-open c "*shell*")        ; returns the mirror's buffer
;;
;; or interactively: `spyglass-share-current-buffer', `spyglass-start-server',
;; `spyglass-connect-host' and `spyglass-open-share'.  See README.org
;; for the API and doc/design.org for the protocol.

;;; Code:

(require 'spyglass-wire)
(require 'spyglass-conn)
(require 'spyglass-owner)
(require 'spyglass-mirror)

(with-eval-after-load 'eat
  (require 'spyglass-eat))

(with-eval-after-load 'ghostel
  (require 'spyglass-ghostel))

(with-eval-after-load 'vterm
  (require 'spyglass-vterm))

;;;; Commands

;;;###autoload
(defun spyglass-share-current-buffer (&optional read-only)
  "Share the current buffer; with prefix argument READ-ONLY, refuse input.
Starts the server if it does not run."
  (interactive "P")
  (unless (spyglass-server)
    (spyglass-server-start))
  (spyglass-share (current-buffer) :read-only (and read-only t))
  (message "Sharing %s%s on port %d" (buffer-name)
           (if read-only " read-only" "")
           (spyglass-server-port (spyglass-server))))

;;;###autoload
(defun spyglass-unshare-current-buffer ()
  "Stop sharing the current buffer."
  (interactive)
  (spyglass-unshare (current-buffer)))

;;;###autoload
(defun spyglass-start-server ()
  "Start the spyglass server and say where it listens."
  (interactive)
  (let ((server (spyglass-server-start)))
    (message "Spyglass listens on 127.0.0.1:%d, registered in %s"
             (spyglass-server-port server)
             (abbreviate-file-name (or (spyglass-server-file server) "no file")))))

(defvar spyglass--host-history nil
  "Hosts given to `spyglass-connect-host'.")

;;;###autoload
(defun spyglass-connect-host (host)
  "Connect to the spyglass server of HOST through SSH, or of this host.
HOST \"localhost\" connects locally through the registration file."
  (interactive (list (read-string "Host: " nil 'spyglass--host-history)))
  (let ((connection
         (if (member host '("localhost" "127.0.0.1" ""))
             (spyglass-connect (spyglass-file-transport) :name "localhost")
           (spyglass-connect (spyglass-ssh-transport host) :name host))))
    (message "Connecting to %s..." (spyglass-connection-label connection))
    connection))

(defun spyglass--read-connection ()
  "Read a live connection, or return the only one."
  (let ((connections (cl-remove-if-not #'spyglass-connection-live-p
                                       (spyglass-connections))))
    (cond
     ((null connections) (user-error "No spyglass connection is open"))
     ((null (cdr connections)) (car connections))
     (t (let* ((names (mapcar (lambda (c) (cons (spyglass-connection-label c) c))
                              connections)))
          (cdr (assoc (completing-read "Connection: " names nil t) names)))))))

;;;###autoload
(defun spyglass-open-share (connection share-id)
  "Open a mirror of SHARE-ID, shared by CONNECTION's peer, and show it."
  (interactive
   (let* ((connection (spyglass--read-connection))
          (shares (mapcar (lambda (s) (plist-get s :id))
                          (spyglass-connection-shares connection))))
     (unless shares (user-error "%s shares nothing" (spyglass-connection-label connection)))
     (list connection (completing-read "Share: " shares nil t))))
  (pop-to-buffer (spyglass-open connection share-id)))

;;;###autoload
(defun spyglass-disconnect-command (connection)
  "Close CONNECTION for good."
  (interactive (list (spyglass--read-connection)))
  (spyglass-disconnect connection))

(provide 'spyglass)
;;; spyglass.el ends here
