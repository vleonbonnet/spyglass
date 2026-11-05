;;; spyglass-bench-agent.el --- The far end of spyglass's benchmarks  -*- lexical-binding: t; -*-

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

;; A batch Emacs the benchmark's driver starts, on this host or on
;; another through SSH.  It listens with a spyglass server registered
;; in server.json of its directory, and shares a buffer, "control",
;; whose input is the driver's commands, as JSON objects; it plays the
;; owner or the viewer of a scenario as told, and answers on standard
;; error, one line each, "SPYBENCH" and a JSON object.  It kills
;; itself when its memory grows past `spyglass-bench-rss-limit', and
;; when no command came for ten minutes.

;;; Code:

(require 'spyglass-bench-common)

(defvar spyglass-bench-agent--last-command (float-time)
  "When the last command came.")

(defvar spyglass-bench-agent--dialed nil
  "The connection this agent dialed to the driver, or nil.")

(defun spyglass-bench-agent--say (plist)
  "Tell the driver PLIST, a line on standard error."
  (message "SPYBENCH %s" (json-serialize plist :null-object nil :false-object :false)))

(defun spyglass-bench-agent--clean (value)
  "Return VALUE, a parsed JSON object, with false as nil."
  (cond ((eq value :false) nil)
        ((and (consp value) (keywordp (car value)))
         (cl-loop for (k v) on value by #'cddr nconc (list k (spyglass-bench-agent--clean v))))
        (t value)))

(defun spyglass-bench-agent--command (text)
  "Run the command TEXT, a JSON object, and answer the driver."
  (setq spyglass-bench-agent--last-command (float-time))
  (let* ((command (spyglass-bench-agent--clean
                   (json-parse-string text :object-type 'plist :false-object :false)))
         (id (plist-get command :id))
         (args (plist-get command :args)))
    (condition-case err
        (pcase (plist-get command :cmd)
          ("prepare"
           (spyglass-bench-agent--say
            (list :id id :result (spyglass-bench-owner-prepare (plist-get command :scenario)
                                                               args))))
          ("run"
           (spyglass-bench-owner-run
            (lambda (result) (spyglass-bench-agent--say (list :id id :result result)))))
          ("open"
           ;; A viewer of terminals has the emulator, and its faces.
           (when (and (plist-get args :kind)
                      (spyglass-bench-terminal-available-p (plist-get args :kind)))
             (spyglass-bench-load-terminal (plist-get args :kind)))
           (let ((buffer (spyglass-bench-viewer-open (car (spyglass-connections)))))
             (spyglass-bench-wait (lambda () (eq (spyglass-mirror-state buffer) 'live)) 60)
             (spyglass-bench-agent--say
              (list :id id :result (list :state (symbol-name (spyglass-mirror-state buffer)))))))
          ("result"
           (spyglass-bench-agent--say (list :id id :result (spyglass-bench-viewer-result))))
          ("show"
           (let ((cells (spyglass-bench-show-mirror (plist-get spyglass-bench--viewer :buffer)
                                                    (plist-get args :cols)
                                                    (plist-get args :rows))))
             (spyglass-bench-agent--say
              (list :id id :result (list :cols (car cells) :rows (cdr cells))))))
          ("chatter"
           (spyglass-bench-chatter (car (spyglass-connections)) (plist-get args :interval))
           (spyglass-bench-agent--say (list :id id :result (list :ok t))))
          ("chan-reset"
           (spyglass-bench-channel-reset (car (spyglass-connections)))
           (spyglass-bench-agent--say (list :id id :result (list :ok t))))
          ("chan-send"
           (spyglass-bench-agent--say
            (list :id id :result (spyglass-bench-channel-send (car (spyglass-connections)) args))))
          ("chan-result"
           (spyglass-bench-agent--say
            (list :id id :result (spyglass-bench-channel-result (car (spyglass-connections))))))
          ("chan-cleanup"
           (spyglass-bench-channel-cleanup)
           (spyglass-bench-agent--say (list :id id :result (list :rss (spyglass-bench-rss)))))
          ("dial"
           ;; The pipes to SSH are sized as the stream is made.
           (when spyglass-bench-agent--dialed
             (spyglass-disconnect spyglass-bench-agent--dialed)
             (setq spyglass-bench-agent--dialed nil))
           (when (plist-get args :pipe)
             (setq spyglass-w32-pipe-size (plist-get args :pipe)))
           (let ((connection (spyglass-connect
                              (spyglass-ssh-transport (plist-get args :host)
                                                      :program (plist-get args :program)
                                                      :args (append (plist-get args :args) nil)
                                                      :file (plist-get args :file))
                              :name "driver")))
             (setq spyglass-bench-agent--dialed connection)
             (spyglass-bench-wait (lambda () (spyglass-connection-live-p connection)) 60)
             (spyglass-bench-agent--say
              (list :id id :result (list :live (if (spyglass-connection-live-p connection) t :false))))))
          ("xfer"
           (let ((connection (if (plist-get args :dialed)
                                 spyglass-bench-agent--dialed
                               (car (last (spyglass-connections))))))
             (spyglass-bench-xfer-receive
              connection args
              (lambda (result) (spyglass-bench-agent--say (list :id id :result result))))))
          ("knobs"
           (setq spyglass-bench-channel-sizes
                 (and (plist-get args :window)
                      (cons (plist-get args :window) (plist-get args :chunk))))
           (spyglass-bench-agent--say (list :id id :result (list :ok t))))
          ("probe-start"
           (spyglass-bench-start-guard 0.5)
           (spyglass-bench-probe-start)
           (spyglass-bench-agent--say (list :id id :result (list :ok t))))
          ("probe-stop"
           (spyglass-bench-start-guard)
           (spyglass-bench-agent--say (list :id id :result (spyglass-bench-probe-stop))))
          ("idle"
           (spyglass-bench-start-guard 0.5)
           (spyglass-bench-probe-start)
           (spyglass-bench-wait (lambda () nil) (plist-get args :seconds))
           (spyglass-bench-start-guard)
           (spyglass-bench-agent--say (list :id id :result (spyglass-bench-probe-stop))))
          ("cleanup"
           (spyglass-bench-chatter nil nil)
           (spyglass-bench-owner-cleanup)
           (spyglass-bench-viewer-cleanup)
           (spyglass-bench-agent--say (list :id id :result (list :rss (spyglass-bench-rss)))))
          ("quit"
           (spyglass-bench-agent--say (list :id id :result (list :bye t)))
           (kill-emacs 0))
          (other (error "Unknown command %S" other)))
      (error (spyglass-bench-agent--say
              (list :id id :error (error-message-string err)))))))

(defvar spyglass-bench-agent--queue nil
  "Commands received and not run yet, oldest first.")

(defun spyglass-bench-agent--input (kind value)
  "Take a command typed into the control buffer: KIND and VALUE.
Commands run from the main loop, not from the process filter nor a
timer: scenarios wait for events, which waiting there would distort."
  (when (eq kind 'string)
    (setq spyglass-bench-agent--queue
          (append spyglass-bench-agent--queue (list value)))))

(defun spyglass-bench-agent-main ()
  "Listen, share the control buffer, and serve the driver until told to quit."
  (setq spyglass-bench-abort-function
        (lambda (msg) (spyglass-bench-agent--say (list :abort msg))))
  (spyglass-bench-read-environment)
  (add-hook 'spyglass-error-functions
            (lambda (_c msg) (spyglass-bench-agent--say (list :warning msg))))
  (spyglass-bench-start-guard)
  (spyglass-bench-xfer-serve)
  (let ((server (spyglass-server-start :file (expand-file-name "server.json"))))
    (with-current-buffer (get-buffer-create "control")
      (spyglass-share (current-buffer) :input #'spyglass-bench-agent--input))
    (spyglass-bench-agent--say (list :ready t :port (spyglass-server-port server)
                                     :system (symbol-name system-type)
                                     :emacs emacs-version)))
  (while (< (- (float-time) spyglass-bench-agent--last-command) 600)
    (spyglass-bench-step)
    (while spyglass-bench-agent--queue
      (spyglass-bench-agent--command (pop spyglass-bench-agent--queue))))
  (kill-emacs 4))

(provide 'spyglass-bench-agent)
;;; spyglass-bench-agent.el ends here
