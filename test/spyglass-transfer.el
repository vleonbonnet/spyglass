;;; spyglass-transfer.el --- Measure transfers between two Emacs processes  -*- lexical-binding: t; -*-

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

;; `make transfer' has the benchmark's agent (see spyglass-bench.el)
;; take payloads from this Emacs, the driver, or the reverse, as
;; drover's clones take a conversation: pieces asked on a channel, a
;; few awaiting their answers at once, answered in base64 (see
;; "Transfers" in spyglass-bench-common.el).  It sweeps the sender's
;; window and chunks, the receiver's `read-process-output-max', and the
;; pipes to SSH on Windows (`spyglass-w32-pipe-size'), each point
;; repeated, the repetitions interleaved, and prints the medians: the
;; rate, how late each end's timers ran (a timer every 10 ms), the
;; receiver's processor time and the peak of its resident memory over
;; what it held before, and whether every byte arrived as sent.
;;
;;   make transfer BENCH_REMOTE=core BENCH_SSH=myssh \
;;     BENCH_REMOTE_EMACS=/ucrt64/bin/emacs
;;
;; Variables, space-separated lists:
;;
;; - XFER_DIRS: "to", the agent taking from the driver, and "from".
;; - XFER_PATHS: "out", the driver dialing the agent through `BENCH_SSH
;;   -W' (the driver's end of the stream a pipe to SSH, the agent's a
;;   socket), and "back", the agent dialing the driver through
;;   XFER_BACK_SSH there, with XFER_BACK_ARGS, comma-separated, to
;;   XFER_BACK_HOST (the agent's end a pipe, the driver's a socket).
;; - XFER_WINDOWS and XFER_CHUNKS, in KB: the sender's window and chunk
;;   size, in place of those spyglass chooses (`spyglass--channel-sizes'),
;;   a chunk larger than the window cut to it, a chunk of 0 a quarter of
;;   the window, at most 256; a window of 0 is spyglass's choice.
;; - XFER_READ_MAX, in KB: the receiver's reads, when the agent
;;   receives; XFER_PIPE, in KB: the agent's pipes to SSH dialing back.
;; - XFER_SIZES, in MB; XFER_PIECE, in KB, and XFER_FLIGHT, the
;;   requests: 256 and 4 by default, as drover's clones; XFER_REPS.
;;
;; Every Emacs kills itself past 2 GB of resident memory.

;;; Code:

(require 'spyglass-bench)

(defun spyglass-transfer--numbers (name default)
  "Return the numbers of the environment variable NAME, else DEFAULT."
  (let ((value (spyglass-bench--getenv name)))
    (if value (mapcar #'string-to-number (split-string value)) default)))

(defun spyglass-transfer--words (name default)
  "Return the words of the environment variable NAME, else DEFAULT."
  (let ((value (spyglass-bench--getenv name)))
    (if value (split-string value) default)))

(defvar spyglass-transfer--pipe nil
  "The pipe size, in KB, of the agent's stream dialed back, or nil.")

(defun spyglass-transfer--dial-back (pipe)
  "Have the agent dial this Emacs back, its pipes PIPE KB."
  (unless (eql pipe spyglass-transfer--pipe)
    (let ((result (spyglass-bench--command
                   "dial" :args (list :host (spyglass-bench--getenv "XFER_BACK_HOST" (system-name))
                                      :program (spyglass-bench--getenv "XFER_BACK_SSH" "ssh")
                                      :args (vconcat
                                             (split-string (spyglass-bench--getenv "XFER_BACK_ARGS" "")
                                                           "," t))
                                      :file (spyglass-server-file spyglass--server)
                                      :pipe (* 1024 pipe)))))
      (unless (eq (plist-get result :live) t)
        (error "The agent could not dial back"))
      (setq spyglass-transfer--pipe pipe))))

(defun spyglass-transfer--args (point)
  "Return the receiver's arguments for the transfer POINT."
  (pcase-let ((`(,_dir ,path ,size ,_window ,_chunk ,read-max ,_pipe) point))
    (list :total (round (* size 1024 1024))
          :piece (* 1024 (string-to-number (spyglass-bench--getenv "XFER_PIECE" "256")))
          :flight (string-to-number (spyglass-bench--getenv "XFER_FLIGHT" "4"))
          :read-max (* 1024 read-max)
          :dialed (if (equal path "back") t :false))))

(defun spyglass-transfer--accepted ()
  "Return the connection the agent dialed, as this Emacs accepted it."
  (seq-find (lambda (connection) (not (eq connection spyglass-bench--connection)))
            (spyglass-connections)))

(defun spyglass-transfer--run (point)
  "Run the transfer POINT describes; return its measurements.
They are the receiver's, with :core and :mac the lateness of each
end's timers."
  (pcase-let ((`(,dir ,path ,_size ,window ,chunk ,_read-max ,pipe) point))
    ;; A window of 0 leaves the choice to spyglass.
    (if (equal dir "to")
        (setq spyglass-bench-channel-sizes
              (and (> window 0) (cons (* 1024 window) (* 1024 chunk))))
      (spyglass-bench--command "knobs" :args (if (> window 0)
                                                 (list :window (* 1024 window)
                                                       :chunk (* 1024 chunk))
                                               (list :window :false))))
    (when (equal path "back")
      (spyglass-transfer--dial-back pipe))
    (garbage-collect)
    (if (equal dir "to")
        (progn
          (spyglass-bench-probe-start)
          (let* ((result (spyglass-bench--command "xfer" :args (spyglass-transfer--args point)))
                 (mac (spyglass-bench-probe-stop)))
            (append (list :core (plist-get result :late) :mac mac) result)))
      (spyglass-bench--command "probe-start")
      (let ((result nil))
        (spyglass-bench-xfer-receive
         (if (equal path "back") (spyglass-transfer--accepted) spyglass-bench--connection)
         ;; This end's reads stay as they are: the agent's are swept.
         (plist-put (spyglass-transfer--args point) :read-max nil)
         (lambda (r) (setq result r)))
        (spyglass-bench-wait (lambda () result) 580)
        (let ((core (spyglass-bench--command "probe-stop")))
          (unless result (error "The transfer did not end"))
          (append (list :core core :mac (plist-get result :late)) result))))))

(defun spyglass-transfer--log (point result)
  "Log RESULT of the transfer POINT."
  (spyglass-bench--log "%S: %.1f MB/s, Core late %.1f/%.1f/%.1f ms, Reaver %.1f ms, %s"
                       point (/ (nth 2 point) (plist-get result :time))
                       (* 1000 (plist-get (plist-get result :core) :p95))
                       (* 1000 (plist-get (plist-get result :core) :p99))
                       (* 1000 (plist-get (plist-get result :core) :max))
                       (* 1000 (plist-get (plist-get result :mac) :max))
                       (if (eq (plist-get result :exact) t) "exact" "NOT EXACT")))

(defun spyglass-transfer--median (values)
  "Return the median of VALUES."
  (let ((sorted (sort (copy-sequence values) #'<)))
    (nth (/ (length sorted) 2) sorted)))

(defun spyglass-transfer--row (point results)
  "Return the table row of POINT from its RESULTS, medians."
  (pcase-let ((`(,dir ,path ,size ,window ,chunk ,read-max ,pipe) point))
    (cl-flet ((med (f) (spyglass-transfer--median (mapcar f results)))
              (ms (s) (format "%.1f" (* 1000 s))))
      (list (if (equal dir "to") "→agent" "agent→") path (format "%g" size)
            (if (> window 0) (format "%d" window) "auto")
            (if (> window 0) (format "%d" chunk) "auto")
            (format "%d" read-max) (if (equal path "back") (format "%d" pipe) "-")
            (format "%.1f" (med (lambda (r) (/ size (plist-get r :time)))))
            (format "%s/%s/%s"
                    (ms (med (lambda (r) (plist-get (plist-get r :core) :p95))))
                    (ms (med (lambda (r) (plist-get (plist-get r :core) :p99))))
                    (ms (med (lambda (r) (plist-get (plist-get r :core) :max)))))
            (ms (med (lambda (r) (plist-get (plist-get r :mac) :max))))
            (format "%.2f" (med (lambda (r) (plist-get r :cpu))))
            (format "%d" (/ (med (lambda (r) (- (plist-get r :rss-peak) (plist-get r :rss)))) 1024))
            (format "%d/%d" (cl-count-if (lambda (r) (eq (plist-get r :exact) t)) results)
                    (length results))))))

(defun spyglass-transfer-main ()
  "Sweep the transfers and print their table."
  (setq spyglass-bench--remote (spyglass-bench--getenv "SPYGLASS_BENCH_REMOTE"))
  (spyglass-bench-read-environment)
  (spyglass-bench-start-guard)
  (add-hook 'spyglass-error-functions
            (lambda (_c msg) (spyglass-bench--log "error: %s" msg)))
  (let* ((dirs (spyglass-transfer--words "XFER_DIRS" '("to")))
         (paths (spyglass-transfer--words "XFER_PATHS" '("out")))
         (sizes (spyglass-transfer--numbers "XFER_SIZES" '(10)))
         (windows (spyglass-transfer--numbers "XFER_WINDOWS" '(64 128 256 512 1024 2048 4096)))
         (chunks (spyglass-transfer--numbers "XFER_CHUNKS" '(256)))
         (read-maxes (spyglass-transfer--numbers "XFER_READ_MAX" '(64)))
         (pipes (spyglass-transfer--numbers "XFER_PIPE" '(64)))
         (reps (car (spyglass-transfer--numbers "XFER_REPS" '(3))))
         (points nil) (results (make-hash-table :test #'equal)) (failed nil)
         (dir (make-temp-file "spyglass-transfer-" t))
         (idle nil))
    (dolist (path paths)
      (dolist (direction dirs)
        (dolist (size sizes)
          (dolist (pipe (if (equal path "back") pipes '(0)))
            (dolist (read-max read-maxes)
              (dolist (chunk chunks)
                (dolist (window windows)
                  (push (list direction path size window
                              (cond ((= window 0) 0)
                                    ((= chunk 0) (min 256 (max 4 (/ window 4))))
                                    (t (min chunk window)))
                              read-max pipe)
                        points))))))))
    (setq points (delete-dups (nreverse points)))
    (unwind-protect
        (progn
          (spyglass-bench--start-agent)
          (spyglass-bench-xfer-serve)
          (when (member "back" paths)
            (spyglass-server-start :file (expand-file-name "server.json" dir)))
          (push (spyglass-bench--command "idle" :args (list :seconds 3)) idle)
          (dotimes (rep reps)
            ;; Each repetition in another order, each point among the others.
            (let ((order (append (nthcdr (% rep (length points)) points)
                                 (take (% rep (length points)) points))))
              (dolist (point order)
                (condition-case err
                    (let ((result (spyglass-transfer--run point)))
                      (spyglass-transfer--log point result)
                      (push result (gethash point results)))
                  (error (push point failed)
                         (spyglass-bench--log "%S failed: %s" point (error-message-string err)))))))
          (push (spyglass-bench--command "idle" :args (list :seconds 3)) idle))
      (spyglass-bench--stop-agent)
      (when spyglass--server (spyglass-server-stop))
      (delete-directory dir t))
    (princ (format "\nspyglass transfers, %s, between %s (Emacs %s) and %s (Emacs %s), pieces of %s KB, %s at once, %d runs each\n"
                   (format-time-string "%F %R") (system-name) emacs-version
                   (or spyglass-bench--remote "localhost")
                   (plist-get spyglass-bench--ready :emacs)
                   (spyglass-bench--getenv "XFER_PIECE" "256")
                   (spyglass-bench--getenv "XFER_FLIGHT" "4") reps))
    (when idle
      (princ (format "The agent idle, before and after: timers late %s ms (p95/p99/max)\n\n"
                     (mapconcat (lambda (late)
                                  (format "%.1f/%.1f/%.1f" (* 1000 (plist-get late :p95))
                                          (* 1000 (plist-get late :p99)) (* 1000 (plist-get late :max))))
                                (reverse idle) " and "))))
    (spyglass-bench--print-table-of
     '("dir" "path" "MB" "window KB" "chunk KB" "read KB" "pipe KB" "MB/s"
       "agent late ms p95/p99/max" "driver late ms max" "receiver CPU s" "receiver RSS +MB" "exact")
     (cl-loop for point in points
              for runs = (gethash point results)
              when runs collect (spyglass-transfer--row point runs)))
    (when spyglass-bench--warnings
      (princ (format "\nAgent warnings: %S\n" (delete-dups spyglass-bench--warnings))))
    (when failed
      (princ (format "\nFAILED: %S\n" (delete-dups failed))))
    (kill-emacs (if failed 1 0))))

(provide 'spyglass-transfer)
;;; spyglass-transfer.el ends here
