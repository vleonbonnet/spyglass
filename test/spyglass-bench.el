;;; spyglass-bench.el --- Measure spyglass between two Emacs processes  -*- lexical-binding: t; -*-

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

;; `make bench' plays the scenarios of spyglass-bench-common.el
;; between this batch Emacs, the driver, and an agent, another batch
;; Emacs it starts (test/spyglass-bench-agent.el): on this host,
;; through 127.0.0.1, or with BENCH_REMOTE=HOST on another host,
;; through `BENCH_SSH -W', the agent's files copied to a temporary
;; directory there and removed after.
;;
;;   make bench
;;   make bench BENCH_REMOTE=core BENCH_SSH=myssh \
;;     BENCH_REMOTE_EMACS=/ucrt64/bin/emacs
;;
;; Each scenario runs in both directions, the agent owning and the
;; driver viewing (agent→driver) and the reverse (driver→agent), and
;; ends by comparing the digests of the owner's buffer and of the
;; mirror: text, mirrored properties and overlays.  The table gives,
;; per scenario:
;;
;; - time: for large buffers the snapshot, from the subscription to
;;   the last frame acknowledged; else the producer's run, then the
;;   drain until the viewer acknowledged everything;
;; - wire: bytes the owner sent;
;; - latency: from a change on the owner to its update applied on the
;;   viewer, through the estimated clock offset, median and 99th
;;   percentile; and to its acknowledgment back on the owner;
;; - tick: main-thread time of the owner's captures and sends, median,
;;   99th percentile and maximum;
;; - CPU: the owner's processor time during the scenario, and the
;;   producer's alone on a buffer no one sees, when measured;
;; - GC: garbage collections and their time on owner and viewer;
;; - RSS: peak resident memory of owner and viewer.
;;
;; The resize scenario has the viewer show its mirror in windows of
;; four sizes, and the owner's terminal take them: its time is the
;; total, its latency (median and maximum) from showing a window to the
;; owner's terminal taking its size when the driver owns, to the
;; driver's mirror learning it back when the agent owns; `exact' says
;; each size was reached (sN for N sizes) besides the digests.
;;
;; BENCH_ONLY, a space-separated list of scenario names, restricts the
;; run.  Every Emacs kills itself past 2 GB of resident memory.

;;; Code:

(require 'spyglass-bench-common)

(declare-function profiler-reset "profiler" ())
(declare-function profiler-report "profiler" ())
(declare-function profiler-report-render-reversed-calltree "profiler" ())
(declare-function profiler-report-expand-entry "profiler" (&optional full))

(defvar spyglass-bench--agent nil
  "Process of the agent.")

(defvar spyglass-bench--replies (make-hash-table)
  "Replies of the agent by command identifier.")

(defvar spyglass-bench--next-id 0
  "Last command identifier.")

(defvar spyglass-bench--ready nil
  "The agent's ready message, once received.")

(defvar spyglass-bench--abort nil
  "Why the agent aborted, if it did.")

(defvar spyglass-bench--warnings nil
  "Warnings the agent reported.")

(defvar spyglass-bench--connection nil
  "Connection to the agent.")

(defvar spyglass-bench--control nil
  "Mirror of the agent's control buffer.")

(defvar spyglass-bench--remote nil
  "Host of the agent, or nil for this one.")

(defvar spyglass-bench--remote-dir nil
  "Directory of the agent on its host.")

(defun spyglass-bench--getenv (name &optional default)
  "Return environment variable NAME, or DEFAULT when unset or empty."
  (let ((v (getenv name)))
    (if (and v (not (string-empty-p v))) v default)))

(defun spyglass-bench--log (format-string &rest args)
  "Print FORMAT-STRING with ARGS, as progress."
  (apply #'message (concat "bench: " format-string) args))

;;;; The agent

(defun spyglass-bench--agent-filter (_process output)
  "Read the agent's lines in OUTPUT."
  (let ((pending (concat (or (get 'spyglass-bench--agent 'pending) "") output)))
    (while (string-match "\n" pending)
      (let ((line (string-trim-right (substring pending 0 (match-beginning 0)) "\r")))
        (setq pending (substring pending (match-end 0)))
        (if (string-prefix-p "SPYBENCH " line)
            (let ((msg (condition-case nil
                           (json-parse-string (substring line 9) :object-type 'plist
                                              :false-object nil :null-object nil)
                         (error nil))))
              (cond
               ((plist-get msg :ready) (setq spyglass-bench--ready msg))
               ((plist-get msg :abort) (setq spyglass-bench--abort (plist-get msg :abort))
                (spyglass-bench--log "agent aborted: %s" spyglass-bench--abort))
               ((plist-get msg :warning)
                (push (plist-get msg :warning) spyglass-bench--warnings))
               ((plist-get msg :id) (puthash (plist-get msg :id) msg spyglass-bench--replies))))
          (unless (string-empty-p line)
            (spyglass-bench--log "agent: %s" line)))))
    (put 'spyglass-bench--agent 'pending pending)))

(defun spyglass-bench--ssh ()
  "Return the SSH program."
  (spyglass-bench--getenv "SPYGLASS_BENCH_SSH" "ssh"))

(defun spyglass-bench--remote-run (command)
  "Run COMMAND on the remote host and return its output, trimmed."
  (with-temp-buffer
    (let ((status (call-process (spyglass-bench--ssh) nil t nil "-o" "BatchMode=yes"
                                spyglass-bench--remote command)))
      (unless (eql status 0)
        (error "Remote command failed (%s): %s: %s" status command (buffer-string)))
      (string-trim (buffer-string)))))

(defun spyglass-bench--remote-copy (file dir)
  "Copy FILE to DIR on the remote host."
  (let ((status (call-process "sh" nil nil nil "-c"
                              (format "cat %s | %s -o BatchMode=yes %s %s"
                                      (shell-quote-argument file)
                                      (shell-quote-argument (spyglass-bench--ssh))
                                      (shell-quote-argument spyglass-bench--remote)
                                      (shell-quote-argument
                                       (format "cat > %s/%s" dir
                                               (file-name-nondirectory file)))))))
    (unless (eql status 0) (error "Copying %s failed" file))))

(defun spyglass-bench--sources ()
  "Return the library's files and the agent's."
  (append (cl-remove-if (lambda (f) (string-match-p "-\\(pkg\\|autoloads\\)\\.el\\'" f))
                        (file-expand-wildcards "spyglass*.el"))
          (list "test/spyglass-bench-common.el" "test/spyglass-bench-agent.el")))

(defun spyglass-bench--start-agent ()
  "Start the agent, here or on the remote host, and wait until it listens."
  (let* ((timeout (spyglass-bench--getenv "SPYGLASS_TIMEOUT" "600"))
         (process-environment (copy-sequence process-environment)))
    (setq spyglass-bench--ready nil spyglass-bench--abort nil)
    (if spyglass-bench--remote
        (let* ((dir (spyglass-bench--remote-run "mktemp -d /tmp/spyglass-bench-XXXXXX"))
               (emacs (spyglass-bench--getenv "SPYGLASS_BENCH_REMOTE_EMACS" "emacs")))
          (setq spyglass-bench--remote-dir dir)
          (spyglass-bench--log "copying to %s:%s" spyglass-bench--remote dir)
          (dolist (file (spyglass-bench--sources))
            (spyglass-bench--remote-copy file dir))
          (spyglass-bench--remote-run
           (format "cd %s && timeout 300 %s -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile spyglass*.el spyglass-bench-common.el 2>&1"
                   dir emacs))
          (setq spyglass-bench--agent
                (make-process
                 :name "agent" :connection-type 'pipe :noquery t
                 :command (list (spyglass-bench--ssh) "-o" "BatchMode=yes"
                                spyglass-bench--remote
                                (format "cd %s && SPYGLASS_BENCH_WINDOW=%s exec timeout %s %s -Q --batch -L . -l spyglass-bench-agent.el -f spyglass-bench-agent-main"
                                        dir (or (getenv "SPYGLASS_BENCH_WINDOW") "")
                                        timeout emacs))
                 :filter #'spyglass-bench--agent-filter)))
      (let ((dir (make-temp-file "spyglass-bench-" t))
            (root default-directory))
        (setq spyglass-bench--remote-dir dir)
        (setq spyglass-bench--agent
              (let ((default-directory dir))
                (make-process
                 :name "agent" :connection-type 'pipe :noquery t
                 :command (list "sh" "-c"
                                (format "trap '' PIPE; exec timeout %s %s -Q --batch -L %s -L %s --eval '(setq load-prefer-newer t)' -l spyglass-bench-agent.el -f spyglass-bench-agent-main"
                                        timeout
                                        (shell-quote-argument
                                         (spyglass-bench--getenv "SPYGLASS_EMACS" "emacs"))
                                        (shell-quote-argument root)
                                        (shell-quote-argument (expand-file-name "test" root))))
                 :filter #'spyglass-bench--agent-filter)))))
    (unless (spyglass-bench-wait (lambda () (or spyglass-bench--ready
                                                (not (process-live-p spyglass-bench--agent))))
                                 120)
      (error "The agent did not start"))
    (unless spyglass-bench--ready (error "The agent exited"))
    (spyglass-bench--log "agent ready: %S" spyglass-bench--ready)
    (setq spyglass-bench--connection
          (spyglass-connect
           (if spyglass-bench--remote
               (spyglass-ssh-transport spyglass-bench--remote
                                       :program (spyglass-bench--ssh)
                                       :file (concat spyglass-bench--remote-dir "/server.json"))
             (spyglass-file-transport
              (expand-file-name "server.json" spyglass-bench--remote-dir)))
           :name (or spyglass-bench--remote "localhost")))
    (unless (spyglass-bench-wait (lambda () (spyglass-connection-live-p spyglass-bench--connection))
                                 60)
      (error "Cannot connect to the agent"))
    (setq spyglass-bench--control (spyglass-open spyglass-bench--connection "control"))
    (spyglass-bench-wait (lambda () (eq (spyglass-mirror-state spyglass-bench--control) 'live)) 60)
    ;; A few heartbeats estimate the clock offset.
    (dotimes (_ 5)
      (spyglass--ping spyglass-bench--connection)
      (spyglass-bench-wait (lambda () nil) 0.05))))

(defun spyglass-bench--command (cmd &rest plist)
  "Send the agent CMD with PLIST; wait for its result and return it."
  (spyglass-bench-wait (lambda () (or (eq (spyglass-mirror-state spyglass-bench--control) 'live)
                                      spyglass-bench--abort))
                       120)
  (when spyglass-bench--abort (error "Agent aborted: %s" spyglass-bench--abort))
  (let ((id (cl-incf spyglass-bench--next-id)))
    (spyglass-mirror-send-string
     (json-serialize (append (list :id id :cmd cmd) plist) :null-object nil :false-object :false)
     spyglass-bench--control)
    (spyglass-bench-wait (lambda () (or (gethash id spyglass-bench--replies)
                                        spyglass-bench--abort
                                        (not (process-live-p spyglass-bench--agent))))
                         580)
    (let ((reply (gethash id spyglass-bench--replies)))
      (cond
       (spyglass-bench--abort (error "Agent aborted: %s" spyglass-bench--abort))
       ((null reply) (error "No reply to %s" cmd))
       ((plist-get reply :error) (error "Agent: %s" (plist-get reply :error)))
       (t (plist-get reply :result))))))

(defun spyglass-bench--stop-agent ()
  "Stop the agent and remove its directory."
  (ignore-errors (spyglass-bench--command "quit"))
  (when spyglass-bench--connection (ignore-errors (spyglass-disconnect spyglass-bench--connection)))
  (spyglass-bench-wait (lambda () (not (process-live-p spyglass-bench--agent))) 10)
  (when (process-live-p spyglass-bench--agent) (delete-process spyglass-bench--agent))
  (if spyglass-bench--remote
      (when spyglass-bench--remote-dir
        (ignore-errors (spyglass-bench--remote-run
                        (format "rm -rf %s" spyglass-bench--remote-dir))))
    (when spyglass-bench--remote-dir (delete-directory spyglass-bench--remote-dir t))))

;;;; Scenarios

(defun spyglass-bench--scenarios ()
  "Return the scenarios to run: (NAME SCENARIO ARGS OPTIONS)."
  (let ((all
         `(("large-10M" "large" (:size 10000000) nil)
           ("large-100M" "large" (:size 100000000) nil)
           ("appends-100k" "appends" (:count 100000 :baseline t) nil)
           ("stream-50MB/s" "stream" (:rate 50000000 :seconds 4 :keep 8000000 :baseline t) nil)
           ("term-80x200" "term" (:rows 80 :cols 200 :hz 60 :seconds 5 :hint t :baseline t) nil)
           ("term-50x300" "term" (:rows 50 :cols 300 :hz 60 :seconds 5 :hint t :baseline t) nil)
           ("term-80x200-unhinted" "term" (:rows 80 :cols 200 :hz 60 :seconds 5 :hint nil
                                                 :baseline t)
            nil)
           ("overlays-2000" "overlays" (:paragraphs 2000 :baseline t) nil)
           ("eat-seq-300k" "terminal" (:kind "eat" :count 300000) (:terminal "eat"))
           ("vterm-seq-300k" "terminal" (:kind "vterm" :count 300000) (:terminal "vterm"))
           ("ghostel-seq-300k" "terminal" (:kind "ghostel" :count 300000)
            (:terminal "ghostel"))
           ("reconnect-appends" "appends" (:count 100000) (:drops 3))
           ("resize" "resize" nil (:resize t))))
        (only (spyglass-bench--getenv "SPYGLASS_BENCH_ONLY")))
    (if only
        (let ((names (split-string only)))
          (cl-remove-if-not (lambda (s) (member (car s) names)) all))
      all)))

(defvar spyglass-bench--drop-timers nil
  "Timers breaking the stream during a scenario.")

(defun spyglass-bench--drops (count)
  "Break the connection's stream COUNT times, 0.3 s apart."
  (dotimes (i count)
    (push (run-with-timer (+ 0.2 (* 0.3 i)) nil
                          (lambda ()
                            (when (spyglass-connection-live-p spyglass-bench--connection)
                              (spyglass-bench--log "dropping the stream")
                              (spyglass-connection-drop-stream spyglass-bench--connection))))
          spyglass-bench--drop-timers)))

(defun spyglass-bench--cancel-drops ()
  "Cancel the drops not done yet."
  (mapc #'cancel-timer spyglass-bench--drop-timers)
  (setq spyglass-bench--drop-timers nil))

(defun spyglass-bench--json-args (args)
  "Return ARGS with nil as false, for JSON."
  (cl-loop for (k v) on args by #'cddr nconc (list k (if (eq v nil) :false v))))

(defun spyglass-bench--agent-owns (scenario args options)
  "Play SCENARIO with ARGS and OPTIONS, the agent owning; return measurements."
  ;; A viewer of terminals has the emulator, and its faces, as the owner does.
  (when (and (plist-get options :terminal)
             (spyglass-bench-terminal-available-p (plist-get options :terminal)))
    (spyglass-bench-load-terminal (plist-get options :terminal)))
  (let ((prepared (spyglass-bench--command "prepare" :scenario scenario
                                           :args (spyglass-bench--json-args args))))
    (let ((mirror (spyglass-bench-viewer-open spyglass-bench--connection)))
      (spyglass-bench-wait (lambda () (eq (spyglass-mirror-state mirror) 'live)) 120)
      (when (plist-get options :drops) (spyglass-bench--drops (plist-get options :drops)))
      (let* ((owner (prog1 (spyglass-bench--command "run") (spyglass-bench--cancel-drops)))
             (_ (spyglass-bench-wait (lambda () (eq (spyglass-mirror-state mirror) 'live)) 60))
             (viewer (spyglass-bench-viewer-result)))
        (spyglass-bench-viewer-cleanup)
        (spyglass-bench--command "cleanup")
        (list :prepared prepared :owner owner :viewer viewer)))))

(defun spyglass-bench--driver-owns (scenario args options)
  "Play SCENARIO with ARGS and OPTIONS, the driver owning; return measurements."
  (let* ((prepared (spyglass-bench-owner-prepare scenario args)))
    (apply #'spyglass-bench--command "open"
           (and (plist-get options :terminal)
                (list :args (list :kind (plist-get options :terminal)))))
    (when (plist-get options :drops) (spyglass-bench--drops (plist-get options :drops)))
    (let ((owner nil))
      (spyglass-bench-owner-run (lambda (result) (setq owner result)))
      (spyglass-bench-wait (lambda () (or owner spyglass-bench--abort)) 580)
      (spyglass-bench--cancel-drops)
      (unless owner (error "The scenario did not finish"))
      (spyglass-bench-wait (lambda () (spyglass-connection-live-p spyglass-bench--connection)) 60)
      (let ((viewer (spyglass-bench--command "result")))
        (spyglass-bench--command "cleanup")
        (spyglass-bench-owner-cleanup)
        (list :prepared prepared :owner owner :viewer viewer)))))

;;;; Resizing

(defconst spyglass-bench--sizes '((40 . 10) (79 . 20) (25 . 6) (60 . 15))
  "Sizes, in cells, the viewer's window takes in the resize scenario.
They fit the frame of a batch Emacs.")

(defun spyglass-bench--resize-result (latencies matched)
  "Return the measurements of the resize scenario: LATENCIES and MATCHED.
LATENCIES are seconds from each report to the size known at both
ends; MATCHED is non-nil when every size was reached."
  (let ((sorted (sort (copy-sequence latencies) #'<)))
    (list :resizes (length latencies) :matched matched
          :total (apply #'+ latencies)
          :p50 (nth (/ (length sorted) 2) sorted) :max (car (last sorted)))))

(defun spyglass-bench--agent-owns-resize (args)
  "Play the resize scenario with ARGS, the agent owning and the driver viewing."
  (let ((prepared (spyglass-bench--command "prepare" :scenario "resize"
                                           :args (spyglass-bench--json-args args)))
        (latencies nil) (matched t))
    (let* ((mirror (spyglass-bench-viewer-open spyglass-bench--connection))
           (state (buffer-local-value 'spyglass--mirror mirror)))
      (spyglass-bench-wait (lambda () (and (eq (spyglass-mirror-state mirror) 'live)
                                           (spyglass--mirror-sizable state)))
                           60)
      (dolist (size spyglass-bench--sizes)
        (let* ((start (float-time))
               (cells (spyglass-bench-show-mirror mirror (car size) (cdr size))))
          ;; The size went to the owner, which tells its viewers.
          (spyglass-bench-wait (lambda () (equal (spyglass--mirror-grid state) cells)) 30)
          (unless (equal (spyglass--mirror-grid state) cells) (setq matched nil))
          (push (- (float-time) start) latencies)))
      (delete-other-windows)
      (let* ((owner (spyglass-bench--command "run"))
             (viewer (spyglass-bench-viewer-result)))
        (spyglass-bench-viewer-cleanup)
        (spyglass-bench--command "cleanup")
        (list :prepared prepared
              :owner (plist-put owner :resize (spyglass-bench--resize-result latencies matched))
              :viewer viewer)))))

(defun spyglass-bench--driver-owns-resize (args)
  "Play the resize scenario with ARGS, the driver owning and the agent viewing."
  (let* ((prepared (spyglass-bench-owner-prepare "resize" args))
         (share (plist-get spyglass-bench--owner :share))
         (latencies nil) (matched t))
    (spyglass-bench--command "open")
    (dolist (size spyglass-bench--sizes)
      (let* ((start (float-time))
             (shown (spyglass-bench--command "show" :args (list :cols (car size)
                                                                :rows (cdr size))))
             (cells (cons (plist-get shown :cols) (plist-get shown :rows))))
        (spyglass-bench-wait (lambda () (equal (spyglass-share-grid share) cells)) 30)
        (unless (equal (spyglass-share-grid share) cells) (setq matched nil))
        (push (- (float-time) start) latencies)))
    (let ((owner nil))
      (spyglass-bench-owner-run (lambda (result) (setq owner result)))
      (spyglass-bench-wait (lambda () owner) 120)
      (let ((viewer (spyglass-bench--command "result")))
        (spyglass-bench--command "cleanup")
        (spyglass-bench-owner-cleanup)
        (list :prepared prepared
              :owner (plist-put owner :resize (spyglass-bench--resize-result latencies matched))
              :viewer viewer)))))

;;;; The table

(defun spyglass-bench--ms (seconds)
  "Return SECONDS as milliseconds, for the table."
  (if (numberp seconds) (format "%.1f" (* 1000 seconds)) "-"))

(defun spyglass-bench--pair (summary)
  "Return the median and 99th percentile of SUMMARY in milliseconds."
  (if (and summary (plist-get summary :p50))
      (format "%s/%s" (spyglass-bench--ms (plist-get summary :p50))
              (spyglass-bench--ms (plist-get summary :p99)))
    "-"))

(defun spyglass-bench--same-p (owner viewer)
  "Return non-nil when the digests of OWNER and VIEWER agree."
  (let ((a (plist-get owner :digest)) (b (plist-get viewer :digest)))
    (and a b
         (equal (plist-get a :size) (plist-get b :size))
         (equal (plist-get a :text) (plist-get b :text))
         (equal (plist-get a :props) (plist-get b :props))
         (equal (plist-get a :overlays) (plist-get b :overlays)))))

(defun spyglass-bench--row (name direction result)
  "Return the table row of scenario NAME in DIRECTION with RESULT."
  (let* ((owner (plist-get result :owner))
         (viewer (plist-get result :viewer))
         (ostats (plist-get owner :stats))
         (vstats (plist-get viewer :stats))
         (tick (plist-get ostats :tick))
         (baseline (plist-get owner :baseline))
         (resize (plist-get owner :resize))
         (size (plist-get (plist-get owner :digest) :size)))
    (list name direction
          (format "%.1f" (/ (or size 0) 1e6))
          (cond
           (resize (format "%.2f" (plist-get resize :total)))
           ((equal (plist-get owner :scenario) "large")
            (format "%.2f" (or (plist-get owner :synced) 0)))
           (t (format "%.2f+%.2f" (or (plist-get owner :produce) 0) (or (plist-get owner :drain) 0))))
          (format "%.1f" (/ (or (plist-get ostats :bytes) 0) 1e6))
          (if resize
              (format "%s/%s" (spyglass-bench--ms (plist-get resize :p50))
                      (spyglass-bench--ms (plist-get resize :max)))
            (spyglass-bench--pair (plist-get vstats :latency)))
          (spyglass-bench--pair (plist-get ostats :latency))
          (if (and tick (plist-get tick :p50))
              (format "%s/%s/%s" (spyglass-bench--ms (plist-get tick :p50))
                      (spyglass-bench--ms (plist-get tick :p99))
                      (spyglass-bench--ms (plist-get tick :max)))
            "-")
          (format "%.2f%s" (or (plist-get owner :cpu) 0)
                  (if (and baseline (plist-get baseline :cpu))
                      (format " (%.2f)" (plist-get baseline :cpu))
                    ""))
          (format "%d/%d %.2f/%.2f" (or (plist-get owner :gcs) 0) (or (plist-get viewer :gcs) 0)
                  (or (plist-get owner :gc-time) 0) (or (plist-get viewer :gc-time) 0))
          (format "%d/%d" (/ (or (plist-get owner :rss-peak) 0) 1024)
                  (/ (or (plist-get viewer :rss-peak) 0) 1024))
          (format "%s%s%s%s"
                  (if (and (spyglass-bench--same-p owner viewer)
                           (or (null resize) (plist-get resize :matched)))
                      "yes" "NO")
                  (if resize (format " s%d" (plist-get resize :resizes)) "")
                  (let ((r (plist-get ostats :resumes))) (if r (format " r%d" r) ""))
                  (let ((f (plist-get ostats :full-scans))) (if f (format " f%d" f) ""))))))

(defconst spyglass-bench--header
  '("scenario" "dir" "MB" "time s" "wire MB" "lat ms p50/p99" "ack ms p50/p99"
    "tick ms p50/p99/max" "owner CPU s (alone)" "GC n o/v, s o/v" "RSS MB o/v" "exact")
  "Columns of the table.")

(defun spyglass-bench--print-table (rows)
  "Print ROWS as an Org table."
  (let* ((rows (cons spyglass-bench--header rows))
         (widths (apply #'cl-mapcar (lambda (&rest cells) (apply #'max (mapcar #'length cells)))
                        rows)))
    (cl-flet ((line (cells)
                (concat "| " (mapconcat #'identity
                                        (cl-mapcar (lambda (c w) (string-pad c w)) cells widths)
                                        " | ")
                        " |")))
      (princ (concat (line (car rows)) "\n"))
      (princ (concat "|" (mapconcat (lambda (w) (make-string (+ 2 w) ?-)) widths "+") "|\n"))
      (dolist (row (cdr rows)) (princ (concat (line row) "\n"))))))

;;;; Main

(defun spyglass-bench-main ()
  "Run the benchmarks and print their table."
  (setq spyglass-bench--remote (spyglass-bench--getenv "SPYGLASS_BENCH_REMOTE"))
  (spyglass-bench-read-environment)
  (spyglass-bench-start-guard)
  (add-hook 'spyglass-error-functions
            (lambda (_c msg) (spyglass-bench--log "error: %s" msg)))
  (let ((rows nil) (failed nil)
        (process-environment (if spyglass-bench--remote
                                 (append '("SPYGLASS_EAT_EL=" "SPYGLASS_VTERM_EL="
                                           "SPYGLASS_GHOSTEL_EL=")
                                         process-environment)
                               process-environment)))
    (unwind-protect
        (progn
          (spyglass-bench--start-agent)
          (dolist (s (spyglass-bench--scenarios))
            (pcase-let ((`(,name ,scenario ,args ,options) s))
              (dolist (direction '(agent driver))
                (if (and (plist-get options :terminal)
                         (or (not (spyglass-bench-terminal-available-p
                                   (plist-get options :terminal)))
                             spyglass-bench--remote))
                    (spyglass-bench--log "%s %s: skipped, no %s on both ends"
                                         name direction (plist-get options :terminal))
                  (spyglass-bench--log "%s, %s owning..." name direction)
                  (when (spyglass-bench--getenv "SPYGLASS_BENCH_PROFILE")
                    (require 'profiler)
                    (profiler-start 'cpu))
                  (condition-case err
                      (let* ((result (cond
                                      ((and (plist-get options :resize) (eq direction 'agent))
                                       (spyglass-bench--agent-owns-resize args))
                                      ((plist-get options :resize)
                                       (spyglass-bench--driver-owns-resize args))
                                      ((eq direction 'agent)
                                       (spyglass-bench--agent-owns scenario args options))
                                      (t (spyglass-bench--driver-owns scenario args options))))
                             (row (spyglass-bench--row
                                   name (if (eq direction 'agent) "agent→driver" "driver→agent")
                                   result)))
                        (spyglass-bench--log "%S" row)
                        (when (spyglass-bench--getenv "SPYGLASS_BENCH_PROFILE")
                          (profiler-stop)
                          (profiler-report)
                          (with-current-buffer (car (cl-remove-if-not
                                                     (lambda (b) (string-match-p "Profiler-Report"
                                                                                 (buffer-name b)))
                                                     (buffer-list)))
                            (profiler-report-render-reversed-calltree)
                            (goto-char (point-min))
                            (while (not (eobp))
                              (ignore-errors (profiler-report-expand-entry t))
                              (forward-line 1))
                            (spyglass-bench--log "profile:\n%s"
                                                 (buffer-substring-no-properties
                                                  1 (min (point-max) 6000))))
                          (profiler-reset))
                        (let ((o (plist-get result :owner)))
                          (spyglass-bench--log "owner: %S MB of strings, gc-cons-percentage %S"
                                               (plist-get o :string-mb) (plist-get o :gc-pct))
                          (spyglass-bench--log "viewer: %S" (plist-get (plist-get result :viewer) :stats))
                          (spyglass-bench--log "owner stats: %S" (plist-get o :stats)))
                        (unless (spyglass-bench--same-p (plist-get result :owner)
                                                        (plist-get result :viewer))
                          (push name failed)
                          (let ((a (append (plist-get (plist-get (plist-get result :owner) :digest)
                                                      :overlay-list) nil))
                                (b (append (plist-get (plist-get (plist-get result :viewer) :digest)
                                                      :overlay-list) nil)))
                            (when (or a b)
                              (spyglass-bench--log "overlays only on the owner: %S"
                                                   (seq-take (cl-set-difference a b :test #'equal) 6))
                              (spyglass-bench--log "overlays only on the viewer: %S"
                                                   (seq-take (cl-set-difference b a :test #'equal) 6))))
                          (spyglass-bench--log "digests differ: %S / %S"
                                               (plist-get (plist-get result :owner) :digest)
                                               (plist-get (plist-get result :viewer) :digest)))
                        (push row rows))
                    (error (push name failed)
                           (spyglass-bench--log "%s failed: %s" name (error-message-string err))
                           (ignore-errors (spyglass-bench-viewer-cleanup))
                           (ignore-errors (spyglass-bench-owner-cleanup))
                           (ignore-errors (spyglass-bench--command "cleanup")))))))))
      (spyglass-bench--stop-agent))
    (princ (format "\nspyglass benchmarks, %s, driver %s (Emacs %s), agent %s (Emacs %s)\n\n"
                   (format-time-string "%F %R")
                   (system-name) emacs-version
                   (or spyglass-bench--remote "localhost")
                   (plist-get spyglass-bench--ready :emacs)))
    (spyglass-bench--print-table (nreverse rows))
    (when spyglass-bench--warnings
      (princ (format "\nAgent warnings: %S\n" (delete-dups spyglass-bench--warnings))))
    (when failed
      (princ (format "\nFAILED: %s\n" (mapconcat #'identity (delete-dups failed) ", "))))
    (kill-emacs (if failed 1 0))))

(provide 'spyglass-bench)
;;; spyglass-bench.el ends here
