;;; spyglass-gui-test.el --- Tests of parking frames, in a graphical Emacs  -*- lexical-binding: t; -*-

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

;; Parking frames need a graphical display, which batch Emacs lacks:
;; `make test-gui' runs these tests in a graphical Emacs started
;; iconified, which writes the results to SPYGLASS_GUI_LOG and exits.
;; This Emacs is owner and viewer, except for the test of a Ghostel
;; terminal mirrored by another Emacs, the benchmark's agent, a batch
;; Emacs.  It kills itself past 2 GB of resident memory.

;;; Code:

(require 'spyglass-test-helpers)
(require 'spyglass-transport-test)
(require 'spyglass-bench)
(require 'desktop)
(require 'frameset)
(require 'benchmark)

(defvar ghostel--term-cols)
(defvar ghostel--term-rows)
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))

(defun spyglass-gui-test--parking-frames ()
  "Return the live parking frames."
  (cl-remove-if-not #'spyglass-parking-frame-p (frame-list)))

(defun spyglass-gui-test--parked-p (buffer)
  "Return BUFFER's parking frame, or nil."
  (spyglass-share-parked (spyglass-share-of buffer)))

(defun spyglass-gui-test--frame-is-hidden (frame)
  "Check that FRAME, a parking frame, stays out of the user's way."
  (should (null (frame-visible-p frame)))
  (should-not (eq frame (selected-frame)))
  (should (null (frame-focus-state frame)))
  (should-not (memq frame (visible-frame-list)))
  ;; Never reached by `other-frame' or `next-frame'.
  (let ((seen nil) (f (selected-frame)))
    (dotimes (_ (length (frame-list)))
      (setq f (next-frame f))
      (push f seen))
    (should-not (memq frame seen)))
  (should (frame-parameter frame 'no-other-frame))
  ;; Neither desktops nor framesets save it.
  (should-not (desktop--check-dont-save frame))
  ;; No minibuffer of its own; deleting the minibuffer's frame deletes it.
  (should-not (eq (window-frame (minibuffer-window frame)) frame))
  (should (eq (frame-parameter frame 'delete-before)
              (window-frame (minibuffer-window frame))))
  (should (= (frame-parameter frame 'tab-bar-lines) 0))
  (let ((window (frame-root-window frame)))
    (should (window-dedicated-p window))
    (should (window-parameter window 'no-other-window))))

(ert-deftest spyglass-gui-parks-and-releases ()
  "A buffer needing a window is parked while viewers watch it and no window shows it."
  (spyglass-test-with-connection connection
    (let ((buffer (generate-new-buffer "parked"))
          (other (get-buffer-create "*scratch*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (insert "text\n"))
            (set-window-buffer (selected-window) other)
            (spyglass-share buffer :needs-window t)
            ;; Shared without viewers: not parked.
            (spyglass-test-settle 0.2)
            (should-not (spyglass-gui-test--parked-p buffer))
            (let ((mirror (spyglass-test-mirror connection buffer)))
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              (let ((frame (spyglass-gui-test--parked-p buffer)))
                (should (frame-live-p frame))
                (should (eq (window-buffer (frame-root-window frame)) buffer))
                (spyglass-gui-test--frame-is-hidden frame)
                ;; Shown in a window of the user's: released, frame deleted.
                (set-window-buffer (selected-window) buffer)
                (spyglass-test-wait (lambda () (not (frame-live-p frame))))
                (should-not (frame-live-p frame))
                (should-not (spyglass-gui-test--parked-p buffer)))
              ;; Hidden again: parked again.
              (set-window-buffer (selected-window) other)
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              (should (spyglass-gui-test--parked-p buffer))
              ;; The viewer leaves: released.
              (spyglass-close mirror)
              (spyglass-test-wait (lambda () (null (spyglass-gui-test--parking-frames))))
              (should (null (spyglass-gui-test--parking-frames)))
              (kill-buffer mirror))
            ;; Unshared: released.
            (let ((mirror (spyglass-test-mirror connection buffer)))
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              (spyglass-unshare buffer)
              (should (null (spyglass-gui-test--parking-frames)))
              (kill-buffer mirror))
            ;; Killed: released.
            (spyglass-share buffer :needs-window t)
            (let ((mirror (spyglass-test-mirror connection buffer)))
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              (kill-buffer buffer)
              (should (null (spyglass-gui-test--parking-frames)))
              (kill-buffer mirror)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest spyglass-gui-frame-shown-is-adopted ()
  "A parking frame something makes visible becomes an ordinary frame."
  (spyglass-test-with-connection connection
    (let ((buffer (generate-new-buffer "adopted")))
      (unwind-protect
          (let ((mirror (progn (spyglass-share buffer :needs-window t)
                               (spyglass-test-mirror connection buffer))))
            (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
            (let ((frame (spyglass-gui-test--parked-p buffer)))
              ;; As `display-buffer' told to reuse windows of all frames would.
              (make-frame-visible frame)
              (spyglass-test-wait (lambda () (not (spyglass-parking-frame-p frame))))
              (should (frame-live-p frame))
              (should-not (spyglass-parking-frame-p frame))
              (should-not (spyglass-gui-test--parked-p buffer))
              (should-not (window-dedicated-p (frame-root-window frame)))
              (delete-frame frame t))
            (kill-buffer mirror))
        (kill-buffer buffer)))))

(ert-deftest spyglass-gui-minibuffer-frame-can-be-deleted ()
  "Deleting the frame whose minibuffer a parking frame uses deletes it first."
  (spyglass-test-with-connection connection
    (let* ((buffer (generate-new-buffer "surrogate"))
           (first (selected-frame))
           (second (make-frame '((visibility . icon)))))
      (unwind-protect
          (let ((mirror (progn (spyglass-share buffer :needs-window t)
                               (spyglass-test-mirror connection buffer))))
            (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
            (let* ((frame (spyglass-gui-test--parked-p buffer))
                   (minibuffer-frame (window-frame (minibuffer-window frame))))
              (should (memq minibuffer-frame (list first second)))
              (when (eq minibuffer-frame first) (select-frame second))
              (delete-frame minibuffer-frame)
              (should-not (frame-live-p frame))
              ;; Parked again, with the frame that remains.
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              (should (spyglass-gui-test--parked-p buffer)))
            (kill-buffer mirror))
        (kill-buffer buffer)
        (when (and (frame-live-p second) (frame-live-p first))
          (delete-frame second t))))))

(defmacro spyglass-gui-test--with-visible-frame (&rest body)
  "Run BODY with the selected frame visible and its echo area empty.
Then iconify the frame again.  A mirror reports the size of windows on
visible frames only."
  (declare (indent 0) (debug t))
  (let ((frame (make-symbol "frame")))
    `(let ((,frame (selected-frame)))
       (unwind-protect
           (progn
             (make-frame-visible ,frame)
             (spyglass-test-wait (lambda () (eq (frame-visible-p ,frame) t)) 5)
             ;; A long message grows the echo area, and shrinks the
             ;; windows above once it goes: let it go now.
             (message nil)
             (redisplay t)
             ,@body)
         (when (frame-live-p ,frame) (iconify-frame ,frame))))))

(defun spyglass-gui-test--tty-size (process)
  "Return the size PROCESS's pty has, (COLUMNS . ROWS), as `stty' reads it."
  (with-temp-buffer
    (call-process "sh" nil t nil "-c"
                  (format "stty size < %s" (shell-quote-argument (process-tty-name process))))
    (let ((words (split-string (buffer-string))))
      (and (= (length words) 2)
           (cons (string-to-number (cadr words)) (string-to-number (car words)))))))

(ert-deftest spyglass-gui-parked-window-is-sized ()
  "A parked terminal's window takes the viewer's size; the owner's windows keep theirs."
  (spyglass-test-with-connection connection
    (let ((buffer (generate-new-buffer "parked-pty"))
          (config (current-window-configuration)))
      (unwind-protect
          (spyglass-gui-test--with-visible-frame
            (with-current-buffer buffer
              (make-process :name "parked-pty" :buffer buffer :command '("cat")
                            :connection-type 'pty :noquery t)
              (setq-local spyglass-refit-function #'spyglass-refit-process))
            (spyglass-share buffer :needs-window t)
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (viewer-window (split-window nil 12 t))
                   (process (get-buffer-process buffer)))
              (set-window-buffer viewer-window mirror)
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              ;; Each layout of the viewer's windows reaches the owner
              ;; through the window change functions, as redisplay runs them.
              (dolist (layout (list #'ignore
                                    (lambda () (split-window viewer-window))
                                    (lambda () (delete-other-windows viewer-window))))
                (funcall layout)
                (redisplay t)
                (let ((want (spyglass-window-cells viewer-window))
                      (edges (cons (frame-pixel-width) (mapcar #'window-pixel-edges (window-list)))))
                  (spyglass-test-wait (lambda () (equal (spyglass-gui-test--tty-size process) want)))
                  (should (equal (spyglass-gui-test--tty-size process) want))
                  (should (equal (spyglass-window-cells
                                  (frame-root-window (spyglass-gui-test--parked-p buffer)))
                                 want))
                  ;; No window of the user's moved.
                  (should (equal (cons (frame-pixel-width) (mapcar #'window-pixel-edges (window-list)))
                                 edges))))
              (kill-buffer mirror)))
        (set-window-configuration config)
        (delete-other-windows)
        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))

(ert-deftest spyglass-gui-owner-policy-of-a-share ()
  "A share's own `owner' policy: the viewer's window sizes it while parked, the owner's once shown there."
  (spyglass-test-with-connection connection
    (let ((buffer (generate-new-buffer "owner-pty"))
          (config (current-window-configuration))
          ;; Whatever the option says, the share's policy decides.
          (spyglass-size-policy 'latest))
      (unwind-protect
          (spyglass-gui-test--with-visible-frame
            (with-current-buffer buffer
              (make-process :name "owner-pty" :buffer buffer :command '("cat")
                            :connection-type 'pty :noquery t)
              (setq-local spyglass-refit-function #'spyglass-refit-process))
            (spyglass-share buffer :needs-window t :size-policy 'owner)
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (viewer-window (split-window nil 12 t))
                   (process (get-buffer-process buffer)))
              (set-window-buffer viewer-window mirror)
              (redisplay t)
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              ;; Parked: the viewer's window.
              (let ((want (spyglass-window-cells viewer-window)))
                (spyglass-test-wait (lambda () (equal (spyglass-gui-test--tty-size process) want)))
                (should (equal (spyglass-gui-test--tty-size process) want)))
              ;; Shown in a window of the owner's: that window, though
              ;; the viewer typed last.
              (spyglass-mirror-send-string "x" mirror)
              (let ((owner-window (split-window (frame-root-window) nil 'below)))
                (set-window-buffer owner-window buffer)
                (redisplay t)
                (spyglass-test-wait (lambda () (null (spyglass-gui-test--parked-p buffer))))
                (with-current-buffer buffer (spyglass--refit (spyglass-share-of buffer)))
                (let ((want (spyglass-window-cells owner-window)))
                  (spyglass-test-wait (lambda () (equal (spyglass-gui-test--tty-size process) want)))
                  (should (equal (spyglass-gui-test--tty-size process) want)))
                ;; Hidden there again: the viewer's window once more.
                (delete-window owner-window)
                (redisplay t)
                (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
                (let ((want (spyglass-window-cells viewer-window)))
                  (spyglass-test-wait (lambda () (equal (spyglass-gui-test--tty-size process) want)))
                  (should (equal (spyglass-gui-test--tty-size process) want))))
              (kill-buffer mirror)))
        (set-window-configuration config)
        (delete-other-windows)
        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))

(defconst spyglass-gui-test--script
  (concat "sleep 1; i=0; while [ $i -lt 40 ]; do echo \"parked line $i\"; i=$((i+1)); "
          "sleep 0.05; done; echo done; while :; do stty size; sleep 0.2; done")
  "What the Ghostel terminal runs: lines printed while parked, then its size.")

(defun spyglass-gui-test--ghostel ()
  "Return a Ghostel terminal no window shows, running the test's script."
  (spyglass-bench-read-environment)
  (unless (spyglass-bench-terminal-available-p "ghostel")
    (ert-skip "No Ghostel here"))
  (spyglass-bench-load-terminal "ghostel")
  (let ((buffer (generate-new-buffer "bench")))
    (ghostel-exec buffer "/bin/sh" (list "-c" spyglass-gui-test--script))
    buffer))

(defun spyglass-gui-test--kill-terminal (buffer)
  "Kill BUFFER, a terminal, and its process."
  (when (buffer-live-p buffer)
    (let ((kill-buffer-query-functions nil))
      (when (get-buffer-process buffer) (delete-process (get-buffer-process buffer)))
      (kill-buffer buffer))))

(defun spyglass-gui-test--printed-size ()
  "Return the size the script last printed in this terminal, (COLUMNS . ROWS)."
  (save-excursion
    (goto-char (point-max))
    (and (re-search-backward "^\\([0-9]+\\) \\([0-9]+\\)$" nil t)
         (cons (string-to-number (match-string 2)) (string-to-number (match-string 1))))))

(ert-deftest spyglass-gui-ghostel-renders-parked ()
  "A Ghostel terminal no window shows renders while parked, and takes the viewer's size."
  (spyglass-test-with-connection connection
    (let ((buffer (spyglass-gui-test--ghostel))
          (config (current-window-configuration)))
      (unwind-protect
          (spyglass-gui-test--with-visible-frame
            (should-not (get-buffer-window buffer t))
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (viewer-window (split-window nil 20 t)))
              (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
              (should (spyglass-gui-test--parked-p buffer))
              (with-current-buffer buffer
                (spyglass-test-wait (lambda () (save-excursion
                                                 (goto-char 1)
                                                 (search-forward "done" nil t)))
                                    20)
                (should (save-excursion (goto-char 1) (search-forward "parked line 39" nil t))))
              (spyglass-test-converge buffer mirror)
              ;; The viewer's window sizes it, through its parking window.
              (set-window-buffer viewer-window mirror)
              (setf (spyglass--mirror-sent-size (buffer-local-value 'spyglass--mirror mirror)) nil)
              (spyglass--mirror-report-sizes)
              (let ((want (spyglass-window-cells viewer-window)))
                (with-current-buffer buffer
                  (spyglass-test-wait (lambda () (equal (spyglass-gui-test--printed-size) want)) 10)
                  (should (equal (cons ghostel--term-cols ghostel--term-rows) want))
                  (should (equal (spyglass-gui-test--printed-size) want)))
                (should (equal (spyglass-window-cells
                                (frame-root-window (spyglass-gui-test--parked-p buffer)))
                               want)))
              (kill-buffer mirror)))
        (set-window-configuration config)
        (delete-other-windows)
        (spyglass-gui-test--kill-terminal buffer)
        (should (null (spyglass-gui-test--parking-frames)))))))

(ert-deftest spyglass-gui-ghostel-mirrored-by-another-emacs ()
  "A parked Ghostel terminal's output reaches a mirror in another Emacs."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (spyglass-bench--start-agent)
          (setq buffer (spyglass-gui-test--ghostel))
          (spyglass-share buffer :id "bench")
          (spyglass-bench--command "open" :args (list :kind "ghostel"))
          (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)) 20)
          (should (spyglass-gui-test--parked-p buffer))
          (with-current-buffer buffer
            (spyglass-test-wait (lambda () (save-excursion
                                             (goto-char 1)
                                             (search-forward "done" nil t)))
                                20)
            (should (save-excursion (goto-char 1) (search-forward "parked line 39" nil t))))
          ;; The agent's mirror holds what the owner's buffer holds.
          (let ((owner nil) (viewer nil))
            (spyglass-test-wait
             (lambda ()
               (setq owner (spyglass-bench-digest buffer t)
                     viewer (plist-get (spyglass-bench--command "result") :digest))
               (equal (plist-get owner :text) (plist-get viewer :text)))
             20)
            (should (equal (plist-get owner :text) (plist-get viewer :text)))
            (should (equal (plist-get owner :props) (plist-get viewer :props)))))
      (ignore-errors (spyglass-bench--stop-agent))
      (spyglass-gui-test--kill-terminal buffer)
      (spyglass-test-stop))))

(defun spyglass-gui-test--redisplay-cost (buffers)
  "Return milliseconds a redisplay takes while BUFFERS change, on average."
  (garbage-collect)
  (let ((start (float-time)) (n 500))
    (dotimes (i n)
      (dolist (b buffers)
        (with-current-buffer b
          (goto-char (point-max))
          (insert (format "line %d\n" i))))
      (redisplay t))
    (/ (* 1000 (- (float-time) start)) n)))

(ert-deftest spyglass-gui-parked-frames-cost-nothing-to-redisplay ()
  "Redisplay does not draw parking frames: its cost does not grow with them."
  (spyglass-test-with-connection connection
    (let* ((buffers (cl-loop for i below 10 collect (generate-new-buffer (format "cost-%d" i))))
           (idle (list (generate-new-buffer "cost-idle")))
           (mirrors nil))
      (unwind-protect
          (spyglass-gui-test--with-visible-frame
            (dolist (b buffers)
              (spyglass-share b :needs-window nil)
              (push (spyglass-open connection (buffer-name b)) mirrors))
            (spyglass-test-wait (lambda () (cl-every (lambda (m) (eq (spyglass-mirror-state m) 'live))
                                                     mirrors)))
            ;; Shared and watched alike, hidden or parked.
            (let* ((hidden (spyglass-gui-test--redisplay-cost buffers))
                   (start (float-time))
                   (made (progn
                           (dolist (b buffers) (spyglass-share b :needs-window t))
                           (spyglass-test-wait
                            (lambda () (= (length (spyglass-gui-test--parking-frames)) 10)))
                           (- (float-time) start)))
                   (parked (spyglass-gui-test--redisplay-cost buffers))
                   (count (length (spyglass-gui-test--parking-frames)))
                   ;; What polling adds for a parked share, every 50 ms.
                   (check (let ((share (spyglass-share-of (car buffers))))
                            (/ (* 1e6 (car (benchmark-run 1000 (spyglass--check-parking share))))
                               1000)))
                   (parked-idle (spyglass-gui-test--redisplay-cost idle))
                   (shown (save-window-excursion
                            (delete-other-windows)
                            (let ((window (selected-window)) (window-min-height 1))
                              (dolist (b buffers)
                                (set-window-buffer window b)
                                (when (cdr (memq b buffers))
                                  (setq window (split-window window 3)))))
                            (redisplay t)
                            (spyglass-gui-test--redisplay-cost buffers))))
              (should (= count 10))
              (message "GUI: redisplay with 10 shared buffers changing: %.3f ms hidden, %.3f ms parked, %.3f ms shown in windows; parked and unchanged: %.3f ms; 10 parked in %.0f ms; checking a parked share %.1f us; RSS %d MB"
                       hidden parked shown parked-idle (* 1000 made) check
                       (/ (spyglass-bench-rss) 1024))
              ;; Within the noise of a millisecond.
              (should (< parked (+ hidden 1.0)))))
        (mapc #'kill-buffer mirrors)
        (mapc #'kill-buffer buffers)
        (mapc #'kill-buffer idle)))))

(ert-deftest spyglass-gui-largest-parked-window ()
  "A parked window costs memory with its size, bounded by `spyglass-max-terminal-size'."
  (spyglass-test-with-connection connection
    (let ((buffer (generate-new-buffer "largest")))
      (unwind-protect
          (let* ((mirror (progn (spyglass-share buffer :needs-window t)
                                (with-current-buffer buffer
                                  (setq-local spyglass-refit-function #'ignore))
                                (spyglass-test-mirror connection buffer)))
                 (sub (car (spyglass-share-subs (spyglass-share-of buffer))))
                 (costs nil))
            (with-current-buffer buffer
              (dotimes (i 600) (insert (make-string 999 (+ ?a (% i 26))) "\n")))
            (spyglass-test-wait (lambda () (spyglass-gui-test--parked-p buffer)))
            (garbage-collect)
            (let ((before (spyglass-bench-rss)))
              (dolist (size (list '(80 . 24) '(200 . 60) spyglass-max-terminal-size '(100000 . 100000)))
                (spyglass--send connection (list :t "sz" :sub (spyglass--sub-id sub)
                                                 :c (car size) :r (cdr size) :f :false))
                (let ((want (cons (min (car size) (car spyglass-max-terminal-size))
                                  (min (cdr size) (cdr spyglass-max-terminal-size)))))
                  (spyglass-test-wait
                   (lambda () (equal (spyglass-window-cells
                                      (frame-root-window (spyglass-gui-test--parked-p buffer)))
                                     want)))
                  (should (equal (spyglass-window-cells
                                  (frame-root-window (spyglass-gui-test--parked-p buffer)))
                                 want))
                  (spyglass-test-settle 0.3)
                  (push (format "%dx%d: +%d MB" (car want) (cdr want)
                                (/ (- (spyglass-bench-rss) before) 1024))
                        costs)))
              (message "GUI: resident memory of a parked window: %s"
                       (mapconcat #'identity (nreverse costs) ", "))
              (should (< (- (spyglass-bench-rss) before) (* 100 1024))))
            (kill-buffer mirror))
        (kill-buffer buffer)))))

(ert-deftest spyglass-gui-attempts-never-block ()
  "Attempts to connect, failing or not, never keep a graphical Emacs from running.
The same attempts as `spyglass-transport-test.el''s, redisplaying
between steps."
  (spyglass-transport-test-with-ssh (ssh)
    (let ((silent (spyglass-transport-test-silent-port))
          (lags nil))
      (unwind-protect
          (cl-flet ((attempt (name transport predicate)
                      (spyglass-transport-test-with ((spyglass-connect-timeout 1.0)
                                                     (spyglass-reconnect-delays '(0.2)))
                        (spyglass-test-start)
                        (pcase-let ((`(,connection . ,lag)
                                     (spyglass-transport-test-attempt
                                      (funcall transport) predicate)))
                          (should (funcall predicate connection))
                          (push (format "%s %.1f ms" name (* 1000 lag)) lags)
                          (should (< lag spyglass-transport-test-max-lag))))))
            (attempt "refused"
                     (lambda () (spyglass-tcp-transport "127.0.0.1" (spyglass-transport-test-closed-port) "t"))
                     (spyglass-transport-test-reported "Cannot connect"))
            (attempt "never answers"
                     (lambda () (spyglass-tcp-transport "127.0.0.1" (car silent) "t"))
                     (spyglass-transport-test-reported "timed out"))
            (attempt "unresolvable"
                     (lambda () (spyglass-tcp-transport "spyglass.invalid" 4000 "t"))
                     (spyglass-transport-test-reported "cannot resolve"))
            (attempt "connects"
                     (lambda () (spyglass-file-transport spyglass-test--server-file))
                     #'spyglass-connection-live-p)
            (attempt "ssh, no server"
                     (lambda () (spyglass-ssh-transport "peer" :program ssh
                                                        :file "/nonexistent/server.json"))
                     (spyglass-transport-test-reported "no spyglass server registered"))
            (attempt "ssh connects"
                     (lambda () (spyglass-ssh-transport "peer" :program ssh
                                                        :file spyglass-test--server-file))
                     #'spyglass-connection-live-p)
            (message "GUI: longest wait during attempts: %s"
                     (mapconcat #'identity (nreverse lags) ", ")))
        (mapc #'delete-process (cdr silent))))))

(defun spyglass-gui-test-run ()
  "Run the GUI tests, write their results to SPYGLASS_GUI_LOG, and exit.
Each step of waiting redisplays, as the command loop would: window
change functions, Ghostel's among them, run then."
  (spyglass-bench-start-guard)
  (advice-add 'spyglass-test-step :before #'redisplay)
  (let* ((stats (ert-run-tests-batch (or (getenv "SPYGLASS_GUI_SELECTOR") "spyglass-gui-")))
         (failed (ert-stats-completed-unexpected stats)))
    (with-current-buffer (messages-buffer)
      (let ((coding-system-for-write 'utf-8))
        (write-region (point-min) (point-max) (getenv "SPYGLASS_GUI_LOG") nil 'silent)))
    (kill-emacs (if (zerop failed) 0 1))))

(provide 'spyglass-gui-test)
;;; spyglass-gui-test.el ends here
