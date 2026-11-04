;;; spyglass-window-test.el --- Tests of terminals sized by their viewers  -*- lexical-binding: t; -*-

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

;; Viewers report the size of the windows showing their mirrors, and
;; the owner gives the terminal the size its policy picks: checked on
;; a process in a pty, whose size `stty' reads, and on real Eat, vterm
;; and Ghostel terminals when they are available.  This batch Emacs is
;; both owner and viewer; its frame, split, gives windows of known
;; sizes.  Window change functions run only on redisplay, which a batch
;; Emacs never does: the tests report sizes as those functions would.
;; Parking needs a graphical display: see spyglass-gui-test.el.

;;; Code:

(require 'spyglass-test-helpers)

(defun spyglass-window-test--tty-size (process)
  "Return the size PROCESS's pty has, (COLUMNS . ROWS), as `stty' reads it."
  (let ((tty (process-tty-name process)))
    (with-temp-buffer
      (call-process "sh" nil t nil "-c" (format "stty size < %s" (shell-quote-argument tty)))
      (let ((words (split-string (buffer-string))))
        (and (= (length words) 2)
             (cons (string-to-number (cadr words)) (string-to-number (car words))))))))

(defun spyglass-window-test--ptys-p ()
  "Return non-nil when processes may run in ptys, whose size `stty' reads.
Emacs on Windows runs every process through pipes: there is no pty to
size, and `process-tty-name' is nil."
  (not (memq system-type '(windows-nt ms-dos))))

(defun spyglass-window-test--report (mirror)
  "Report MIRROR's window size now, as its window change functions would."
  (setf (spyglass--mirror-sent-size (buffer-local-value 'spyglass--mirror mirror)) nil)
  (spyglass--mirror-report-sizes))

(defun spyglass-window-test--sub (share)
  "Return SHARE's only subscription."
  (car (spyglass-share-subs share)))

(defmacro spyglass-window-test--with-windows (&rest body)
  "Run BODY, then restore the window configuration of the batch frame."
  (declare (indent 0) (debug t))
  (let ((config (make-symbol "config")))
    `(let ((,config (current-window-configuration)))
       (unwind-protect (progn ,@body)
         (set-window-configuration ,config)
         (delete-other-windows)))))

(defun spyglass-window-test--pty-buffer (name)
  "Return a buffer called NAME running `cat' in a pty, sizable by viewers."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (make-process :name name :buffer buffer :command '("cat")
                    :connection-type 'pty :noquery t)
      (setq-local spyglass-refit-function #'spyglass-refit-process))
    buffer))

(defun spyglass-window-test--kill (buffer)
  "Kill BUFFER and its process."
  (when (buffer-live-p buffer)
    (let ((kill-buffer-query-functions nil))
      (when (get-buffer-process buffer) (delete-process (get-buffer-process buffer)))
      (kill-buffer buffer))))

(ert-deftest spyglass-window-capabilities ()
  "Peers announce optional messages; a peer announcing none gets no size."
  (spyglass-test-with-connection connection
    (should (spyglass-connection-capable-p connection "sz"))
    (let ((buffer (spyglass-window-test--pty-buffer "caps")))
      (unwind-protect
          (spyglass-window-test--with-windows
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (share (spyglass-share-of buffer)))
              (spyglass-test-wait (lambda () (spyglass--mirror-sizable
                                              (buffer-local-value 'spyglass--mirror mirror))))
              (set-window-buffer (selected-window) mirror)
              ;; As a peer predating sizes would be.
              (setf (spyglass-connection-peer-caps connection) nil)
              (spyglass-window-test--report mirror)
              (spyglass-test-settle 0.2)
              (should (null (spyglass--sub-size (spyglass-window-test--sub share))))
              (setf (spyglass-connection-peer-caps connection) '("sz"))
              (spyglass-window-test--report mirror)
              (spyglass-test-wait (lambda () (spyglass--sub-size (spyglass-window-test--sub share))))
              (should (spyglass--sub-size (spyglass-window-test--sub share)))))
        (spyglass-window-test--kill buffer)))))

(ert-deftest spyglass-window-reports-size-pty ()
  "A viewer's window sizes a pty no owner's window shows, as it changes."
  (skip-unless (spyglass-window-test--ptys-p))
  (spyglass-test-with-connection connection
    (let ((buffer (spyglass-window-test--pty-buffer "pty")))
      (unwind-protect
          (spyglass-window-test--with-windows
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (share (spyglass-share-of buffer))
                   (process (get-buffer-process buffer)))
              (spyglass-test-wait (lambda () (spyglass--mirror-sizable
                                              (buffer-local-value 'spyglass--mirror mirror))))
              ;; On open: the mirror shown in the whole frame.
              (set-window-buffer (selected-window) mirror)
              (spyglass-window-test--report mirror)
              (let ((size (spyglass-window-cells (selected-window))))
                (spyglass-test-wait (lambda () (equal (spyglass-window-test--tty-size process) size)))
                (should (equal (spyglass--sub-size (spyglass-window-test--sub share)) size))
                (should (equal (spyglass-window-test--tty-size process) size)))
              ;; On change: a smaller window.
              (split-window nil 8)
              (split-window nil 30 t)
              (spyglass-window-test--report mirror)
              (let ((size (spyglass-window-cells (selected-window))))
                ;; Eight lines, its mode line one: the owner has no header line.
                (should (equal (cdr size) 7))
                (spyglass-test-wait (lambda () (equal (spyglass-window-test--tty-size process) size)))
                (should (equal (spyglass-window-test--tty-size process) size))
                ;; Viewers learn the terminal's size.
                (spyglass-test-wait (lambda () (equal (spyglass--mirror-grid
                                                       (buffer-local-value 'spyglass--mirror mirror))
                                                      size)))
                (should (equal (spyglass--mirror-grid (buffer-local-value 'spyglass--mirror mirror))
                               size))
                ;; Hidden: the terminal keeps its size.
                (delete-other-windows)
                (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
                (spyglass-window-test--report mirror)
                (spyglass-test-wait (lambda () (null (spyglass--sub-size
                                                      (spyglass-window-test--sub share)))))
                (should (null (spyglass--sub-size (spyglass-window-test--sub share))))
                (should (equal (spyglass-window-test--tty-size process) size)))))
        (spyglass-window-test--kill buffer)))))

(ert-deftest spyglass-window-owner-wins-when-active ()
  "The most recently active party sizes the terminal; owners' windows keep theirs."
  (skip-unless (spyglass-window-test--ptys-p))
  (spyglass-test-with-connection connection
    (let ((buffer (spyglass-window-test--pty-buffer "active")))
      (unwind-protect
          (spyglass-window-test--with-windows
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (share (spyglass-share-of buffer))
                   (process (get-buffer-process buffer))
                   (owner-window (selected-window))
                   (viewer-window (split-window nil 8)))
              (spyglass-test-wait (lambda () (spyglass--mirror-sizable
                                              (buffer-local-value 'spyglass--mirror mirror))))
              (set-window-buffer owner-window buffer)
              (set-window-buffer viewer-window mirror)
              (let ((owner-size (spyglass-window-cells owner-window))
                    (viewer-size (spyglass-window-cells viewer-window))
                    (edges (list (window-edges owner-window) (window-edges viewer-window))))
                (should-not (equal owner-size viewer-size))
                ;; Opening the mirror made the viewer the latest.
                (spyglass-window-test--report mirror)
                (spyglass-test-wait (lambda () (equal (spyglass-window-test--tty-size process)
                                                      viewer-size)))
                (should (equal (spyglass-window-test--tty-size process) viewer-size))
                ;; The owner uses its window: its size.
                (with-selected-window owner-window (spyglass--owner-command))
                (should (equal (spyglass-window-test--tty-size process) owner-size))
                ;; The viewer types: its size again.
                (spyglass-mirror-send-string "x" mirror)
                (spyglass-test-wait (lambda () (equal (spyglass-window-test--tty-size process)
                                                      viewer-size)))
                (should (equal (spyglass-window-test--tty-size process) viewer-size))
                ;; The terminal's own refit, on a change of the owner's
                ;; windows, keeps the latest party's size.
                (with-current-buffer buffer (spyglass-refit-process))
                (should (equal (spyglass-window-test--tty-size process) viewer-size))
                ;; No window of the owner was resized.
                (should (equal (list (window-edges owner-window) (window-edges viewer-window))
                               edges)))))
        (spyglass-window-test--kill buffer)))))

(ert-deftest spyglass-window-focus-makes-the-viewer-latest ()
  "Selecting a mirror's window gives its viewer the terminal's size."
  (skip-unless (spyglass-window-test--ptys-p))
  (spyglass-test-with-connection connection
    (let ((buffer (spyglass-window-test--pty-buffer "focus")))
      (unwind-protect
          (spyglass-window-test--with-windows
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (process (get-buffer-process buffer))
                   (owner-window (selected-window))
                   (viewer-window (split-window nil 8)))
              (spyglass-test-wait (lambda () (spyglass--mirror-sizable
                                              (buffer-local-value 'spyglass--mirror mirror))))
              (set-window-buffer owner-window buffer)
              (set-window-buffer viewer-window mirror)
              (spyglass-window-test--report mirror)
              (let ((owner-size (spyglass-window-cells owner-window))
                    (viewer-size (spyglass-window-cells viewer-window)))
                (spyglass-test-wait (lambda () (equal (spyglass-window-test--tty-size process)
                                                      viewer-size)))
                (with-selected-window owner-window (spyglass--owner-command))
                (should (equal (spyglass-window-test--tty-size process) owner-size))
                ;; What `window-selection-change-functions' do, the frame
                ;; having focus as a focus-in event says.
                (set-frame-parameter nil 'last-focus-update t)
                (select-window viewer-window)
                (spyglass--mirror-check-focus)
                (spyglass-test-wait (lambda () (equal (spyglass-window-test--tty-size process)
                                                      viewer-size)))
                (should (equal (spyglass-window-test--tty-size process) viewer-size))
                ;; Only a change of focus is news.
                (should (equal spyglass--mirror-focused mirror))
                (select-window owner-window)
                (spyglass--mirror-check-focus)
                (should (null spyglass--mirror-focused)))))
        (set-frame-parameter nil 'last-focus-update nil)
        (spyglass-window-test--kill buffer)))))

(ert-deftest spyglass-window-policies ()
  "Each policy picks its size among the owner's and the viewers'."
  (let* ((share (spyglass--make-share :id "p" :buffer (current-buffer)))
         (connection (spyglass--make-connection :state 'open))
         (a (spyglass--make-sub :connection-ref (spyglass--ref connection)
                                :share-ref (spyglass--ref share) :size '(100 . 30) :active 10.0))
         (b (spyglass--make-sub :connection-ref (spyglass--ref connection)
                                :share-ref (spyglass--ref share) :size '(80 . 40) :active 20.0)))
    (cl-letf (((symbol-function 'spyglass--sub-live-p) (lambda (_) t)))
      (setf (spyglass-share-subs share) (list a b)
            (spyglass-share-owner-active share) 15.0)
      (let ((spyglass-size-policy 'latest))
        (should (equal (spyglass--choose-size share '(120 . 50)) '(80 . 40)))
        (setf (spyglass-share-owner-active share) 25.0)
        (should (equal (spyglass--choose-size share '(120 . 50)) '(120 . 50)))
        ;; The owner showing it nowhere does not count.
        (should (equal (spyglass--choose-size share nil) '(80 . 40)))
        ;; Ties go to the owner.
        (setf (spyglass-share-owner-active share) 20.0)
        (should (equal (spyglass--choose-size share '(120 . 50)) '(120 . 50))))
      (let ((spyglass-size-policy 'smallest))
        (should (equal (spyglass--choose-size share '(120 . 35)) '(80 . 30)))
        (should (equal (spyglass--choose-size share nil) '(80 . 30))))
      (let ((spyglass-size-policy 'owner))
        (should (equal (spyglass--choose-size share '(120 . 50)) '(120 . 50)))
        (should (equal (spyglass--choose-size share nil) '(80 . 40))))
      ;; Without a viewer's window, the terminal sizes itself.
      (setf (spyglass--sub-size a) nil (spyglass--sub-size b) nil)
      (should (null (spyglass--choose-size share '(120 . 50)))))))

(ert-deftest spyglass-window-smallest-policy ()
  "Under `smallest', the smallest of the owner's and viewer's windows wins."
  (skip-unless (spyglass-window-test--ptys-p))
  (let ((spyglass-size-policy 'smallest))
    (spyglass-test-with-connection connection
      (let ((buffer (spyglass-window-test--pty-buffer "smallest")))
        (unwind-protect
            (spyglass-window-test--with-windows
              (let* ((mirror (spyglass-test-mirror connection buffer))
                     (process (get-buffer-process buffer))
                     (owner-window (selected-window))
                     (viewer-window (split-window nil 8 t)))
                (spyglass-test-wait (lambda () (spyglass--mirror-sizable
                                                (buffer-local-value 'spyglass--mirror mirror))))
                (set-window-buffer owner-window buffer)
                (set-window-buffer viewer-window mirror)
                (spyglass-window-test--report mirror)
                (let ((want (cons (min (car (spyglass-window-cells owner-window))
                                       (car (spyglass-window-cells viewer-window)))
                                  (min (cdr (spyglass-window-cells owner-window))
                                       (cdr (spyglass-window-cells viewer-window))))))
                  (spyglass-test-wait (lambda () (equal (spyglass-window-test--tty-size process)
                                                        want)))
                  (should (equal (spyglass-window-test--tty-size process) want)))))
          (spyglass-window-test--kill buffer))))))

(ert-deftest spyglass-window-reports-are-bounded ()
  "A viewer cannot ask for a terminal larger than `spyglass-max-terminal-size'."
  (spyglass-test-with-connection connection
    (let ((buffer (spyglass-window-test--pty-buffer "bounded")))
      (unwind-protect
          (let* ((_mirror (spyglass-test-mirror connection buffer))
                 (share (spyglass-share-of buffer))
                 (sub (spyglass-window-test--sub share)))
            (spyglass--send connection (list :t "sz" :sub (spyglass--sub-id sub)
                                             :c 100000 :r 100000 :f :false))
            (spyglass-test-wait (lambda () (spyglass--sub-size sub)))
            (should (equal (spyglass--sub-size sub) spyglass-max-terminal-size))
            (spyglass--send connection (list :t "sz" :sub (spyglass--sub-id sub)
                                             :c "x" :r -3 :f :false))
            (spyglass-test-wait (lambda () (null (spyglass--sub-size sub))))
            (should (null (spyglass--sub-size sub))))
        (spyglass-window-test--kill buffer)))))

(ert-deftest spyglass-window-sizing-hooks-come-and-go ()
  "The size wrapper and activity hooks stay only while a viewer sizes a terminal."
  (spyglass-test-with-connection connection
    (let ((buffer (spyglass-window-test--pty-buffer "hooks")))
      (unwind-protect
          (spyglass-window-test--with-windows
            (let ((mirror (spyglass-test-mirror connection buffer)))
              (spyglass-test-wait (lambda () (spyglass--mirror-sizable
                                              (buffer-local-value 'spyglass--mirror mirror))))
              (set-window-buffer (selected-window) mirror)
              (spyglass-window-test--report mirror)
              (spyglass-test-wait (lambda () spyglass--sizing))
              (should (memq #'spyglass--owner-command post-command-hook))
              (should (advice-function-member-p
                       #'spyglass--process-window-size
                       (default-value 'window-adjust-process-window-size-function)))
              (spyglass-close mirror)
              (spyglass-test-wait (lambda () (not spyglass--sizing)))
              (should-not (memq #'spyglass--owner-command post-command-hook))
              (should-not (advice-function-member-p
                           #'spyglass--process-window-size
                           (default-value 'window-adjust-process-window-size-function)))))
        (spyglass-window-test--kill buffer)))))

(defconst spyglass-window-test--size-loop "while :; do stty size; sleep 0.1; done"
  "Shell loop printing the size of its terminal, as the program in it sees it.")

(defun spyglass-window-test--printed-size ()
  "Return the size the loop last printed in this terminal, (COLUMNS . ROWS)."
  (save-excursion
    (goto-char (point-max))
    (and (re-search-backward "^\\([0-9]+\\) \\([0-9]+\\)$" nil t)
         (cons (string-to-number (match-string 2)) (string-to-number (match-string 1))))))

(defun spyglass-window-test--terminal (kind check)
  "Size a terminal of KIND from a viewer's window; CHECK its size.
The program in the terminal prints the size of its pty.  CHECK is
called in the terminal's buffer with the size wanted."
  (require 'spyglass-bench-common)
  (spyglass-bench-read-environment)
  (unless (spyglass-bench-terminal-available-p kind)
    (ert-skip (format "No %s here" kind)))
  (spyglass-bench-load-terminal kind)
  (spyglass-test-with-connection connection
    (spyglass-window-test--with-windows
      (let* ((loop spyglass-window-test--size-loop)
             (buffer (pcase kind
                       ("eat" (eat-make "size-eat" "/bin/sh" nil "-c" loop))
                       ("vterm" (with-current-buffer (generate-new-buffer "size-vterm")
                                  (let ((vterm-shell (format "/bin/sh -c %s"
                                                             (shell-quote-argument loop))))
                                    (vterm-mode))
                                  (current-buffer)))
                       ("ghostel" (let ((b (generate-new-buffer "size-ghostel")))
                                    ;; Ghostel renders through a window: the owner's.
                                    (set-window-buffer (selected-window) b)
                                    (ghostel-exec b "/bin/sh" (list "-c" loop))
                                    b)))))
        (unwind-protect
            (let* ((mirror (spyglass-test-mirror connection buffer))
                   (window (if (equal kind "ghostel") (split-window nil 6) (selected-window))))
              (spyglass-test-wait (lambda () (spyglass--mirror-sizable
                                              (buffer-local-value 'spyglass--mirror mirror))))
              (set-window-buffer window mirror)
              (split-window window 20 t)
              (spyglass-window-test--report mirror)
              (let ((size (spyglass-window-cells window)))
                (with-current-buffer buffer
                  (spyglass-test-wait (lambda () (equal (spyglass-window-test--printed-size) size))
                                      10)
                  (should (equal (spyglass-window-test--printed-size) size))
                  (funcall check size))))
          (spyglass-window-test--kill buffer))))))

(defvar eat-terminal)
(defvar vterm-shell)
(defvar ghostel--term-cols)
(defvar ghostel--term-rows)
(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat-term-size "eat" (terminal))
(declare-function vterm-mode "vterm" ())
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))

(ert-deftest spyglass-window-eat ()
  "A viewer's window sizes an Eat terminal and its pty."
  (spyglass-window-test--terminal
   "eat" (lambda (size) (should (equal (eat-term-size eat-terminal) size)))))

(defvar vterm-min-window-width)

(ert-deftest spyglass-window-vterm ()
  "A viewer's window sizes a vterm terminal and its pty."
  ;; vterm keeps terminals 80 columns wide at least, by default.
  (let ((vterm-min-window-width 10))
    (spyglass-window-test--terminal "vterm" #'ignore)))

(ert-deftest spyglass-window-ghostel ()
  "A viewer's window sizes a Ghostel terminal and its pty."
  (spyglass-window-test--terminal
   "ghostel" (lambda (size) (should (equal (cons ghostel--term-cols ghostel--term-rows)
                                           size)))))

(provide 'spyglass-window-test)
;;; spyglass-window-test.el ends here
