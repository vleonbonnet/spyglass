;;; spyglass-e2e-test.el --- Mirror buffers end to end  -*- lexical-binding: t; -*-

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

;; This Emacs shares buffers with itself through 127.0.0.1 and checks
;; that mirrors show exactly what the owner's buffers hold, text and
;; mirrored properties alike.

;;; Code:

(require 'spyglass-test-helpers)
(require 'comint)

(defmacro spyglass-e2e--with-buffer (name &rest body)
  "Run BODY in a fresh buffer called NAME, killed afterwards."
  (declare (indent 1) (debug (form body)))
  `(let ((buffer (generate-new-buffer ,name)))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer)
         (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))

(ert-deftest spyglass-e2e-plain-text ()
  "A buffer's text and later insertions reach its mirror."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "plain"
      (insert "hello\n")
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-converge (current-buffer) mirror)
        (goto-char (point-max))
        (insert "world\n")
        (goto-char 1)
        (insert "first ")
        (spyglass-test-converge (current-buffer) mirror)
        (should (equal (with-current-buffer mirror (buffer-string))
                       "first hello\nworld\n"))
        (should (eq (buffer-local-value 'major-mode mirror) 'spyglass-mirror-mode))
        (should (buffer-local-value 'buffer-read-only mirror))))))

(ert-deftest spyglass-e2e-empty-and-erased ()
  "Empty buffers mirror, and erasing a buffer empties its mirror."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "empty"
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (should (= (buffer-size mirror) 0))
        (insert "x")
        (spyglass-test-converge (current-buffer) mirror)
        (erase-buffer)
        (spyglass-test-converge (current-buffer) mirror)))))

(ert-deftest spyglass-e2e-characters ()
  "Every Emacs character survives: Unicode, raw bytes, NUL, CR."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "chars"
      (insert "é😀\0\r\n\t" (string #x3fff80 #x3fffff #x10ffff #xd800) "end")
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-converge (current-buffer) mirror)
        (should (equal (with-current-buffer mirror (buffer-string)) (buffer-string)))))))

(defun spyglass-e2e--urandom-p ()
  "Return non-nil when `head' reads random bytes from /dev/urandom.
It reads the path itself, as MSYS2's on Windows, where Emacs does not
see /dev."
  (and (executable-find "head")
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (let ((coding-system-for-read 'binary))
           (call-process "head" nil t nil "-c" "16" "/dev/urandom"))
         (= (buffer-size) 16))))

(ert-deftest spyglass-e2e-random-bytes ()
  "Random bytes from a process, raw bytes among them, arrive exactly."
  (skip-unless (spyglass-e2e--urandom-p))
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "urandom"
      (let* ((buffer (current-buffer))
             (mirror (spyglass-test-mirror connection buffer))
             (process (make-process
                       :name "urandom" :command '("head" "-c" "300000" "/dev/urandom")
                       :coding 'utf-8 :connection-type 'pipe
                       :filter (lambda (_p s)
                                 (with-current-buffer buffer
                                   (goto-char (point-max))
                                   (insert s))))))
        (spyglass-test-wait (lambda () (not (process-live-p process))) 20)
        (accept-process-output nil 0.1)
        (should (> (buffer-size) 100000))
        (should (string-match-p "[\200-\377]" (buffer-string)))
        (spyglass-test-converge buffer mirror 20)
        (should (equal (with-current-buffer mirror (buffer-string)) (buffer-string)))))))

(ert-deftest spyglass-e2e-unibyte-owner ()
  "A unibyte buffer mirrors byte for byte, as raw bytes."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "unibyte"
      (set-buffer-multibyte nil)
      (insert (unibyte-string 0 65 200 255 10))
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-wait (lambda () (= (buffer-size mirror) 5)))
        (should (equal (with-current-buffer mirror (buffer-string))
                       (string-to-multibyte (buffer-string))))))))

(ert-deftest spyglass-e2e-faces ()
  "Named faces, anonymous faces and font-lock-face reach the mirror."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "faces"
      (insert (propertize "bold" 'face 'bold) " "
              (propertize "red" 'face '(:foreground "red" :weight bold)) " "
              (propertize "list" 'face '(italic (:underline t))) " "
              (propertize "flf" 'font-lock-face 'warning) " "
              (propertize "keymap" 'keymap '(keymap) 'face 'success))
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-converge (current-buffer) mirror)
        (with-current-buffer mirror
          (should (eq (get-text-property 1 'face) 'bold))
          (should (equal (get-text-property 6 'face) '(:foreground "red" :weight bold)))
          (should (null (get-text-property 15 'face)))
          (should (null (text-property-not-all 1 (point-max) 'keymap nil))))
        ;; Where font lock shows `font-lock-face', it is mirrored as a face.
        (setq-local char-property-alias-alist '((face font-lock-face)))
        (put-text-property 1 2 'face 'italic)
        (spyglass-test-converge (current-buffer) mirror)
        (should (eq (get-text-property 15 'face mirror) 'warning))))))

(defface spyglass-e2e-test-face '((t :foreground "orange" :weight bold :inherit italic))
  "A face the owner has."
  :group 'spyglass)

(ert-deftest spyglass-e2e-viewer-asks-for-fontification ()
  "A mirror's redisplay asks the owner to fontify what it shows."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "code.el"
      (emacs-lisp-mode)
      (insert "(defun foo ()\n  \"Doc.\"\n  1)\n")
      ;; Font lock declines batch sessions; this one plays interactive.
      (let ((noninteractive nil)) (font-lock-mode 1))
      (should (bound-and-true-p jit-lock-mode))
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-converge (current-buffer) mirror)
        (should (null (get-text-property 2 'face mirror)))
        (should (memq #'spyglass--mirror-jit
                      (buffer-local-value 'jit-lock-functions mirror)))
        ;; What redisplay of the mirror would do.
        (with-current-buffer mirror (spyglass--mirror-jit (point-min) (point-max)))
        (spyglass-test-wait (lambda () (get-text-property 2 'face mirror)))
        (should (eq (get-text-property 2 'face mirror) 'font-lock-keyword-face))
        (spyglass-test-converge (current-buffer) mirror)))))

(ert-deftest spyglass-e2e-unknown-face-falls-back ()
  "A face the viewer lacks is replaced by the owner's attributes."
  ;; Both ends are this Emacs: the owner's side computes the attributes,
  ;; and the viewer's side is asked about a face it does not know.
  (let ((attrs (spyglass--face-fallback 'spyglass-e2e-test-face))
        (mirror (spyglass--make-mirror :faces (make-hash-table :test #'equal))))
    (should (equal (plist-get attrs :foreground) "orange"))
    (should (eq (plist-get attrs :weight) 'bold))
    (should (eq (plist-get attrs :slant) 'italic))
    (puthash "spyglass-no-such-face" attrs (spyglass--mirror-faces mirror))
    (should (equal (spyglass--mirror-resolve-face mirror 'spyglass-no-such-face) attrs))
    (should (equal (spyglass--mirror-resolve-face mirror '(bold spyglass-no-such-face))
                   (list 'bold attrs)))
    (should (equal (spyglass--mirror-resolve-face
                    mirror '(:underline t :inherit (bold spyglass-no-such-face)))
                   (list '(:underline t :inherit bold) attrs)))
    (should (null (spyglass--mirror-resolve-face mirror 'spyglass-unheard-of-face)))))

(ert-deftest spyglass-e2e-display-and-invisible ()
  "Display strings and invisibility mirror; evaluated displays do not."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "display"
      (insert (propertize "hidden" 'invisible 'secret) " "
              (propertize "shown" 'display (propertize "SHOWN" 'face 'bold)) " "
              (propertize "evil" 'display '(when (error "evaluated") . "x")))
      (add-to-invisibility-spec 'secret)
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-converge (current-buffer) mirror)
        (spyglass-test-wait (lambda () (equal (buffer-local-value 'buffer-invisibility-spec mirror)
                                              buffer-invisibility-spec)))
        (with-current-buffer mirror
          (should (eq (get-text-property 1 'invisible) 'secret))
          (should (equal-including-properties (get-text-property 8 'display)
                                              (propertize "SHOWN" 'face 'bold)))
          (should (null (get-text-property 14 'display))))))))

(ert-deftest spyglass-e2e-overlays ()
  "Overlays appear, move, change and vanish in the mirror."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "overlays"
      (insert "0123456789abcdefghij\n")
      (let* ((mirror (spyglass-test-mirror connection (current-buffer)))
             (a (make-overlay 2 5))
             (b (make-overlay 8 8)))
        (overlay-put a 'face 'bold)
        (overlay-put b 'before-string (propertize ">>" 'face 'italic))
        (let ((c (make-overlay 10 12)))
          (overlay-put c 'face 'region))
        ;; An overlay change alone, no text changing: the sweep finds it.
        (spyglass-test-wait (lambda () (equal (spyglass-test-overlays mirror)
                                              (spyglass-test-overlays (current-buffer) t)))
                            5)
        (should (equal (spyglass-test-overlays mirror)
                       (spyglass-test-overlays (current-buffer) t)))
        (should (= (length (spyglass-test-overlays mirror)) 2))
        (move-overlay a 3 15)
        (overlay-put a 'face 'italic)
        (goto-char 1) (insert "new text before ")
        (delete-overlay b)
        (spyglass-test-wait (lambda () (equal (spyglass-test-overlays mirror)
                                              (spyglass-test-overlays (current-buffer) t)))
                            5)
        (should (equal (spyglass-test-overlays mirror)
                       (spyglass-test-overlays (current-buffer) t)))
        (spyglass-test-converge (current-buffer) mirror)))))

(ert-deftest spyglass-e2e-silent-changes ()
  "Changes made without modification hooks reach the mirror."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "silent"
      (dotimes (i 200) (insert (format "line %03d of the terminal\n" i)))
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        ;; Unhinted: insertions, deletions and same-length rewrites.
        (let ((inhibit-modification-hooks t))
          (goto-char (point-max)) (insert "appended silently\n")
          (goto-char 1) (delete-region 1 (line-beginning-position 3))
          (goto-char 2000) (delete-region 2000 2005) (insert "XXXXX")
          (put-text-property 100 110 'face 'bold))
        (spyglass-test-converge (current-buffer) mirror)
        ;; Hinted, as a terminal adapter does.
        (let ((inhibit-modification-hooks t))
          (goto-char 3000) (delete-region 3000 3010) (insert "0123456789")
          (spyglass-notify (current-buffer) 3000 3010))
        (spyglass-test-converge (current-buffer) mirror)))))

(ert-deftest spyglass-e2e-poll-survives-a-throw ()
  "Changes without hooks still reach the mirror after a `throw' left a poll.
Emacs runs a repeating timer again only once its function returned: a
`with-timeout' firing in a wait the poll made left it never to run
again, and a terminal whose renders went untold stopped reaching its
viewers."
  (let ((stage (list nil)))
    (cl-flet ((poll (_share) (spyglass-test-wait-in-timer stage)))
      (add-hook 'spyglass--poll-functions #'poll)
      (unwind-protect
          (spyglass-test-with-connection connection
                                         (spyglass-e2e--with-buffer "thrown"
                                           (insert "before\n")
                                           (let ((mirror (spyglass-test-mirror connection (current-buffer))))
                                             (setcar stage 'armed)
                                             (should (eq (spyglass-test-throw-through stage) 'waiting))
                                             (let ((inhibit-modification-hooks t))
                                               (goto-char (point-max))
                                               (insert "after, without hooks\n"))
                                             (spyglass-test-converge (current-buffer) mirror 5))))
        (remove-hook 'spyglass--poll-functions #'poll)))))

(ert-deftest spyglass-e2e-loading-restarts-an-older-poll ()
  "Loading spyglass again restarts a poll an older one started, stuck or not.
Its timer called `spyglass--poll' directly: a running Emacs whose poll
a `throw' had left, as Core's had for hours, found its changes again
once it loaded this spyglass."
  (let ((stage (list nil)))
    (cl-flet ((poll (_share) (spyglass-test-wait-in-timer stage)))
      (add-hook 'spyglass--poll-functions #'poll)
      (unwind-protect
          (spyglass-test-with-connection connection
                                         (spyglass-e2e--with-buffer "older"
                                           (insert "before\n")
                                           (let ((mirror (spyglass-test-mirror connection (current-buffer))))
                                             ;; The poll as an older spyglass started it.
                                             (cancel-timer spyglass--poll-timer)
                                             (setq spyglass--poll-timer
                                                   (run-with-timer spyglass-poll-interval spyglass-poll-interval
                                                                   #'spyglass--poll))
                                             (setcar stage 'armed)
                                             (should (eq (spyglass-test-throw-through stage) 'waiting))
                                             (load "spyglass-owner" nil t)
                                             (should (eq (timer--function spyglass--poll-timer) #'spyglass--run-repeating))
                                             (let ((inhibit-modification-hooks t))
                                               (goto-char (point-max))
                                               (insert "after, without hooks\n"))
                                             (spyglass-test-converge (current-buffer) mirror 5))))
        (remove-hook 'spyglass--poll-functions #'poll)))))

(defun spyglass-e2e--random-edit (silent)
  "Make one random edit in the current buffer, without hooks if SILENT."
  (let ((inhibit-modification-hooks silent)
        (size (buffer-size)))
    (pcase (random 6)
      (0 (goto-char (1+ (random (1+ size))))
         (insert (make-string (random 30) (+ ?a (random 26)))))
      (1 (goto-char (point-max))
         (insert (propertize (format "out %d\n" (random 1000))
                             'face (nth (random 3) '(nil bold (:foreground "blue"))))))
      (2 (when (> size 0)
           (let ((beg (1+ (random size))))
             (delete-region beg (min (point-max) (+ beg (random 40)))))))
      (3 (when (> size 10)
           (let* ((beg (1+ (random (- size 10)))) (end (+ beg (random 10))))
             (put-text-property beg end 'face (nth (random 3) '(italic bold nil))))))
      (4 (when (> size 10)
           (let* ((beg (1+ (random (- size 10)))) (len (random 10)))
             (goto-char beg)
             (delete-region beg (+ beg len))
             (insert (make-string len ?Z)))))
      (5 (when (> size 2000)
           (delete-region 1 (1+ (random 500))))))))

(ert-deftest spyglass-e2e-random-edits ()
  "Random edits, with and without hooks, at random times, mirror exactly."
  (random "spyglass-e2e")
  (dolist (capture '(shadow direct))
    (spyglass-test-with-connection connection
      (spyglass-e2e--with-buffer "random"
        (dotimes (i 100) (insert (format "initial line %d\n" i)))
        (spyglass-share (current-buffer) :capture capture)
        (let ((mirror (spyglass-test-mirror connection (current-buffer))))
          (dotimes (_ 1500)
            (spyglass-e2e--random-edit (and (eq capture 'shadow) (zerop (random 3))))
            (when (zerop (random 20)) (accept-process-output nil 0.001)))
          (spyglass-test-converge (current-buffer) mirror 30))))))

(ert-deftest spyglass-e2e-direct-mode-resyncs-silent-changes ()
  "In direct mode, a change of text without hooks resends the buffer."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "direct"
      (insert "abc\n")
      (spyglass-share (current-buffer) :capture 'direct)
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (let ((inhibit-modification-hooks t)) (goto-char 2) (insert "SILENT"))
        (insert "hooked")
        (spyglass-test-converge (current-buffer) mirror)
        (should (>= (plist-get (spyglass-share-stats (spyglass-share-of (current-buffer)))
                               :resyncs)
                    1))))))

(ert-deftest spyglass-e2e-shadow-to-direct ()
  "A buffer growing past the shadow limit switches to direct capture."
  (let ((spyglass-shadow-limit 20000))
    (spyglass-test-with-connection connection
      (spyglass-e2e--with-buffer "growing"
        (insert "start\n")
        (let ((mirror (spyglass-test-mirror connection (current-buffer)))
              (ov (make-overlay 1 3)))
          (overlay-put ov 'face 'bold)
          (spyglass-test-wait (lambda () (spyglass-test-overlays mirror)) 5)
          (should (spyglass-test-overlays mirror))
          (dotimes (i 5000) (insert (format "line %d\n" i)))
          (spyglass-test-converge (current-buffer) mirror)
          (should (eq (spyglass-share-mode (spyglass-share-of (current-buffer))) 'direct))
          ;; Overlays are not mirrored in direct capture.
          (spyglass-test-wait (lambda () (null (spyglass-test-overlays mirror))) 5)
          (should (null (spyglass-test-overlays mirror)))
          (goto-char 3) (insert "more")
          (spyglass-test-converge (current-buffer) mirror))))))

(ert-deftest spyglass-e2e-many-small-appends ()
  "Ten thousand small appends arrive byte for byte."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "appends"
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (dotimes (i 10000)
          (goto-char (point-max))
          (insert (format "%05d " i))
          (insert (md5 (number-to-string i)))
          (insert "\n")
          (when (zerop (% i 500)) (accept-process-output nil 0)))
        (spyglass-test-converge (current-buffer) mirror 30)))))

(ert-deftest spyglass-e2e-large-snapshot ()
  "A buffer larger than a frame arrives in several frames."
  (let ((spyglass-frame-chars 100000))
    (spyglass-test-with-connection connection
      (spyglass-e2e--with-buffer "large"
        (dotimes (i 30000) (insert (format "row %06d %s\n" i (make-string 20 ?.))))
        (put-text-property 1000 2000 'face 'bold)
        (let ((mirror (spyglass-test-mirror connection (current-buffer))))
          (spyglass-test-converge (current-buffer) mirror 30)
          (should (> (plist-get (spyglass-share-stats (spyglass-share-of (current-buffer)))
                                :frames)
                     5)))))))

(ert-deftest spyglass-e2e-flow-control ()
  "A viewer that stops acknowledging bounds what the owner holds for it."
  (let ((spyglass-window 200000) (spyglass-frame-chars 50000))
    (spyglass-test-with-connection connection
      (spyglass-e2e--with-buffer "flow"
        (let* ((mirror (spyglass-test-mirror connection (current-buffer)))
               (share (spyglass-share-of (current-buffer)))
               (sub (car (spyglass-share-subs share))))
          (let ((spyglass--after-input-functions nil)) ; No acknowledgments.
            (dotimes (i 2000)
              (goto-char (point-max))
              (insert (format "%d %s\n" i (make-string 200 ?x)))
              (when (zerop (% i 50)) (accept-process-output nil 0.005)))
            (spyglass-test-settle 0.3)
            (should (<= (spyglass--sub-inflight sub)
                        (+ spyglass-window (* 4 spyglass-frame-chars))))
            (should (<= (length (spyglass--sub-intervals sub)) spyglass-max-intervals))
            (should (spyglass--sub-intervals sub)))
          ;; Acknowledgments resume, and so does sending.
          (spyglass--mirror-acknowledge connection)
          (spyglass-test-converge (current-buffer) mirror 30))))))

(ert-deftest spyglass-e2e-follow-point ()
  "The owner's cursor reaches the mirror."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "point"
      (insert "abcdef\nghijkl\n")
      (goto-char 4)
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-wait (lambda () (eql (spyglass-mirror-owner-point mirror) 4)))
        (should (eql (spyglass-mirror-owner-point mirror) 4))
        (goto-char 10)
        (spyglass-test-wait (lambda () (eql (spyglass-mirror-owner-point mirror) 10)))
        (should (eql (spyglass-mirror-owner-point mirror) 10))
        (should (eql (with-current-buffer mirror (point)) 10))))))

(ert-deftest spyglass-e2e-text-input ()
  "A viewer types into an editable buffer at its point."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "editable"
      (insert "ab")
      (goto-char 2)
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-mirror-send-string "XY" mirror)
        (spyglass-mirror-send-key "RET" mirror)
        (spyglass-test-wait (lambda () (equal (buffer-string) "aXY\nb")))
        (should (equal (buffer-string) "aXY\nb"))
        (spyglass-test-converge (current-buffer) mirror)))))

(ert-deftest spyglass-e2e-read-only-share ()
  "A read-only share refuses input and says so."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "ro"
      (insert "ab")
      (spyglass-share (current-buffer) :read-only t)
      (let* ((errors nil)
             (spyglass-error-functions (list (lambda (_c msg) (push msg errors))))
             (mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-wait (lambda () (plist-get (spyglass--mirror-meta
                                                   (buffer-local-value 'spyglass--mirror mirror))
                                                  :in)))
        (should-error (spyglass-mirror-send-string "x" mirror) :type 'user-error)
        ;; Sent anyway, as a peer ignoring the flag would: refused.
        (spyglass--send connection (list :t "in" :sub 1 :k "s" :v "x"))
        (spyglass-test-wait (lambda () errors))
        (should (string-match-p "read-only" (car errors)))
        (should (equal (buffer-string) "ab"))))))

(ert-deftest spyglass-e2e-comint-input ()
  "A viewer types into a comint buffer and submits with RET."
  (skip-unless (executable-find "cat"))
  (spyglass-test-with-connection connection
    (let ((buffer (make-comint "spyglass-cat" "cat")))
      (unwind-protect
          (with-current-buffer buffer
            (let ((mirror (spyglass-test-mirror connection buffer)))
              (spyglass-mirror-send-string "hello from the viewer" mirror)
              (spyglass-mirror-send-key "RET" mirror)
              (spyglass-test-wait
               (lambda () (>= (how-many "hello from the viewer" (point-min) (point-max)) 2)))
              (should (>= (how-many "hello from the viewer" (point-min) (point-max)) 2))
              (spyglass-test-converge buffer mirror)))
        (let ((kill-buffer-query-functions nil))
          (when (get-buffer-process buffer) (delete-process (get-buffer-process buffer)))
          (kill-buffer buffer))))))

(ert-deftest spyglass-e2e-submit-function ()
  "RET in a comint mirror runs the buffer's submit function."
  (skip-unless (executable-find "cat"))
  (spyglass-test-with-connection connection
    (let ((buffer (make-comint "spyglass-cat2" "cat")) submitted)
      (unwind-protect
          (with-current-buffer buffer
            (setq-local spyglass-submit-function (lambda () (setq submitted (point)))))
        nil)
      (unwind-protect
          (let ((mirror (spyglass-test-mirror connection buffer)))
            (spyglass-mirror-send-key "RET" mirror)
            (spyglass-test-wait (lambda () submitted))
            (should submitted))
        (let ((kill-buffer-query-functions nil))
          (when (get-buffer-process buffer) (delete-process (get-buffer-process buffer)))
          (kill-buffer buffer))))))

(defun spyglass-e2e--kill-comint (buffer)
  "Kill BUFFER, a comint buffer, and its process."
  (when (buffer-live-p buffer)
    (let ((kill-buffer-query-functions nil))
      (when (get-buffer-process buffer) (delete-process (get-buffer-process buffer)))
      (kill-buffer buffer))))

(defun spyglass-e2e--owner-point (mirror)
  "Return the owner's cursor MIRROR shows, once it settled."
  (spyglass-test-settle 0.1)
  (spyglass-mirror-owner-point mirror))

(ert-deftest spyglass-e2e-comint-input-point ()
  "A viewer edits a shell's input at its input point, which its mirror shows and which moves as it types."
  (skip-unless (executable-find "cat"))
  (spyglass-test-with-connection connection
    (let ((buffer (make-comint "spyglass-point" "cat")))
      (unwind-protect
          (with-current-buffer buffer
            (insert "output above\n")
            (set-marker (process-mark (get-buffer-process buffer)) (point-max))
            ;; The owner reads elsewhere: input still goes to the input.
            (goto-char (point-min))
            (let ((mirror (spyglass-test-mirror connection buffer))
                  (start (marker-position (process-mark (get-buffer-process buffer)))))
              (should (= (spyglass-e2e--owner-point mirror) (point-max)))
              (spyglass-mirror-send-string "abc" mirror)
              (spyglass-test-wait (lambda () (equal (buffer-substring start (point-max)) "abc")))
              (should (= (point) (point-max)))
              (should (= (spyglass-e2e--owner-point mirror) (with-current-buffer mirror (point-max))))
              ;; Moved within the input, edits apply there.
              (spyglass-mirror-send-key "<left>" mirror)
              (spyglass-mirror-send-key "DEL" mirror)
              (spyglass-mirror-send-string "X" mirror)
              (spyglass-test-wait (lambda () (equal (buffer-substring start (point-max)) "aXc")))
              (should (equal (buffer-substring start (point-max)) "aXc"))
              (should (= (point) (+ start 2)))
              (should (= (spyglass-e2e--owner-point mirror) (+ start 2)))
              ;; Never into the output above.
              (dotimes (_ 5) (spyglass-mirror-send-key "DEL" mirror))
              (spyglass-mirror-send-key "<home>" mirror)
              (spyglass-mirror-send-key "<left>" mirror)
              (spyglass-test-wait (lambda () (equal (buffer-substring start (point-max)) "c")))
              (should (equal (buffer-substring (point-min) (point-max)) "output above\nc"))
              (should (= (spyglass-e2e--owner-point mirror) start))
              (spyglass-test-converge buffer mirror)))
        (spyglass-e2e--kill-comint buffer)))))

(ert-deftest spyglass-e2e-mirror-point-returns-to-the-input ()
  "A viewer reads where it likes in its mirror; typing brings its point back to the owner's cursor."
  (skip-unless (executable-find "cat"))
  (spyglass-test-with-connection connection
    (let ((buffer (make-comint "spyglass-reader" "cat")))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer buffer
              (dotimes (i 50) (insert (format "line %d\n" i)))
              (set-marker (process-mark (get-buffer-process buffer)) (point-max)))
            (let ((mirror (spyglass-test-mirror connection buffer)))
              (spyglass-test-converge buffer mirror)
              (set-window-buffer (selected-window) mirror)
              (select-window (get-buffer-window mirror))
              (goto-char (point-min))
              (spyglass-mirror-send-string "typed" mirror)
              (should (= (window-point) (spyglass-mirror-owner-point mirror)))
              (spyglass-test-wait (lambda () (with-current-buffer mirror
                                               (string-suffix-p "typed" (buffer-string)))))
              ;; Following again: the window's point is past what was typed.
              (spyglass-test-settle 0.1)
              (should (= (window-point) (with-current-buffer mirror (point-max))))))
        (spyglass-e2e--kill-comint buffer)))))

(defvar spyglass-e2e--submitted nil
  "What the stand-in shell submitted, newest first.")

(defvar spyglass-e2e--interrupted nil
  "Non-nil once the stand-in shell was interrupted.")

(defun spyglass-e2e--shell-submit ()
  "Submit the stand-in shell's input, as agent-shell's submit does."
  (interactive)
  (goto-char (point-max))
  (let ((input (buffer-substring-no-properties
                (process-mark (get-buffer-process (current-buffer))) (point-max))))
    (push input spyglass-e2e--submitted)
    (insert "\n")
    (set-marker (process-mark (get-buffer-process (current-buffer))) (point-max))))

(defun spyglass-e2e--shell-interrupt ()
  "Interrupt the stand-in shell, asking first as agent-shell may."
  (interactive)
  (when (y-or-n-p "Interrupt? ")
    (setq spyglass-e2e--interrupted t)))

(defvar-keymap spyglass-e2e--shell-mode-map
  :parent comint-mode-map
  "<remap> <comint-send-input>" #'spyglass-e2e--shell-submit
  "C-c C-c" #'spyglass-e2e--shell-interrupt)

(define-derived-mode spyglass-e2e--shell-mode comint-mode "Stand-in shell"
  "A comint buffer submitting as agent-shell does: RET is its own command.
`comint-send-input' alone only keeps the input, as shell-maker's
`comint-input-sender' does, the line committed and nothing sent."
  (setq-local comint-input-sender (lambda (_process _input))))

(ert-deftest spyglass-e2e-key-commands ()
  "A shell whose RET is its own command submits through it once listed, never otherwise looked up."
  (skip-unless (executable-find "cat"))
  (spyglass-test-with-connection connection
    (let ((buffer (make-comint "spyglass-agent" "cat")))
      (unwind-protect
          (with-current-buffer buffer
            (spyglass-e2e--shell-mode)
            (set-marker (process-mark (get-buffer-process buffer)) (point-max))
            (setq spyglass-e2e--submitted nil spyglass-e2e--interrupted nil)
            (let ((mirror (spyglass-test-mirror connection buffer))
                  (errors nil))
              ;; Not listed: comint's own, which submits nothing here.
              (spyglass-mirror-send-string "first" mirror)
              (spyglass-mirror-send-key "RET" mirror)
              (spyglass-test-settle 0.2)
              (should-not spyglass-e2e--submitted)
              ;; Listed, as drover lists a front end's: its own submit.
              (setq-local spyglass-key-commands
                          '(("RET" . spyglass-e2e--shell-submit) ("C-c" . spyglass-e2e--shell-interrupt)))
              (spyglass-mirror-send-string "second" mirror)
              (spyglass-mirror-send-key "RET" mirror)
              (spyglass-test-wait (lambda () spyglass-e2e--submitted))
              (should (equal spyglass-e2e--submitted '("second")))
              ;; A command asking the owner's user fails, never waits.
              (let ((spyglass-error-functions (list (lambda (_c message) (push message errors)))))
                (spyglass-mirror-send-key "C-c" mirror)
                (spyglass-test-wait (lambda () errors) 5))
              (should-not spyglass-e2e--interrupted)
              (should errors)
              (spyglass-test-converge buffer mirror)))
        (spyglass-e2e--kill-comint buffer)))))

(ert-deftest spyglass-e2e-faces-in-strings-fall-back ()
  "Faces of the strings an overlay shows, as a label's display string, reach viewers lacking them."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "labels"
      (insert "Claude> ")
      (let ((label (make-overlay 1 2)))
        (overlay-put label 'display (concat (propertize " Me " 'face 'spyglass-e2e-test-face) "\n"))
        (overlay-put label 'line-prefix "")
        (overlay-put label 'wrap-prefix "  "))
      (let* ((mirror (spyglass-test-mirror connection (current-buffer)))
             (faces (spyglass--mirror-faces (buffer-local-value 'spyglass--mirror mirror))))
        (spyglass-test-wait (lambda () (gethash "spyglass-e2e-test-face" faces)))
        ;; Its attributes as the owner resolves them, for a viewer without it.
        (should (equal (plist-get (gethash "spyglass-e2e-test-face" faces) :foreground) "orange"))
        (should (eq (plist-get (gethash "spyglass-e2e-test-face" faces) :slant) 'italic))
        ;; The overlay's prefixes come along: they change the look of its lines.
        (spyglass-test-wait (lambda () (with-current-buffer mirror
                                         (seq-some (lambda (ov) (overlay-get ov 'wrap-prefix))
                                                   (overlays-in (point-min) (point-max))))))
        (let ((ov (with-current-buffer mirror
                    (seq-find (lambda (ov) (overlay-get ov 'display)) (overlays-in (point-min) (point-max))))))
          (should (equal (overlay-get ov 'line-prefix) ""))
          (should (equal (overlay-get ov 'wrap-prefix) "  "))
          (should (eq (get-text-property 1 'face (overlay-get ov 'display)) 'spyglass-e2e-test-face)))))))

(ert-deftest spyglass-e2e-cursor-at-the-end ()
  "The owner's cursor at the end of the buffer takes the window's cursor, not the cell before it."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "cursor-end"
      (insert "Claude> ")
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-test-wait (lambda () (eql (spyglass-mirror-owner-point mirror) (with-current-buffer mirror (point-max)))))
        (let ((cursor (spyglass--mirror-cursor (buffer-local-value 'spyglass--mirror mirror))))
          (should (= (overlay-start cursor) (with-current-buffer mirror (point-max))))
          (should (get-text-property 0 'cursor (overlay-get cursor 'after-string))))))))

(ert-deftest spyglass-e2e-mode-line-header-line-and-remapping ()
  "A mirror draws this Emacs's mode line, telling of itself where a process would; the owner's header line and remapping come."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "lines"
      (insert "text\n")
      (setq-local mode-line-format '("OWNER-ML %b"))
      (setq-local header-line-format "OWNER-HL")
      (face-remap-add-relative 'bold :foreground "red")
      (let ((owner (current-buffer))
            (rendered nil))
        ;; A batch Emacs renders no header line: the owner's rendering is
        ;; stood for, noting what it was asked to render.
        (cl-letf (((symbol-function 'format-mode-line)
                   (lambda (format &optional _face _window buffer)
                     (when (eq buffer owner) (push format rendered))
                     (if (and (eq buffer owner) (equal format "OWNER-HL")) "OWNER-HL" ""))))
          (save-window-excursion
            (set-window-buffer (selected-window) owner)
            (let* ((mirror (spyglass-test-mirror connection owner))
                   (m (buffer-local-value 'spyglass--mirror mirror)))
              (spyglass-test-wait (lambda () (buffer-local-value 'header-line-format mirror)))
              ;; The owner renders its header line alone, and sends no mode line.
              (should (member "OWNER-HL" rendered))
              (should-not (member '("OWNER-ML %b") rendered))
              (should-not (plist-member (spyglass-share-meta (spyglass-share-of owner)) :ml))
              (with-current-buffer mirror
                ;; This Emacs's mode line, the mirror's state where a
                ;; buffer's process tells its own.
                (should-not (local-variable-p 'mode-line-format))
                (should (equal mode-line-process '(:eval (spyglass--mirror-mode-line-process))))
                (should (string-match-p "\\` [^ ]+@self \\[live\\]\\'" (spyglass--mirror-mode-line-process)))
                ;; What the application tells comes first, spaced from
                ;; the mode name, its percent sign kept as such.
                (setq-local spyglass-mirror-mode-line-function (lambda () "Idle 45%"))
                (let ((line (spyglass--mirror-mode-line-process)))
                  (should (string-prefix-p " Idle 45%% " line))
                  (should (string-match-p "@self \\[live\\]\\'" line)))
                (setq-local spyglass-mirror-mode-line-function (lambda () "  Idle 45%  "))
                (should (string-match-p "\\` Idle 45%% [^ ]" (spyglass--mirror-mode-line-process)))
                (setq-local spyglass-mirror-mode-line-function (lambda () " "))
                (should (string-match-p "\\` [^ ]+@self \\[live\\]\\'" (spyglass--mirror-mode-line-process)))
                ;; An application failing leaves the state alone.
                (setq-local spyglass-mirror-mode-line-function (lambda () (error "Broken")))
                (should (string-match-p "\\` [^ ]+@self \\[live\\]\\'" (spyglass--mirror-mode-line-process)))
                (setq-local spyglass-mirror-mode-line-function nil)
                ;; An older owner's mode line is not shown.
                (spyglass--mirror-apply-meta m (append '(:ml "OLDER-OWNER-ML") (spyglass--mirror-meta m)))
                (should-not (local-variable-p 'mode-line-format))
                (should-not (string-search "OLDER" (spyglass--mirror-mode-line-process)))
                (should (equal (spyglass--mirror-header-line) "OWNER-HL"))
                (should header-line-format)
                (should (assq 'bold face-remapping-alist)))))
          ;; No window of the owner's shows it: the header still the owner's.
          (let ((mirror (car (spyglass-mirrors connection))))
            (setf (spyglass-share-meta-time (spyglass-share-of owner)) 0.0)
            (insert "more\n")
            (spyglass-test-converge owner mirror)
            (with-current-buffer mirror
              (should (equal (spyglass--mirror-header-line) "OWNER-HL"))))
          ;; The remapping gone there, and the header line: gone here.
          (setq-local face-remapping-alist nil)
          (setq-local header-line-format nil)
          (setf (spyglass-share-meta-time (spyglass-share-of owner)) 0.0)
          (insert "again\n")
          (let ((mirror (car (spyglass-mirrors connection))))
            (spyglass-test-wait (lambda () (null (buffer-local-value 'face-remapping-alist mirror))))
            (should-not (buffer-local-value 'face-remapping-alist mirror))
            (should-not (buffer-local-value 'header-line-format mirror))))))))

(ert-deftest spyglass-e2e-reloaded-while-mirrored ()
  "Spyglass loaded anew, as updating it in a running Emacs does, keeps its shares and mirrors going."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "reloaded"
      (insert "before\n")
      (let* ((mirror (spyglass-test-mirror connection (current-buffer)))
             (shares (length (spyglass-shares))))
        ;; A mirror an older spyglass opened draws a mode line of its own.
        (with-current-buffer mirror
          (setq-local mode-line-format '("%e" (:eval (spyglass--mirror-mode-line))))
          (kill-local-variable 'mode-line-process))
        (dolist (file '("spyglass-wire" "spyglass-conn" "spyglass-channel" "spyglass-owner"
                        "spyglass-mirror" "spyglass-window" "spyglass"))
          (load file nil t))
        (should (= (length (spyglass-shares)) shares))
        (should (spyglass-share-of (current-buffer)))
        (should (eq (spyglass-mirror-state mirror) 'live))
        ;; It draws this Emacs's now.
        (should-not (local-variable-p 'mode-line-format mirror))
        (should (equal (buffer-local-value 'mode-line-process mirror)
                       '(:eval (spyglass--mirror-mode-line-process))))
        (goto-char (point-max))
        (insert "after\n")
        (spyglass-test-converge (current-buffer) mirror)
        (spyglass-mirror-send-string "typed" mirror)
        (spyglass-test-wait (lambda () (string-search "typed" (buffer-string))))
        (should (string-search "typed" (buffer-string)))))))

(ert-deftest spyglass-e2e-closed-mirror-says-why ()
  "Typing in a mirror that closed says it is closed, and why, as the application tells it; yanking sends the kill."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "closing"
      (insert "text\n")
      (let ((mirror (spyglass-test-mirror connection (current-buffer)))
            (kill-ring nil))
        (kill-new "pasted words")
        (with-current-buffer mirror (spyglass-mirror-yank))
        (spyglass-test-wait (lambda () (string-search "pasted words" (buffer-string))))
        (should (string-search "pasted words" (buffer-string)))
        (spyglass-unshare (current-buffer))
        (spyglass-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'closed)))
        (should (equal (spyglass-mirror-closed-reason mirror) "unshared"))
        (spyglass-mirror-set-closed-reason mirror "the terminal on its host was closed")
        (should (string-match-p "closed: the terminal on its host was closed"
                                (cadr (should-error (spyglass-mirror-send-string "x" mirror)
                                                    :type 'user-error))))))))

(ert-deftest spyglass-e2e-reconnect-resumes ()
  "A broken stream comes back, and the mirror resumes where it was."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "resume"
      (dotimes (i 1000) (insert (format "before %d\n" i)))
      (let* ((mirror (spyglass-test-mirror connection (current-buffer)))
             (share (spyglass-share-of (current-buffer))))
        (spyglass-test-converge (current-buffer) mirror)
        (dotimes (round 3)
          (dotimes (i 300) (goto-char (point-max)) (insert (format "during %d %d\n" round i)))
          (spyglass-connection-drop-stream connection)
          (should (eq (spyglass-mirror-state mirror) 'reconnecting))
          (dotimes (i 300) (goto-char (point-max)) (insert (format "while down %d %d\n" round i)))
          (spyglass-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'live)))
          (spyglass-test-converge (current-buffer) mirror))
        (should (= (plist-get (spyglass-share-stats share) :resumes) 3))
        (should (= (plist-get (spyglass-share-stats share) :snapshots) 1))))))

(ert-deftest spyglass-e2e-expired-subscription-starts-over ()
  "A viewer coming back after its subscription expired gets a new snapshot."
  (let ((spyglass-resume-timeout 0.1))
    (spyglass-test-with-connection connection
      (spyglass-e2e--with-buffer "expire"
        (insert "abc")
        (let* ((mirror (spyglass-test-mirror connection (current-buffer)))
               (share (spyglass-share-of (current-buffer)))
               (spyglass-reconnect-delays '(0.5)))
          (spyglass-connection-drop-stream connection)
          (insert "def")
          (spyglass-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'live)))
          (spyglass-test-converge (current-buffer) mirror)
          (should (= (plist-get (spyglass-share-stats share) :snapshots) 2)))))))

(ert-deftest spyglass-e2e-unshare-and-kill ()
  "Unsharing or killing a buffer closes its mirrors; closing a mirror ends it."
  (spyglass-test-with-connection connection
    (let ((a (generate-new-buffer "a")) (b (generate-new-buffer "b"))
          (c (generate-new-buffer "c")))
      (let ((ma (spyglass-test-mirror connection a))
            (mb (spyglass-test-mirror connection b))
            (mc (spyglass-test-mirror connection c)))
        (spyglass-unshare a)
        (kill-buffer b)
        (spyglass-close mc)
        (spyglass-test-wait (lambda () (and (eq (spyglass-mirror-state ma) 'closed)
                                            (eq (spyglass-mirror-state mb) 'closed))))
        (should (equal (spyglass-mirror-closed-reason ma) "unshared"))
        (should (equal (spyglass-mirror-closed-reason mb) "killed"))
        (should (eq (spyglass-mirror-state mc) 'closed))
        (spyglass-test-wait (lambda () (null (spyglass-share-subs (spyglass-share-of c)))))
        (should (null (spyglass-share-subs (spyglass-share-of c))))
        (should (null (spyglass-share-mode (spyglass-share-of c)))))
      (kill-buffer a) (kill-buffer c))))

(ert-deftest spyglass-e2e-shares-are-announced ()
  "Peers learn of shares as they come and go."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "announced"
      (spyglass-share (current-buffer))
      (spyglass-test-wait (lambda () (spyglass-connection-shares connection)))
      (should (equal (mapcar (lambda (s) (plist-get s :id)) (spyglass-connection-shares connection))
                     '("announced")))
      (let (listed)
        (spyglass-list-shares connection (lambda (shares) (setq listed (or shares :none))))
        (spyglass-test-wait (lambda () listed))
        (should (equal (plist-get (car listed) :name) "announced")))
      (spyglass-unshare (current-buffer))
      (spyglass-test-wait (lambda () (null (spyglass-connection-shares connection))))
      (should (null (spyglass-connection-shares connection))))))

(ert-deftest spyglass-e2e-wrong-token-is-refused ()
  "A client presenting the wrong token is refused and keeps retrying."
  (unwind-protect
      (let* ((server (spyglass-test-start))
             (errors nil)
             (spyglass-error-functions (list (lambda (_c msg) (push msg errors))))
             (connection (spyglass-connect
                          (spyglass-tcp-transport "127.0.0.1" (spyglass-server-port server)
                                                  "wrong"))))
        (spyglass-test-wait (lambda () (cl-some (lambda (m) (string-match-p "authentication" m))
                                                errors)))
        (should (cl-some (lambda (m) (string-match-p "authentication" m)) errors))
        (should-not (spyglass-connection-live-p connection))
        (should (memq (spyglass-connection-state connection) '(reconnecting connecting))))
    (spyglass-test-stop)))

(ert-deftest spyglass-e2e-garbage-is-dropped ()
  "A stream sending garbage before authenticating is dropped."
  (unwind-protect
      (let* ((server (spyglass-test-start))
             (process (make-network-process :name "garbage" :host "127.0.0.1"
                                            :service (spyglass-server-port server)
                                            :coding 'binary :filter #'ignore)))
        (spyglass-test-settle 0.1)
        (process-send-string process (concat (spyglass-wire--u32 100000000)
                                             (spyglass-wire--u32 0) "xx"))
        (spyglass-test-wait (lambda () (not (process-live-p process))) 5)
        (should-not (process-live-p process))
        (should (null (cl-remove-if-not #'spyglass-connection-live-p (spyglass-connections)))))
    (spyglass-test-stop)))

(ert-deftest spyglass-e2e-disconnect-closes-everything ()
  "Disconnecting closes the mirrors and ends the subscriptions on both sides."
  (spyglass-test-with-connection connection
    (spyglass-e2e--with-buffer "bye"
      (let ((mirror (spyglass-test-mirror connection (current-buffer))))
        (spyglass-disconnect connection)
        (should (eq (spyglass-mirror-state mirror) 'closed))
        (spyglass-test-wait (lambda () (null (spyglass-share-subs (spyglass-share-of (current-buffer))))))
        (should (null (spyglass-share-subs (spyglass-share-of (current-buffer)))))
        (should (eq (spyglass-connection-state connection) 'closed))))))

(ert-deftest spyglass-e2e-registration-file ()
  "The registration file names the port and token, readable by its owner only."
  (unwind-protect
      (let* ((server (spyglass-test-start))
             (text (with-temp-buffer
                     (insert-file-contents spyglass-test--server-file)
                     (buffer-string)))
             (reg (spyglass-parse-registration text)))
        (should (equal (car reg) (spyglass-server-port server)))
        (should (equal (cdr reg) (spyglass-server-token server)))
        (unless (memq system-type '(windows-nt ms-dos))
          (should (= (file-modes spyglass-test--server-file) #o600)))
        (should (equal (process-contact (spyglass-server-process server) :host)
                       "127.0.0.1")))
    (spyglass-test-stop)))

(defun spyglass-e2e--terminal (kind)
  "Check that a terminal of KIND mirrors exactly while a program writes to it."
  (require 'spyglass-bench-common)
  (spyglass-bench-read-environment)
  (unless (spyglass-bench-terminal-available-p kind)
    (ert-skip (format "No %s here" kind)))
  (spyglass-test-with-connection connection
    (let ((buffer (spyglass-bench-make-terminal kind 20000)))
      (unwind-protect
          (let ((mirror (spyglass-test-mirror connection buffer)))
            (spyglass-test-wait (lambda () (not (get-buffer-process buffer))) 60)
            (spyglass-test-converge buffer mirror 30)
            (should (with-current-buffer mirror
                      (save-excursion (goto-char 1) (search-forward "done" nil t))))
            ;; The adapter's hints spare comparing the whole terminal.
            (should (< (or (plist-get (spyglass-share-stats (spyglass-share-of buffer))
                                      :full-scans)
                           0)
                       3)))
        (let ((kill-buffer-query-functions nil))
          (when (get-buffer-process buffer) (delete-process (get-buffer-process buffer)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest spyglass-e2e-eat ()
  "An Eat terminal mirrors exactly while a program writes to it."
  (spyglass-e2e--terminal "eat"))

(ert-deftest spyglass-e2e-vterm ()
  "A vterm terminal mirrors exactly while a program writes to it."
  (spyglass-e2e--terminal "vterm"))

(ert-deftest spyglass-e2e-ghostel ()
  "A Ghostel terminal mirrors exactly while a program writes to it."
  (spyglass-e2e--terminal "ghostel"))

(provide 'spyglass-e2e-test)
;;; spyglass-e2e-test.el ends here
