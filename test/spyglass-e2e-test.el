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

(ert-deftest spyglass-e2e-random-bytes ()
  "Random bytes from a process, raw bytes among them, arrive exactly."
  (skip-unless (and (file-readable-p "/dev/urandom") (executable-find "head")))
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
