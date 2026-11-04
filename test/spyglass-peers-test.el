;;; spyglass-peers-test.el --- Tests of shares for some peers, and of reconnection delays  -*- lexical-binding: t; -*-

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

;; What an application sharing a buffer with some of its peers needs: a
;; share visible to the peers a predicate accepts, input taken from some
;; of them, a size policy of its own, a mirror opened anew in the buffer
;; of one that closed, under a title; and the delays of a connection
;; backing off a peer that stays unreachable.  This Emacs connects to
;; itself twice through 127.0.0.1, so that its server accepts two
;; connections, one per viewer.

;;; Code:

(require 'spyglass-test-helpers)

(defmacro spyglass-peers-test--with-two (vars &rest body)
  "Run BODY with VARS, (A B ACCEPTED-A ACCEPTED-B), bound to two connections.
A and B are this Emacs's connections to its own server, ACCEPTED-A and
ACCEPTED-B their ends the server accepted, which shares see."
  (declare (indent 1) (debug t))
  (pcase-let ((`(,a ,b ,accepted-a ,accepted-b) vars))
    `(spyglass-test-with-connection ,a
       (let* ((,accepted-a (spyglass-test-other-end ,a))
              (,b (spyglass-test-connect "second"))
              (,accepted-b (cl-find-if (lambda (c) (and (spyglass-connection-server c)
                                                        (not (eq c ,accepted-a))
                                                        (spyglass-connection-live-p c)))
                                       (spyglass-connections))))
         (should ,accepted-a)
         (should ,accepted-b)
         ;; One instance on both: its subscriptions' keys must differ.
         (setf (spyglass-connection-next-sub ,b) 1000)
         ,@body))))

(defun spyglass-peers-test--ids (connection)
  "Return the identifiers of the shares CONNECTION's peer announced."
  (mapcar (lambda (share) (plist-get share :id)) (spyglass-connection-shares connection)))

(ert-deftest spyglass-peers-visible-to-some ()
  "A share for some peers is announced to them alone, and only they open it."
  (spyglass-peers-test--with-two (a b accepted-a _accepted-b)
    (with-temp-buffer
      (insert "for the first viewer only")
      (rename-buffer "only-a" t)
      (spyglass-share (current-buffer) :id "only-a" :peers (lambda (c) (eq c accepted-a)))
      (spyglass-test-wait (lambda () (member "only-a" (spyglass-peers-test--ids a))))
      (spyglass-test-settle 0.1)
      (should (member "only-a" (spyglass-peers-test--ids a)))
      (should-not (member "only-a" (spyglass-peers-test--ids b)))
      ;; Asked by name anyway, it does not exist for the other.
      (let ((refused (spyglass-open b "only-a")))
        (spyglass-test-wait (lambda () (eq (spyglass-mirror-state refused) 'closed)))
        (should (equal (spyglass-mirror-closed-reason refused) "no such share"))
        (kill-buffer refused))
      (let ((mirror (spyglass-open a "only-a")))
        (spyglass-test-converge (current-buffer) mirror)
        ;; Shared anew for nobody: the mirror closes.
        (spyglass-share (current-buffer) :peers #'ignore)
        (spyglass-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'closed)))
        (should (equal (spyglass-mirror-closed-reason mirror) "unshared"))
        (spyglass-test-wait (lambda () (null (spyglass-peers-test--ids a))))
        (should-not (spyglass-peers-test--ids a))
        (kill-buffer mirror)))))

(ert-deftest spyglass-peers-input-from-some ()
  "A share whose read-only is a function takes input from some viewers, telling each."
  (skip-unless (executable-find "cat"))
  (spyglass-peers-test--with-two (a b _accepted-a accepted-b)
    (with-temp-buffer
      (insert "ab")
      (rename-buffer "some-input" t)
      (let ((errors nil))
        (let ((spyglass-error-functions (list (lambda (_c message) (push message errors)))))
          (spyglass-share (current-buffer) :id "some-input" :read-only (lambda (c) (eq c accepted-b)))
          (let ((writer (spyglass-open a "some-input"))
                (reader (spyglass-open b "some-input")))
            (dolist (mirror (list writer reader))
              (spyglass-test-wait (lambda () (plist-get (spyglass--mirror-meta
                                                         (buffer-local-value 'spyglass--mirror mirror))
                                                        :in))))
            (should (eq (plist-get (spyglass--mirror-meta (buffer-local-value 'spyglass--mirror writer)) :in) t))
            (should (eq (plist-get (spyglass--mirror-meta (buffer-local-value 'spyglass--mirror reader)) :in)
                        :false))
            (should (string-match-p "read-only"
                                    (cadr (should-error (spyglass-mirror-send-string "x" reader)
                                                        :type 'user-error))))
            ;; Sent anyway, as a viewer ignoring what it was told: refused.
            (spyglass--send b (list :t "in" :sub (spyglass--mirror-id (buffer-local-value 'spyglass--mirror reader))
                                    :k "s" :v "x"))
            (spyglass-test-wait (lambda () errors))
            (should (string-match-p "read-only" (car errors)))
            (should (equal (buffer-string) "ab"))
            (goto-char (point-max))
            (spyglass-mirror-send-string "c" writer)
            (spyglass-test-wait (lambda () (equal (buffer-string) "abc")))
            (should (equal (buffer-string) "abc"))
            ;; The list of shares tells each too.
            (spyglass-test-wait (lambda () (spyglass-connection-shares b)))
            (should (eq (plist-get (car (spyglass-connection-shares a)) :input) t))
            (should (eq (plist-get (car (spyglass-connection-shares b)) :input) nil))
            (kill-buffer writer)
            (kill-buffer reader)))))))

(ert-deftest spyglass-peers-size-policy-of-a-share ()
  "A share's own size policy wins over `spyglass-size-policy'."
  (let* ((share (spyglass--make-share :id "p" :buffer (current-buffer)))
         (connection (spyglass--make-connection :state 'open))
         (sub (spyglass--make-sub :connection-ref (spyglass--ref connection)
                                  :share-ref (spyglass--ref share) :size '(80 . 40) :active 20.0)))
    (cl-letf (((symbol-function 'spyglass--sub-live-p) (lambda (_) t)))
      (setf (spyglass-share-subs share) (list sub)
            (spyglass-share-owner-active share) 10.0)
      (let ((spyglass-size-policy 'latest))
        (should (equal (spyglass--choose-size share '(120 . 50)) '(80 . 40)))
        (puthash share (list :size-policy 'owner) spyglass--share-options)
        (should (equal (spyglass--choose-size share '(120 . 50)) '(120 . 50)))
        ;; The owner showing it nowhere, the viewer's size.
        (should (equal (spyglass--choose-size share nil) '(80 . 40)))))
    (should-error (spyglass-share (current-buffer) :size-policy 'biggest))))

(ert-deftest spyglass-peers-open-again-in-its-buffer ()
  "A mirror that closed opens anew in its buffer, under a title, its windows kept."
  (spyglass-test-with-connection connection
    (with-temp-buffer
      (insert "first\n")
      (rename-buffer "again" t)
      (spyglass-share (current-buffer) :id "again")
      (let* ((mirror (spyglass-open connection "again" :title "My terminal")))
        (spyglass-test-converge (current-buffer) mirror)
        (should (string-match-p "\\` My terminal@" (spyglass--mirror-state-line
                                                     (buffer-local-value 'spyglass--mirror mirror))))
        (should-error (spyglass-open connection "other" :buffer mirror))
        (spyglass-close mirror)
        (insert "second\n")
        (save-window-excursion
          (set-window-buffer (selected-window) mirror)
          (should (eq (spyglass-open connection "again" :buffer mirror :title "Again") mirror))
          (spyglass-test-converge (current-buffer) mirror)
          (should (eq (window-buffer (selected-window)) mirror))
          (should (eq (spyglass-mirror-state mirror) 'live))
          (should (string-match-p "\\` Again@" (spyglass--mirror-state-line
                                                (buffer-local-value 'spyglass--mirror mirror)))))
        (kill-buffer mirror)))))

(ert-deftest spyglass-peers-reconnect-delays ()
  "A connection backs off with delays of its own, starts over when told, and says when it tries next."
  (let* ((attempts nil)
         (spyglass-error-functions nil)
         (connection (spyglass-connect (lambda (done)
                                         (push (float-time) attempts)
                                         (funcall done nil "down"))
                                       :name "unreachable")))
    (unwind-protect
        (progn
          (should-error (spyglass-connection-set-reconnect-delays connection '(1 -2)))
          (should-error (spyglass-connection-set-reconnect-delays connection '(1 0)))
          (should-error (spyglass-connection-set-reconnect-delays connection 5))
          (spyglass-connection-set-reconnect-delays connection '(0.05 0.1 0.4))
          (spyglass-test-wait (lambda () (>= (length attempts) 4)) 5)
          (should (>= (length attempts) 4))
          ;; The last delay repeats: the wait before the next is about 0.4 s.
          (spyglass-test-wait (lambda () (spyglass-connection-next-attempt connection)) 1)
          (let ((next (spyglass-connection-next-attempt connection)))
            (should (and next (<= next 0.4))))
          (should (>= (spyglass-connection-attempts connection) 3))
          ;; Told to start over: an attempt at once, then the first delays.
          (let ((before (length attempts)))
            (should (spyglass-reconnect connection t))
            (should (= (length attempts) (1+ before)))
            (should (= (spyglass-connection-attempts connection) 1))
            (should (<= (spyglass-connection-next-attempt connection) 0.1)))
          ;; Back to the options' delays.
          (spyglass-connection-set-reconnect-delays connection nil)
          (should (equal (spyglass--connection-delays connection) spyglass-reconnect-delays)))
      (spyglass-disconnect connection))
    (should-not (spyglass-connection-next-attempt connection))))

(provide 'spyglass-peers-test)
;;; spyglass-peers-test.el ends here
