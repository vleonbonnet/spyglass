;;; spyglass-paced-test.el --- Frames written whole on paced streams  -*- lexical-binding: t; -*-

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

;; Streams both of whose ends' writes block, as Emacs's sockets on
;; Windows, are paced: the tests of spyglass-conn-test.el writing
;; frames whole, in order and bounded run here again on paced streams,
;; on any system.  Each is a test of its own, in a file of its own, so
;; that they run in several Emacs at once, and that no Emacs keeps the
;; memory the largest of them and of the tests they repeat leave, past
;; 2 GB together.

;;; Code:

(require 'spyglass-test-helpers)
(require 'spyglass-conn-test)

(defmacro spyglass-paced-test-define (&rest tests)
  "Define a test running each of TESTS, tests of spyglass-conn-test.el, on paced streams."
  (declare (indent 0))
  `(progn
     ,@(mapcar (lambda (test)
                 `(ert-deftest ,(intern (replace-regexp-in-string
                                         "\\`spyglass-conn-" "spyglass-paced-" (symbol-name test)))
                      ()
                    ,(format "Frames are written whole, in order and bounded on paced streams too: `%s'."
                             test)
                    (let ((spyglass--pace-streams t))
                      (funcall (ert-test-body (ert-get-test ',test))))))
               tests)))

(spyglass-paced-test-define
  spyglass-conn-frame-whole-while-filter-answers
  spyglass-conn-frame-whole-while-mirror-acknowledges
  spyglass-conn-frame-whole-while-timer-sends
  spyglass-conn-frame-whole-while-channel-answers
  spyglass-conn-frames-handled-in-order
  spyglass-conn-queue-bounded
  spyglass-conn-inbox-bounded
  spyglass-conn-failed-write-drops-stream
  spyglass-conn-reconnect
  spyglass-conn-mirrors-both-ways-slow-readers)

(provide 'spyglass-paced-test)
;;; spyglass-paced-test.el ends here
