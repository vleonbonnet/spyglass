;;; batch-tests.el --- Run the tests loaded, or a shard of them, in batch  -*- lexical-binding: t; -*-

;;; Commentary:

;; The Makefile's test targets load this after the test files and call
;; `batch-tests-run', as they called `ert-run-tests-batch-and-exit'.
;;
;; Trampolines, which native compilation makes for each primitive a
;; test redefines, go to the cache BATCH_TESTS_ELN names, kept between
;; runs and shared by the Emacs of a run: ERT's own function sends them
;; to a new directory every time, so that each Emacs compiled them
;; again, in another Emacs, about half a second each.  Without it, a
;; temporary directory removed after, as ERT does.
;;
;; SELECTOR, a regexp, runs only the tests it matches; BATCH_TESTS_FILE
;; only those that file defines, not those of other test files it
;; requires; and BATCH_TESTS_SHARD, I/N, only the I-th of N shares of
;; those, I from 1: the Makefile runs a file's tests in several Emacs at
;; once so.  The shares take about as long as one another as the times
;; of an earlier run tell, read from BATCH_TESTS_TIMES, the same file
;; for every Emacs of a run: lines of a test's name and its seconds.
;; Without it, every N-th test in ERT's order.  BATCH_TESTS_TIMES_OUT
;; names a file to write the times of this run to, as such lines.

;;; Code:

(require 'ert)

(defvar batch-tests-file nil
  "The test file whose tests run, or nil for every test loaded.")

(defun batch-tests-in-file-p (test)
  "Return non-nil when TEST was defined by loading `batch-tests-file'.
That file's source or byte code."
  (when-let* ((defined (ert-test-file-name test)))
    (equal (file-name-sans-extension (file-truename defined))
           (file-name-sans-extension (file-truename batch-tests-file)))))

(defun batch-tests-times (file)
  "Return the times FILE tells, a hash table of test names to seconds.
Return nil when FILE is nil or missing."
  (when (and file (file-readable-p file))
    (let ((times (make-hash-table :test #'eq)))
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "^\\(\\S-+\\) \\([0-9.]+\\)$" nil t)
          (puthash (intern (match-string 1)) (string-to-number (match-string 2)) times)))
      (and (> (hash-table-count times) 0) times))))

(defun batch-tests-share (names index count times)
  "Return the INDEX-th of COUNT shares of NAMES, test names, INDEX from 0.
With TIMES, a hash table of names to seconds, the shares take about as
long as one another: the longest test first goes to the share with the
least time, a test without a time counting the median.  Else every
COUNT-th name.  Each Emacs of a run computes the same shares."
  (if (not times)
      (seq-filter #'identity
                  (seq-map-indexed (lambda (name position) (and (= (% position count) index) name))
                                   names))
    (let* ((known (sort (delq nil (mapcar (lambda (name) (gethash name times)) names)) #'<))
           (median (if known (nth (/ (length known) 2) known) 0))
           (loads (make-vector count 0))
           (weighted (sort (mapcar (lambda (name) (cons name (gethash name times median))) names)
                           (lambda (a b)
                             (or (> (cdr a) (cdr b))
                                 (and (= (cdr a) (cdr b))
                                      (string< (car a) (car b)))))))
           (share nil))
      (dolist (entry weighted)
        (let ((least 0))
          (dotimes (i count)
            (when (< (aref loads i) (aref loads least))
              (setq least i)))
          (aset loads least (+ (aref loads least) (cdr entry)))
          (when (= least index)
            (push (car entry) share))))
      (sort share #'string<))))

(defun batch-tests-selector ()
  "Return the selector of the tests to run.
SELECTOR, a regexp, selects some tests; BATCH_TESTS_FILE those a file
defined, not those of the test files it requires; BATCH_TESTS_SHARD a
share of these."
  (let* ((regexp (getenv "SELECTOR"))
         (file (getenv "BATCH_TESTS_FILE"))
         (selector (if (member regexp '(nil "")) t regexp))
         (selector (if (member file '(nil ""))
                       selector
                     (setq batch-tests-file file)
                     `(and ,selector (satisfies batch-tests-in-file-p))))
         (shard (getenv "BATCH_TESTS_SHARD")))
    (if (not (and shard (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)\\'" shard)))
        selector
      (let ((index (1- (string-to-number (match-string 1 shard))))
            (count (string-to-number (match-string 2 shard))))
        (cons 'member
              (batch-tests-share (mapcar #'ert-test-name (ert-select-tests selector t))
                                 index count
                                 (batch-tests-times (getenv "BATCH_TESTS_TIMES"))))))))

(defun batch-tests-write-times (stats file)
  "Write to FILE the time each test of STATS, the results of a run, took."
  (with-temp-file file
    (seq-mapn (lambda (test result)
                (when result
                  (insert (format "%s %.3f\n" (ert-test-name test)
                                  (ert-test-result-duration result)))))
              (ert--stats-tests stats)
              (ert--stats-test-results stats))))

(defun batch-tests-run ()
  "Run the tests `batch-tests-selector' selects, and exit.
The exit status is 0 when every result was as expected, 1 when some
was not, and 2 when running the tests failed."
  (unless noninteractive
    (user-error "This function is only for use in batch mode"))
  (let* ((cache (getenv "BATCH_TESTS_ELN"))
         (eln (and (featurep 'native-compile)
                   (if (member cache '(nil ""))
                       (make-temp-file "test-nativecomp-cache-" t)
                     (make-directory cache t)
                     cache)))
         (temporary (and eln (not (equal eln cache))))
         (times (getenv "BATCH_TESTS_TIMES_OUT")))
    (when eln
      (startup-redirect-eln-cache eln))
    ;; As ERT: better crash loudly than recover from undefined behavior.
    (setq attempt-stack-overflow-recovery nil
          attempt-orderly-shutdown-on-fatal-signal nil)
    (unwind-protect
        (let ((stats (ert-run-tests-batch (batch-tests-selector))))
          (unless (member times '(nil ""))
            (batch-tests-write-times stats times))
          (when temporary
            (ignore-errors (delete-directory eln t)))
          (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
      (unwind-protect
          (progn
            (message "Error running tests")
            (backtrace))
        (when temporary
          (ignore-errors (delete-directory eln t)))
        (kill-emacs 2)))))

(provide 'batch-tests)
;;; batch-tests.el ends here
