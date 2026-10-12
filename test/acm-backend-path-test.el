;;; acm-backend-path-test.el --- The path completion backend  -*- lexical-binding: t; -*-

;;; Commentary:
;; The path backend offers the path candidates lsp-bridge recorded for the buffer
;; and inserts the chosen one relative to the directory before point.  What is
;; checked here is that logic: the option that turns it off, the memoized list,
;; the reset, and the insert that must not leave a second dot in front of a file
;; that already starts with one - which is the case the code has a comment about.
;;
;; One of the tests records a limitation rather than a promise: the expansion
;; reads the character before the prefix to decide about that dot, and at the very
;; beginning of a buffer there is none, which `char-to-string' refuses.  The test
;; says so, so that the day somebody completes a path at point-min they find a
;; failing test rather than an unexplained error.

;;; Code:

(require 'ert)
(require 'acm)
(require 'acm-backend-path)

(ert-deftest acm-backend-path-test-candidates-follow-the-option ()
  "The candidates are offered only while the option is on."
  (with-temp-buffer
    (let ((acm-enable-path t))
      (setq-local acm-backend-path-items '("one" "two"))
      (should (equal (acm-backend-path-candidates "keyword") '("one" "two"))))
    (acm-backend-path-clean)
    (let ((acm-enable-path nil))
      (should-not (acm-backend-path-candidates "keyword")))))

(ert-deftest acm-backend-path-test-candidates-come-from-the-cache ()
  "A candidate list already computed for the buffer is handed back as it is."
  (with-temp-buffer
    (setq-local acm-backend-path-items '("new"))
    (setq-local acm-backend-path-cache-candiates '("cached"))
    (should (equal (acm-backend-path-candidates "keyword") '("cached")))))

(ert-deftest acm-backend-path-test-clean-forgets-everything ()
  "Cleaning drops the candidates and the cache of the buffer."
  (with-temp-buffer
    (setq-local acm-backend-path-items '("one"))
    (setq-local acm-backend-path-cache-candiates '("cached"))
    (acm-backend-path-clean)
    (should-not acm-backend-path-items)
    (should-not acm-backend-path-cache-candiates)))

(ert-deftest acm-backend-path-test-expand-inserts-directory-and-name ()
  "The chosen file replaces the typed prefix, directory included."
  (with-temp-buffer
    (insert " src/fo")
    (cl-letf (((symbol-function 'acm-get-input-prefix) (lambda () "src/fo")))
      (acm-backend-path-candidate-expand (list :label "foo.py") 2 nil))
    (should (equal (buffer-string) " src/foo.py"))))

(ert-deftest acm-backend-path-test-expand-does-not-double-the-dot ()
  "A dot outside the prefix is not repeated in front of the file name."
  (with-temp-buffer
    (insert ".gitig")
    (cl-letf (((symbol-function 'acm-get-input-prefix) (lambda () "gitig")))
      (acm-backend-path-candidate-expand (list :label ".gitignore") 2 nil))
    (should (equal (buffer-string) ".gitignore"))))

(ert-deftest acm-backend-path-test-expand-at-point-min-signals ()
  "A prefix at the very beginning of a buffer has no character to look at.
This is a limitation of the backend today, not a promise: the prefix that starts
at `point-min' makes the dot check read a character that is not there.  The test
is here so the day it is fixed, the test is what says so."
  (with-temp-buffer
    (insert "fo")
    (cl-letf (((symbol-function 'acm-get-input-prefix) (lambda () "fo")))
      (should-error (acm-backend-path-candidate-expand (list :label "foo.py") 1 nil)
                    :type 'wrong-type-argument))))

(provide 'acm-backend-path-test)
;;; acm-backend-path-test.el ends here
