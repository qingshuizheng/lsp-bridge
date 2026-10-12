;;; acm-backend-candidates-test.el --- What the small backends offer  -*- lexical-binding: t; -*-

;;; Commentary:
;; Two backends offer a list of candidates lsp-bridge recorded for the buffer,
;; behind an option and a minimum length for the keyword, and memoize what they
;; computed.  What is checked here is exactly that - the option, the length, the
;; memoized list and the reset - which is what decides whether the menu is offered
;; at all, and needs neither a language server nor the ctags binary.

;;; Code:

(require 'ert)
(require 'acm)
(require 'acm-backend-ctags)
(require 'acm-backend-copilot)
(require 'acm-backend-lsp-workspace-symbol)
(require 'acm-backend-search-file-words)
(require 'acm-backend-jupyter)
(require 'acm-backend-tabby)
(require 'acm-backend-tempel)

;; The cache variables of the backends are built with `intern' from a format
;; string, and the items of the workspace symbol backend are made with
;; `make-hash-table' where they are recorded, so the compiler cannot see any of
;; them and has to be told about them.
(defvar acm-backend-ctags-cache-candiates)
(defvar acm-backend-copilot-cache-candiates)
(defvar acm-backend-lsp-workspace-symbol-cache-candidates)
(defvar acm-backend-lsp-workspace-symbol-items)
(defvar acm-backend-jupyter-cache-candiates)
(defvar acm-backend-tabby-cache-candiates)
(defvar acm-backend-search-file-words-cache-candiates)
;; Tempel's own variables are declared here for the same reason: the test sets
;; them as the special variables the backend reads.
(defvar tempel-template-sources)
(defvar tempel-trigger-prefix)

(ert-deftest acm-backend-ctags-test-candidates-follow-option-and-length ()
  "The ctags candidates are offered only past the option and the length."
  (with-temp-buffer
    (setq-local acm-backend-ctags-items '("one" "two"))
    (let ((acm-enable-ctags t)
          (acm-backend-ctags-candidate-min-length 3))
      (should (equal (acm-backend-ctags-candidates "abc") '("one" "two")))
      (acm-backend-ctags-clean)
      (should-not (acm-backend-ctags-candidates "ab")))
    (acm-backend-ctags-clean)
    (let ((acm-enable-ctags nil)
          (acm-backend-ctags-candidate-min-length 0))
      (should-not (acm-backend-ctags-candidates "abc")))))

(ert-deftest acm-backend-ctags-test-clean-forgets-the-cache ()
  "Cleaning drops the candidates and the memoized list."
  (with-temp-buffer
    (setq-local acm-backend-ctags-items '("one"))
    (setq-local acm-backend-ctags-cache-candiates '("cached"))
    (acm-backend-ctags-clean)
    (should-not acm-backend-ctags-items)
    (should-not acm-backend-ctags-cache-candiates)))

(ert-deftest acm-backend-search-file-words-test-candidates-follow-option-and-length ()
  "The file word candidates are offered only past the option and the length."
  (with-temp-buffer
    (setq-local acm-backend-search-file-words-items '("word"))
    (let ((acm-enable-search-file-words t)
          (acm-backend-search-file-words-candidate-min-length 4))
      (should (equal (acm-backend-search-file-words-candidates "word") '("word")))
      (acm-backend-search-file-words-clean)
      (should-not (acm-backend-search-file-words-candidates "wor")))
    (acm-backend-search-file-words-clean)
    (let ((acm-enable-search-file-words nil)
          (acm-backend-search-file-words-candidate-min-length 0))
      (should-not (acm-backend-search-file-words-candidates "word")))))

(ert-deftest acm-backend-copilot-test-candidates-follow-the-trailing-option ()
  "With the option off the items are offered whole; with it on, trimmed.
The trimming is the part of the Copilot backend that is about the text before
point rather than about the service, which is why it can be asked here."
  (with-temp-buffer
    (insert "let x = ")
    (setq-local acm-backend-copilot-items
                (list (list :displayLabel "let x = y" :label "y")))
    (let ((acm-backend-copilot-show-trailing nil))
      (should (equal (plist-get (car (acm-backend-copilot-candidates "let")) :displayLabel)
                     "let x = y")))
    (acm-backend-copilot-clean)
    (setq-local acm-backend-copilot-items
                (list (list :displayLabel "let x = y" :label "y")))
    (let ((acm-backend-copilot-show-trailing t))
      (should (equal (plist-get (car (acm-backend-copilot-candidates "let")) :displayLabel)
                     "y")))))

(ert-deftest acm-backend-copilot-test-a-candidate-that-does-not-start-with-it ()
  "A candidate that does not start with the text before point is left alone."
  (with-temp-buffer
    (insert "let x = ")
    (let ((acm-backend-copilot-show-trailing t)
          (acm-backend-copilot-items (list (list :displayLabel "something else"))))
      (should (equal (plist-get (car (acm-backend-copilot-candidates-preprocess
                                      acm-backend-copilot-items))
                                :displayLabel)
                     "something else")))))

(ert-deftest acm-backend-copilot-test-clean-forgets-everything ()
  "Cleaning drops the Copilot candidates and their cache."
  (with-temp-buffer
    (setq-local acm-backend-copilot-items '("one"))
    (setq-local acm-backend-copilot-cache-candiates '("cached"))
    (acm-backend-copilot-clean)
    (should-not acm-backend-copilot-items)
    (should-not acm-backend-copilot-cache-candiates)))

(ert-deftest acm-backend-lsp-workspace-symbol-test-candidates-need-the-min-length ()
  "Nothing is offered while the keyword is shorter than the minimum."
  (with-temp-buffer
    (setq-local acm-backend-lsp-workspace-symbol-candidate-min-length 5)
    (should-not (acm-backend-lsp-workspace-symbol-candidates "four")))
  (with-temp-buffer
    (setq-local acm-backend-lsp-workspace-symbol-candidate-min-length 3)
    (should-not (acm-backend-lsp-workspace-symbol-candidates "four"))))

(ert-deftest acm-backend-lsp-workspace-symbol-test-clean-makes-an-empty-table ()
  "Cleaning leaves an empty hash for the Python side to fill in again."
  (with-temp-buffer
    (setq-local acm-backend-lsp-workspace-symbol-items '("stale"))
    (setq-local acm-backend-lsp-workspace-symbol-cache-candidates '("stale"))
    (acm-backend-lsp-workspace-symbol-clean)
    (should (hash-table-p acm-backend-lsp-workspace-symbol-items))
    (should (equal (hash-table-count acm-backend-lsp-workspace-symbol-items) 0))
    (should-not acm-backend-lsp-workspace-symbol-cache-candidates)))

(ert-deftest acm-backend-tempel-test-candidates-need-the-option-and-tempel ()
  "Nothing is offered while the option is off, or while tempel is not loaded.
The snippets themselves come out of tempel, so the two gates that decide whether
it is asked at all are the part of this backend that can be asked here; the
second one only means something on a machine without tempel, which is why it is
skipped when tempel is loaded."
  (let ((acm-enable-tempel nil))
    (should-not (acm-backend-tempel-candidates "any")))
  (unless (featurep 'tempel)
    (let ((acm-enable-tempel t))
      (should-not (acm-backend-tempel-candidates "any")))))

(ert-deftest acm-backend-tempel-test-the-trigger-prefix-when-there-is-one ()
  "A tempel that still has a trigger prefix is honoured, and one that does not.
Tempel dropped `tempel-trigger-prefix' upstream, so the versions that have it are
the older ones; both are asked for what they expect.  The test needs tempel
itself to be loadable, which it is not in every environment."
  (require 'tempel nil t)
  (skip-unless (featurep 'tempel))
  (let ((acm-enable-tempel t)
        (acm-backend-tempel-candidates-number 5))
    ;; `setq' rather than `let': both of these are read by the backend as special
    ;; variables, and a lexical binding of one would not be the one it sees - the
    ;; compiler says so, which is how the first version of this test was found to
    ;; be configuring nothing at all.
    (unwind-protect
        (progn
          (setq tempel-template-sources (list (lambda () '((acm-backend-tempel-test-foo ("x"))))))
          (setq tempel-trigger-prefix "@")
          (should (equal (mapcar (lambda (candidate) (plist-get candidate :label))
                                 (acm-backend-tempel-candidates "@acm"))
                         '("acm-backend-tempel-test-foo")))
          (should-not (acm-backend-tempel-candidates "acm"))
          (setq tempel-trigger-prefix nil)
          (should (equal (mapcar (lambda (candidate) (plist-get candidate :label))
                                 (acm-backend-tempel-candidates "acm"))
                         '("acm-backend-tempel-test-foo"))))
      (setq tempel-trigger-prefix nil)
      (setq tempel-template-sources nil))))

(ert-deftest acm-backend-tabby-test-find-max-common ()
  "Two strings are joined on their longest overlap, and only when they overlap."
  (should (equal (acm-backend-tabby-find-max-common "abc" "cde") "abcde"))
  (should (equal (acm-backend-tabby-find-max-common "abc" "abc") "abc"))
  (should (equal (acm-backend-tabby-find-max-common "foo" "oo") "foo"))
  (should (equal (acm-backend-tabby-find-max-common "abc" "cd") "abcd"))
  (should-not (acm-backend-tabby-find-max-common "abc" "xyz")))

(ert-deftest acm-backend-tabby-test-candidates-and-clean ()
  "The candidates are the recorded items, sorted, and cleaned away."
  (with-temp-buffer
    (setq-local acm-backend-tabby-items (list (list :label "foo") (list :label "fo")))
    (should (equal (mapcar (lambda (candidate) (plist-get candidate :label))
                           (acm-backend-tabby-candidates "fo"))
                   '("fo" "foo")))
    (acm-backend-tabby-clean)
    (should-not acm-backend-tabby-items)
    (should-not acm-backend-tabby-cache-candiates)))

(ert-deftest acm-backend-jupyter-test-candidates-and-clean ()
  "The candidates are the recorded items, and cleaned away."
  (with-temp-buffer
    (setq-local acm-backend-jupyter-items (list (list :label "foo")))
    (should (equal (mapcar (lambda (candidate) (plist-get candidate :label))
                           (acm-backend-jupyter-candidates "fo"))
                   '("foo")))
    (acm-backend-jupyter-clean)
    (should-not acm-backend-jupyter-items)
    (should-not acm-backend-jupyter-cache-candiates)))

(provide 'acm-backend-candidates-test)
;;; acm-backend-candidates-test.el ends here
