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
(require 'acm-backend-search-file-words)

;; The cache variables of the backends are built with `intern' from a format
;; string, so the compiler cannot see them and has to be told about them.
(defvar acm-backend-ctags-cache-candiates)
(defvar acm-backend-search-file-words-cache-candiates)

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

(provide 'acm-backend-candidates-test)
;;; acm-backend-candidates-test.el ends here
