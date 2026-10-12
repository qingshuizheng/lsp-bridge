;;; lsp-bridge-completion-test.el --- The completion predicates  -*- lexical-binding: t; -*-

;;; Commentary:
;; Two questions decide whether the menu may appear: is the character before
;; point one that hides it, and is point inside a string.  A backend that answers
;; differently - a chat buffer where a colon starts a keyword, a source that
;; completes a code fence language - answers through the hook lsp-bridge offers
;; for each question.  What is checked here is that those hooks are honoured and
;; that the answers without them are unchanged.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-completion)

(ert-deftest lsp-bridge-completion-test-hide-character-hook-is-honoured ()
  "A function in the hook may keep a hiding character from hiding the menu."
  (with-temp-buffer
    (insert ":")
    (let ((lsp-bridge-completion-hide-character-functions nil))
      (should-not (lsp-bridge--not-match-hide-characters)))
    (let ((lsp-bridge-completion-hide-character-functions (list (lambda () t))))
      (should (lsp-bridge--not-match-hide-characters)))
    (let ((lsp-bridge-completion-hide-character-functions (list (lambda () nil))))
      (should-not (lsp-bridge--not-match-hide-characters)))))

(ert-deftest lsp-bridge-completion-test-in-string-hook-is-honoured ()
  "A function in the hook may allow the menu inside a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"abc\"")
    (goto-char (1- (point-max)))
    (should (acm-in-string-p))
    (let ((lsp-bridge-completion-in-string-functions nil))
      (should-not (lsp-bridge--not-in-string)))
    (let ((lsp-bridge-completion-in-string-functions (list (lambda () t))))
      (should (lsp-bridge--not-in-string)))))

(provide 'lsp-bridge-completion-test)
;;; lsp-bridge-completion-test.el ends here
