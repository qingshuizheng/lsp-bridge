;;; lsp-bridge-call-hierarchy-test.el --- The call hierarchy commands  -*- lexical-binding: t; -*-

;;; Commentary:
;; A popup that is driven by the call hierarchy keys but shows something else -
;; the code action menu is one - takes the commands over through
;; `lsp-bridge-call-hierarchy-overrides' in its own buffer, instead of advising
;; the commands.  What is checked here is that a buffer without such an override
;; is untouched, that one with it is routed to the override, and that the
;; commands are not advised by this package at all.

;;; Code:

(require 'ert)
(require 'lsp-bridge-call-hierarchy)

(ert-deftest lsp-bridge-call-hierarchy-test-preview-goes-to-the-override ()
  "A buffer that takes the preview over is the one that runs."
  (with-temp-buffer
    (let (called)
      (setq-local lsp-bridge-call-hierarchy-overrides
                  (list (cons 'lsp-bridge-call-hierarchy-maybe-preview
                              (lambda () (setq called 'override)))))
      (cl-letf (((symbol-function 'lsp-bridge-call-hierarchy-show)
                 (lambda () (setq called 'default))))
        (lsp-bridge-call-hierarchy-maybe-preview))
      (should (eq called 'override)))))

(ert-deftest lsp-bridge-call-hierarchy-test-no-override-keeps-the-default ()
  "With no override the preview does what it always did.
The preview height of 0 is the one case that returns early without asking for
the selected call."
  (with-temp-buffer
    (let ((lsp-bridge-call-hierarchy-preview-height 0)
          called)
      (cl-letf (((symbol-function 'lsp-bridge-call-hierarchy-show)
                 (lambda () (setq called t))))
        (lsp-bridge-call-hierarchy-maybe-preview))
      (should-not called))))

(ert-deftest lsp-bridge-call-hierarchy-test-the-commands-are-not-advised ()
  "The commands keep their own definitions; nothing wraps them."
  (dolist (command '(lsp-bridge-call-hierarchy-maybe-preview
                     lsp-bridge-call-hierarchy-select
                     lsp-bridge-call-hierarchy-quit))
    (should-not (advice-member-p #'ignore command))
    (should-not (advice-member-p 'lsp-bridge-code-action-popup-select command))
    (should-not (advice-member-p 'lsp-bridge-code-action-popup-quit command))
    (should-not (advice-member-p 'lsp-bridge-code-action-popup-maybe-preview command))))

(provide 'lsp-bridge-call-hierarchy-test)
;;; lsp-bridge-call-hierarchy-test.el ends here
