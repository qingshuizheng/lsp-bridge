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
;; The icons a call is shown with come from this one.
(require 'acm-icon)

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

(ert-deftest lsp-bridge-call-hierarchy-test-padding-a-string ()
  "A string shorter than the width is padded with spaces."
  (should (equal (lsp-bridge-call-hierarchy-format-call-string "abc" 6) "abc   "))
  (should (equal (lsp-bridge-call-hierarchy-format-call-string "" 3) "   "))
  (should (equal (lsp-bridge-call-hierarchy-format-call-string "abc" 3) "abc")))

(ert-deftest lsp-bridge-call-hierarchy-test-truncating-a-string ()
  "A string longer than the width is cut short and marked with dots."
  (should (equal (lsp-bridge-call-hierarchy-format-call-string "abcdefgh" 6) "abc..."))
  ;; The dots are part of the width, so the result is never wider than asked.
  (should (equal (length (lsp-bridge-call-hierarchy-format-call-string "a long name" 8)) 8)))

(ert-deftest lsp-bridge-call-hierarchy-test-the-display-of-a-call ()
  "A call is shown as its name and path, each padded to its own width."
  (let ((lsp-bridge-call-hierarchy-name-width 6)
        (lsp-bridge-call-hierarchy-path-width 4))
    (let ((line (substring-no-properties
                 (lsp-bridge-call-hierarchy-format-call
                  (list :name "callMe" :rel_path "a/b" :icon nil)))))
      ;; The name and the path are there, each padded to its width, and the line
      ;; ends with a newline; where an icon would be there is something too - an
      ;; unknown icon falls back to a blank of its own, which is not worth pinning
      ;; to a number of characters.
      (should (string-suffix-p "callMe a/b \n" line))
      (should (> (length line) (length "callMe a/b \n"))))))

(ert-deftest lsp-bridge-call-hierarchy-test-an-override-is-this-buffers ()
  "A command is taken over by this buffer, and by no other buffer."
  (let ((taker (lambda () "taken over")))
    (with-temp-buffer
      (setq-local lsp-bridge-call-hierarchy-overrides (list (cons 'some-command taker)))
      (should (eq (lsp-bridge-call-hierarchy--override 'some-command) taker))
      (should-not (lsp-bridge-call-hierarchy--override 'another-command)))
    ;; The alist is buffer-local, which is what keeps a popup of one buffer from
    ;; taking over the keys of another.
    (with-temp-buffer
      (should-not (lsp-bridge-call-hierarchy--override 'some-command)))))

(provide 'lsp-bridge-call-hierarchy-test)
;;; lsp-bridge-call-hierarchy-test.el ends here
