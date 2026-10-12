;;; lsp-bridge-ui-test.el --- What the mode line is told  -*- lexical-binding: t; -*-

;;; Commentary:
;; The mode line of a buffer shows what the language server is doing, which the
;; server reports as a message like `Loading (96%)', and the name of the definition
;; point is in, which is kept in the buffer.  Parsing that message into the text the
;; mode line shows, and keeping and reading back the definition name, need neither
;; a server nor a frame, and are what is checked here.  The rest of the module reads
;; the state of the whole package for the mode line, which is not text.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-ui)

(ert-deftest lsp-bridge-ui-test-progress-with-a-stage-and-a-percent ()
  "A stage and a percentage become the two of them, with no icon here.
The icon is left out on an Emacs without the icon library, which is why the text
starts with the space that would have separated it, and the percentage was dropped
before this fix because the pattern looked for `%%' where the message has one."
  (should (equal (lsp-bridge--format-progress-message "Loading (96%)")
                 " Loading 96%"))
  ;; The rustAnalyzer prefix is not part of the stage name.
  (should (equal (lsp-bridge--format-progress-message "rustAnalyzer/Indexing (12%)")
                 " Indexing 12%")))

(ert-deftest lsp-bridge-ui-test-progress-with-only-a-stage ()
  "A message without a percentage keeps the stage alone."
  (should (equal (lsp-bridge--format-progress-message "Indexing") " Indexing")))

(ert-deftest lsp-bridge-ui-test-progress-without-a-stage ()
  "A message the pattern does not match gives nothing to show."
  (should (equal (lsp-bridge--format-progress-message "(96%)") ""))
  (should (equal (lsp-bridge--format-progress-message "") "")))

(ert-deftest lsp-bridge-ui-test-current-defun-is-kept-per-buffer ()
  "The definition name is stored in the buffer it belongs to."
  (with-temp-buffer
    (should-not (lsp-bridge-symbols--current-defun))
    (lsp-bridge-symbols--record-current-defun "my-function")
    (should (equal (lsp-bridge-symbols--current-defun) "my-function")))
  (with-temp-buffer
    ;; A fresh buffer has none of its own, which is the point of it being local.
    (should-not (lsp-bridge-symbols--current-defun))))

(provide 'lsp-bridge-ui-test)
;;; lsp-bridge-ui-test.el ends here
