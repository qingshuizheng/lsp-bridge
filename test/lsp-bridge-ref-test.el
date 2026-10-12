;;; lsp-bridge-ref-test.el --- The reference buffer  -*- lexical-binding: t; -*-

;;; Commentary:
;; The references of a symbol are listed in a buffer of their own: a line naming
;; the file, then one line per match with its line and column.  Reading that
;; buffer back - which file a match belongs to, where it is, what code it holds -
;; and the small text questions around it need neither a server nor a project,
;; and that is what is checked here.
;;
;; Three parts are left out on purpose, because they cannot be asked without a
;; language server:
;; - the search commands, which `lsp-bridge-ref-search-create' generates one of
;;   per language mode; they call `lsp-bridge-call-file-api' and wait for the
;;   answer of a server;
;; - `lsp-bridge-ref-popup', which is called with the answer;
;; - applying an edit from the ref buffer back to the file that was searched,
;;   which needs the file to be under version control to show the change.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-ref)

(ert-deftest lsp-bridge-ref-test-file-extension ()
  "The extension is what follows the last dot, dots included."
  (should (equal (lsp-bridge-ref-file-extension "/a/b/c.tar.gz") "tar.gz"))
  (should (equal (lsp-bridge-ref-file-extension "/a/b/c.py") "py"))
  (should (equal (lsp-bridge-ref-file-extension "plain") "")))

(ert-deftest lsp-bridge-ref-test-match-line-and-column ()
  "The line and column of a match are read off the line it is on."
  (with-temp-buffer
    (insert "42:7:let x = 1\n")
    (goto-char (point-min))
    (should (equal (lsp-bridge-ref-get-match-line) 42))
    (goto-char (point-min))
    (should (equal (lsp-bridge-ref-get-match-column) 7))))

(ert-deftest lsp-bridge-ref-test-match-file ()
  "The file of a match is the file line above it."
  (with-temp-buffer
    (insert "/tmp/a.py\n10:3:let x = 1\n")
    (goto-char (point-max))
    (should (equal (lsp-bridge-ref-get-match-file) "/tmp/a.py"))))

(ert-deftest lsp-bridge-ref-test-line-content ()
  "The code of a result line is what follows its line:column prefix."
  (with-temp-buffer
    (insert "first\n12:3:let x = 1\n")
    (should (equal (lsp-bridge-ref-get-line-content (current-buffer) 2) "let x = 1"))))

(ert-deftest lsp-bridge-ref-test-current-line-empty-p ()
  "A line of nothing but whitespace is empty."
  (with-temp-buffer
    (insert "   \n")
    (goto-char (point-min))
    (should (lsp-bridge-ref-current-line-empty-p))
    (insert "word")
    (goto-char (point-min))
    (should-not (lsp-bridge-ref-current-line-empty-p))))

(provide 'lsp-bridge-ref-test)
;;; lsp-bridge-ref-test.el ends here
