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

(ert-deftest lsp-bridge-ref-test-a-line-with-a-row-and-column-prefix ()
  "The prefix of a result line is found, and its end is what is returned."
  (with-temp-buffer
    (insert "12:3: some code here\n")
    (goto-char (point-min))
    (should (equal (lsp-bridge-ref-get-row-column-position) 6))))

(ert-deftest lsp-bridge-ref-test-a-line-without-a-row-and-column-prefix ()
  "A line that does not begin with one has no such position."
  (with-temp-buffer
    (insert "some code here\n")
    (goto-char (point-min))
    (should-not (lsp-bridge-ref-get-row-column-position)))
  (with-temp-buffer
    (insert "/tmp/a-file.py\n")
    (goto-char (point-min))
    (should-not (lsp-bridge-ref-get-row-column-position))))

(ert-deftest lsp-bridge-ref-test-counting-the-hits ()
  "The hits counted are the regions carrying the match face."
  (with-temp-buffer
    (insert "one match, another match, and a plain word\n")
    (should (equal (lsp-bridge-ref-stat-hits) 0))
    (add-text-properties 1 4 '(face lsp-bridge-ref-font-lock-match))
    (add-text-properties 16 19 '(face lsp-bridge-ref-font-lock-match))
    (should (equal (lsp-bridge-ref-stat-hits) 2))))

(ert-deftest lsp-bridge-ref-test-finding-the-buffer-of-a-file ()
  "The buffer visiting a file is found by its name, and nothing else is."
  (let ((file (make-temp-file "lsp-bridge-ref-test" nil ".py")))
    (unwind-protect
        (let ((buffer (find-file-noselect file)))
          (should (eq (lsp-bridge-ref-get-match-buffer file) buffer))
          ;; A path with no buffer of its own is nothing.
          (should-not (lsp-bridge-ref-get-match-buffer "/tmp/not-open-at-all")))
      (let ((buffer (get-file-buffer file)))
        (when buffer (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest lsp-bridge-ref-test-an-org-file-is-told-by-its-extension ()
  "Only a file whose extension is org is an org file."
  (should (lsp-bridge-ref-is-org-file "notes.org"))
  (should-not (lsp-bridge-ref-is-org-file "notes.py"))
  (should-not (lsp-bridge-ref-is-org-file "notes.org.txt")))

(ert-deftest lsp-bridge-ref-test-deleting-the-code-of-a-result-line ()
  "The code after the line and column of a result is what is killed."
  (with-temp-buffer
    (insert "12:3: some code here\n")
    (goto-char (point-min))
    (lsp-bridge-ref-delete-line)
    (should (equal (buffer-string) "12:3:\n"))))

(ert-deftest lsp-bridge-ref-test-deleting-a-line-that-is-not-a-result ()
  "A line with no line and column is left alone."
  (with-temp-buffer
    (insert "/tmp/a-file.py\n")
    (goto-char (point-min))
    (lsp-bridge-ref-delete-line)
    (should (equal (buffer-string) "/tmp/a-file.py\n"))))

(ert-deftest lsp-bridge-ref-test-deleting-the-code-of-every-result-line ()
  "Every result line of the reference buffer loses its code."
  (let ((lsp-bridge-ref-buffer (get-buffer-create "*lsp-bridge-ref-test*")))
    (unwind-protect
        (progn
          (with-current-buffer lsp-bridge-ref-buffer
            (erase-buffer)
            (insert "12:3: some code here\n")
            (insert "/tmp/a-file.py\n")
            (insert "4:0: other code\n"))
          (lsp-bridge-ref-delete-all-lines)
          (with-current-buffer lsp-bridge-ref-buffer
            (should (equal (buffer-string)
                           "12:3:\n/tmp/a-file.py\n4:0:\n"))))
      (kill-buffer lsp-bridge-ref-buffer))))

(ert-deftest lsp-bridge-ref-test-reading-the-code-of-a-line ()
  "The code of a result line is what stands after its line and column."
  (let ((buffer (get-buffer-create "*lsp-bridge-ref-line-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "12:3: some code here\n")
            (insert "4:0: other code\n"))
          ;; The result begins right after the line and column prefix, so it keeps
          ;; the space that stands between the prefix and the code.
          (should (equal (lsp-bridge-ref-get-line-content buffer 1) " some code here"))
          (should (equal (lsp-bridge-ref-get-line-content buffer 2) " other code")))
      (kill-buffer buffer))))

(provide 'lsp-bridge-ref-test)
;;; lsp-bridge-ref-test.el ends here
