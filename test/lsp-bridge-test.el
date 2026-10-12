;;; lsp-bridge-test.el --- In-Emacs smoke test for lsp-bridge

;;; Commentary:

;;; Code:

(defun lsp-bridge-start-test ()
  "Start the LSP bridge test."
  (message "Starting LSP bridge test...")
  (setq lsp-bridge-log-level 'debug)
  (setq lsp-bridge-epc-debug 't)
  (lsp-bridge-start-process)
  (while (not lsp-bridge-epc-process)
    (message "waiting for remote process to start...")
    (with-current-buffer lsp-bridge-name
      (message (buffer-string)))
    (sleep-for 1))
  (lsp-bridge-call-async "start_test")
  (while 't (sit-for 1)))

(provide 'lsp-bridge-test)
;; The lookups asked below are the ones of the package itself.
(require 'lsp-bridge)

(ert-deftest lsp-bridge-test-paths-compare-as-the-system-does ()
  "Two paths are equal as strings, case insensitively only on Windows."
  (should (lsp-bridge-path-equal "/tmp/a.py" "/tmp/a.py"))
  (should-not (lsp-bridge-path-equal "/tmp/a.py" "/tmp/b.py"))
  ;; This system is not one of the case insensitive ones.
  (should-not (lsp-bridge-path-equal "/tmp/A.py" "/tmp/a.py"))
  (let ((system-type 'windows-nt))
    (should (lsp-bridge-path-equal "/tmp/A.py" "/tmp/a.py")))
  (let ((system-type 'cygwin))
    (should (lsp-bridge-path-equal "/tmp/A.py" "/tmp/a.py"))))

(ert-deftest lsp-bridge-test-the-mode-a-file-path-picks ()
  "The mode of a file path is the one `auto-mode-alist' gives it."
  (let ((auto-mode-alist (cons (cons "\\.lspbridgetest\\'" 'lsp-bridge-test-mode) auto-mode-alist)))
    (should (eq (lsp-bridge-get-mode-name-from-file-path "a.lspbridgetest")
                'lsp-bridge-test-mode))
    (should-not (lsp-bridge-get-mode-name-from-file-path "a.unknown-extension-at-all"))))

(ert-deftest lsp-bridge-test-a-literal-extension-entry ()
  "An entry whose key is a string is matched literally."
  (let ((table (list (cons "py" "the-python-server") (cons "js" "the-js-server"))))
    (should (equal (lsp-bridge-find-langserver-info-by-extension "py" table)
                   '("py" . "the-python-server")))
    (should-not (lsp-bridge-find-langserver-info-by-extension "rb" table))))

(ert-deftest lsp-bridge-test-an-extension-entry-that-is-a-list ()
  "An entry whose key is a list holds for every extension in it."
  (let ((table (list (cons '("ts" "tsx") "the-ts-server"))))
    (should (equal (lsp-bridge-find-langserver-info-by-extension "tsx" table)
                   '(("ts" "tsx") . "the-ts-server")))
    (should-not (lsp-bridge-find-langserver-info-by-extension "js" table))))

(ert-deftest lsp-bridge-test-the-full-extension-is-tried-before-the-plain-one ()
  "A file such as `a.component.ts' finds the entry for its full extension first."
  (let ((table (list (cons "ts" "the-plain-ts-server")
                     (cons "component.ts" "the-angular-server"))))
    (should (equal (lsp-bridge-get-lang-server-by-extension "a.component.ts" table)
                   "the-angular-server"))
    ;; A file with no such entry falls back to its plain extension.
    (should (equal (lsp-bridge-get-lang-server-by-extension "a.other.ts" table)
                   "the-plain-ts-server"))
    ;; And a file with no extension at all has no server to find.
    (should-not (lsp-bridge-get-lang-server-by-extension "no-extension" table))))

(ert-deftest lsp-bridge-test-the-server-of-a-mode ()
  "A mode entry is matched either as one symbol or as a list of them."
  (let ((table (list (cons 'python-mode "the-python-server")
                     (cons '(js-mode js-ts-mode) "the-js-server"))))
    (should (equal (lsp-bridge-lang-server-by-mode 'python-mode table)
                   '(python-mode . "the-python-server")))
    (should (equal (lsp-bridge-lang-server-by-mode 'js-ts-mode table)
                   '((js-mode js-ts-mode) . "the-js-server")))
    (should-not (lsp-bridge-lang-server-by-mode 'ruby-mode table))))

;;; lsp-bridge-test.el ends here
