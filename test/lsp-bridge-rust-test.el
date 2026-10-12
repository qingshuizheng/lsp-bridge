;;; lsp-bridge-rust-test.el --- The rust-analyzer extras  -*- lexical-binding: t; -*-

;;; Commentary:
;; Two of the three things in this module are one line each, asking the server to
;; expand the macro at point or to reload the workspace, and what they ask for is
;; worth its own test because it is the whole of them.  The third shows the
;; expansion the server sent in a buffer of its own, put in the Rust mode - which
;; is what needs the tree-sitter mode of Rust to be there, so it is stood in for
;; here, as the server is.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-rust)

(defun lsp-bridge-rust-test--calls (body)
  "Return the (METHOD . ARGS) pairs BODY asks the file api for."
  (let (calls)
    (cl-letf (((symbol-function 'lsp-bridge-call-file-api)
               (lambda (method &rest args) (push (cons method args) calls)))
              ((symbol-function 'lsp-bridge--position) (lambda () '(:line 1 :character 2))))
      (funcall body))
    (nreverse calls)))

(ert-deftest lsp-bridge-rust-test-the-two-commands ()
  "Each command asks the server for the one thing it is named after."
  (should (equal (lsp-bridge-rust-test--calls #'lsp-bridge-rust-expand-macro)
                 '(("rust_expand_macro" (:line 1 :character 2)))))
  (should (equal (lsp-bridge-rust-test--calls #'lsp-bridge-rust-reload-workspace)
                 '(("rust_reload_workspace")))))

(ert-deftest lsp-bridge-rust-test-showing-an-expansion ()
  "The expansion is shown in a buffer named after the macro, in the Rust mode."
  (let (modes)
    (cl-letf (((symbol-function 'rust-ts-mode)
               ;; The mode is called from inside the buffer that is being shown.
               (lambda () (push (buffer-name) modes)))
              ((symbol-function 'switch-to-buffer-other-window) (lambda (buffer) buffer)))
      (lsp-bridge-rust-expand-macro--update "my_macro" "(expanded text)"))
    (with-current-buffer "*rust macro expansion my_macro*"
      (should (equal (buffer-string) "(expanded text)")))
    (should (equal modes (list "*rust macro expansion my_macro*")))))

(provide 'lsp-bridge-rust-test)
;;; lsp-bridge-rust-test.el ends here
