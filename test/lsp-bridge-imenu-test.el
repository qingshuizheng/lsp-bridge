;;; lsp-bridge-imenu-test.el --- Tests for the imenu command  -*- lexical-binding: t; -*-

;;; Commentary:

;; The imenu command has two ways of answering: a mode with an imenu of its own is
;; asked for it, and every other mode asks the language server.  What the server
;; answers is turned into the index imenu works with, which is a tree where a symbol
;; with children keeps them and one without points at its own position.
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/lsp-bridge-imenu-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'lsp-bridge-imenu)

(ert-deftest lsp-bridge-imenu-test-a-mode-with-its-own-imenu ()
  "A mode listed as raw is asked for its own imenu."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (called)
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (function &optional _) (setq called function)))
                ((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (&rest _) (setq called 'the-server))))
        (lsp-bridge-imenu))
      (should (eq called 'imenu)))))

(ert-deftest lsp-bridge-imenu-test-every-other-mode-asks-the-server ()
  "A mode that is not listed asks the server for the symbols of the file."
  (with-temp-buffer
    (fundamental-mode)
    (let (called)
      (cl-letf (((symbol-function 'call-interactively) (lambda (&rest _) (setq called 'the-mode)))
                ((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (method &rest _) (setq called method))))
        (lsp-bridge-imenu))
      (should (equal called "imenu")))))

(provide 'lsp-bridge-imenu-test)
;;; lsp-bridge-imenu-test.el ends here
