;;; lsp-bridge-imenu.el --- LSP bridge  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'imenu)
(require 'lsp-bridge)

(defcustom lsp-bridge-raw-imenu-modes
  '(emacs-lisp-mode org-mode)
  "Modes that use `lsp-bridge-imenu-function' instead of the server.
A mode listed here has an imenu of its own, so lsp-bridge asks it rather than a
language server."
  :type '(repeat symbol)
  :group 'lsp-bridge)

(defcustom lsp-bridge-imenu-function 'imenu
  "Determines which Imenu function is used.
Possible values: `imenu', `consult-imenu', or `consult-imenu-multi'."
  :type '(choice (const :tag "Default" imenu)
          (const :tag "Use consult-imenu" consult-imenu)
          (const :tag "Use consult-imenu-multi" consult-imenu-multi))
  :group 'lsp-bridge)

;;;###autoload
(defun lsp-bridge-imenu ()
  "Show the imenu of the current buffer.
A major mode listed in `lsp-bridge-raw-imenu-modes' has an imenu of its own, and
`lsp-bridge-imenu-function' is run for it; every other mode asks the language
server for the symbols of the file."
  (interactive)
  (if (memq major-mode lsp-bridge-raw-imenu-modes)
      (call-interactively lsp-bridge-imenu-function)
    (lsp-bridge-call-file-api "imenu")))

(defun lsp-bridge--imenu-show (filename filehost res)
  "Show the imenu of the buffer of FILENAME on FILEHOST from the RES answer.
RES is the list of symbols the server sent; it is turned into the index of
`imenu', which `lsp-bridge-imenu-function' is then run against.  A symbol with
children keeps them, a symbol without any points at its own position."
  (lsp-bridge--with-file-buffer filename filehost
                                (let* ((index-items
                                        (cl-labels
                                            ((dfs (&key name children range &allow-other-keys)
                                               (if (seq-empty-p children)
                                                   (cons name (acm-backend-lsp-position-to-point
                                                               (plist-get range :start)))
                                                 (cons name
                                                       (mapcar (lambda (c) (apply #'dfs c)) children)))))
                                          (mapcar (lambda (s) (apply #'dfs s)) res)))
                                       (imenu-create-index-function #'(lambda () index-items))
                                       (last-nonmenu-event 13))
                                  (call-interactively lsp-bridge-imenu-function))))

(provide 'lsp-bridge-imenu)
;;; lsp-bridge-imenu.el ends here
