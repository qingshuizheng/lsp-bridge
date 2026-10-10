;;; lsp-bridge.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: lsp-bridge.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2022-10-10 15:23:53 +0800
;;           By: Gong Qijian
;; URL: https://github.com/manateelazycat/lsp-bridge
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Lsp-Bridge
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'ansi-color)
(require 'markdown-mode)
(require 'diff)

(defvar acm-library-path (expand-file-name "acm" (if load-file-name
                                                     (file-name-directory load-file-name)
                                                   default-directory))
  "Directory holding the bundled acm completion framework.
Set from the location of this library and added to `load-path'.")
(add-to-list 'load-path acm-library-path t)
(require 'acm-frame)

(require 'lsp-bridge-epc)
(require 'lsp-bridge-ref)
(require 'lsp-bridge-jdtls)
(require 'lsp-bridge-peek)
(require 'lsp-bridge-call-hierarchy)
(require 'lsp-bridge-code-action)
(require 'lsp-bridge-diagnostic)
(require 'lsp-bridge-lsp-installer)
(require 'lsp-bridge-org-babel)
(require 'lsp-bridge-inlay-hint)
(require 'lsp-bridge-dart)
(require 'lsp-bridge-semantic-tokens)
(require 'lsp-bridge-rust)
(require 'lsp-bridge-breadcrumb)

(defgroup lsp-bridge nil
  "LSP-Bridge group."
  :group 'applications)

(require 'acm)
(require 'tramp)
(defvar lsp-bridge-tramp-alias-alist nil
  "Alist mapping tramp host aliases to canonical host names.
Users set it to tell lsp-bridge which tramp host an alias refers to.")

(defvar lsp-bridge-tramp-connection-info nil
  "Cached tramp connection information of an opened remote file.")

(defvar lsp-bridge--remote-ip-to-host nil
  "Alist mapping remote server IPs to hostnames.
Populated lazily when the remote server sends an IP (via
`get_lsp_file_host') that doesn't appear in
`lsp-bridge-tramp-alias-alist'.")

(defun lsp-bridge--resolve-remote-host (host)
  "Translate HOST to a known hostname if it is an unrecognized IP.
Returns HOST unchanged when it is already a known key in
`lsp-bridge-tramp-alias-alist', or the empty string.  Otherwise
checks a cache and, as a last resort, searches open buffers for a
remote buffer whose `lsp-bridge-remote-file-host' IS a known key."
  (cond
   ((string= host "") host)
   ;; Already a known hostname.
   ((assoc host lsp-bridge-tramp-alias-alist) host)
   ;; Cached IP-to-hostname.
   ((cdr (assoc host lsp-bridge--remote-ip-to-host)))
   ;; Unknown IP -- find a remote buffer whose host IS known.
   (t (let ((resolved
             (cl-dolist (buf (buffer-list))
               (with-current-buffer buf
                 (when (and (boundp 'lsp-bridge-remote-file-host)
                            lsp-bridge-remote-file-host
                            (not (string= lsp-bridge-remote-file-host ""))
                            (assoc lsp-bridge-remote-file-host
                                   lsp-bridge-tramp-alias-alist))
                   (cl-return lsp-bridge-remote-file-host))))))
        (when resolved
          (push (cons host resolved) lsp-bridge--remote-ip-to-host))
        (or resolved host)))))

(setq acm-backend-lsp-fetch-completion-item-func 'lsp-bridge-fetch-completion-item-info)

(defun lsp-bridge-fetch-completion-item-info (candidate)
  "Fetch extra completion item info for CANDIDATE.
Send a `completionItem/resolve' request for the file path and key of
CANDIDATE, unless the same item was already requested; the request
fills in the documentation and additional text edits of the item."
  (let ((kind (plist-get candidate :icon))
        (server-name (plist-get candidate :server))
        (key (plist-get candidate :key)))
    ;; Try send `completionItem/resolve' request to fetch `documentation' and `additionalTextEdits' information.
    (unless (equal acm-backend-lsp-fetch-completion-item-ticker (list acm-backend-lsp-filepath key kind))
      (if (lsp-bridge-is-remote-file)
          (lsp-bridge-remote-send-func-request "fetch_completion_item_info" (list acm-backend-lsp-filepath key server-name))
        (lsp-bridge-call-async "fetch_completion_item_info" acm-backend-lsp-filepath key server-name))
      (setq-local acm-backend-lsp-fetch-completion-item-ticker (list acm-backend-lsp-filepath key kind)))))

(defcustom lsp-bridge-completion-popup-predicates '(
                                                    lsp-bridge--not-delete-command
                                                    lsp-bridge--not-follow-complete
                                                    lsp-bridge--not-match-stop-commands
                                                    lsp-bridge--not-match-hide-characters
                                                    lsp-bridge--not-match-hide-keywords

                                                    lsp-bridge--not-only-blank-before-cursor
                                                    lsp-bridge--not-in-string
                                                    lsp-bridge--not-in-org-table

                                                    lsp-bridge--not-execute-macro
                                                    lsp-bridge--not-in-multiple-cursors
                                                    lsp-bridge--not-in-mark-macro

                                                    lsp-bridge--is-evil-state
                                                    lsp-bridge--is-meow-state

                                                    lsp-brige--not-in-chatgpt-response

                                                    lsp-bridge--not-complete-manually
                                                    )
  "Predicate functions that decide whether to popup completion.
Each predicate is called with no argument while handling a
completion response; the menu pops up only when all of them return
non-nil.  The default watches the last command, the cursor context,
macro and multiple-cursor states and the mode."
  :type '(repeat function)
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-predicates '(
                                          lsp-bridge--not-acm-doc-markdown-buffer
                                          lsp-bridge--not-mind-wave-chat-buffer
                                          lsp-bridge--not-zsh-buffer
                                          )
  "Predicate functions that decide whether lsp-bridge may start.
Each predicate is called with no argument from the mode hooks listed
in `lsp-bridge-default-mode-hooks'; lsp-bridge stays disabled in the
buffer when any of them returns nil.  The default skips acm doc,
mind-wave chat and zsh buffers."
  :type '(repeat function)
  :group 'lsp-bridge)
(defcustom lsp-bridge-flash-region-delay .3
  "How many seconds to flash `lsp-bridge-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-heartbeat-interval nil
  "Interval for sending heartbeat to server in seconds.

Setting this to nil or 0 will turn off the heartbeat mechanism."
  :type 'number
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-mode-line t
  "Whether display LSP-bridge's server info in mode-line ."
  :type 'boolean
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-stop-commands
  '("undo-tree-undo" "undo-tree-redo"
    "undo-tree-visualize-redo" "undo-tree-visualize-undo"
    "kill-region" "delete-block-backward"
    "python-black-buffer" "acm-complete-or-expand-yas-snippet" "acm-insert-number-or-complete-candiate"
    "yank" "string-rectangle" "query-replace" "grammatical-edit-unwrap")
  "If last command is match this option, stop popup completion ui."
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-hide-characters '(":" ";" "(" ")" "[" "]" "{" "}" "," "\"" ">")
  "Characters before point that stop the popup completion menu.
When the character before point is one of these strings, completion
does not pop up.  This option only takes effect when
`lsp-bridge-completion-obey-trigger-characters-p' is nil, and it must
then be set with `setq' rather than `custom-set-variables'."
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-obey-trigger-characters-p t
  "Whether trigger characters win over hidden start characters.
When non-nil, a trigger character from the language server pops up
the completion menu even when the character before point is listed in
`lsp-bridge-completion-hide-characters'."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-popup-documentation-buffer " *lsp-bridge-hover*"
  "Buffer for display hover information."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-buffer-documentation-buffer "*lsp-bridge-doc*"
  "Buffer for display documentation information."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-disable-backup t
  "Whether lsp-bridge turns off Emacs backup files.
When non-nil, lsp-bridge sets `make-backup-files', `auto-save-default'
and `create-lockfiles' to nil while it is enabled."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-signature-help t
  "Whether to enable signature-help."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-help-fetch-idle 0.5
  "The idle seconds to fetch signature help.."
  :type 'float
  :safe #'floatp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-search-words t
  "Whether to enable search words of files."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-search-words-prohibit-file-extensions '("png" "jpg" "jpeg" "gif" "pdf")
  "The file extensions to prohibit search words."
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-auto-format-code nil
  "Whether to auto format code."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-auto-format-code-idle 1
  "The idle seconds to auto format code."
  :type 'float
  :safe #'floatp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-diagnostics t
  "Whether to enable diagnostics."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-elisp-symbols-update-idle 3
  "The idle seconds to update elisp symbols."
  :type 'float
  :safe #'floatp
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-langserver-dir nil
  "The directory where the user place langserver configuration."
  :type '(choice (const nil)
                 (string))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-multiserver-dir nil
  "The directory where the user place multiserver configuration."
  :type '(choice (const nil)
                 (string))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-ssh-private-key nil
  "Path of a custom SSH private key used for SSH connections."
  :type '(choice (const nil)
                 (string))
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'lsp-bridge)

(defcustom lsp-bridge-user-ssh-agent nil
  "Whether to use ssh-agent in SSH connections."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-symbols-enable-which-func nil
  "Wether use lsp-bridge in which-func."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-enable-completion-in-string nil
  "Whether to enable completion in string, default is disable."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defcustom lsp-bridge-completion-in-string-file-types '("vue" "dart" "html")
  "File types where completion in string is allowed.
This is a list of file extensions for which
LSP-Bridge will enable completion inside string literals."
  :type '(repeat string)
  :group 'lsp-bridge)

(defface lsp-bridge-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'lsp-bridge)

(defcustom lsp-bridge-tramp-blacklist nil
  "Tramp hosts that don't use lsp-bridge."
  :type '(repeat string)
  :safe #'listp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-python-command "python3"
  "Python command on remote host."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-python-file "~/lsp-bridge/lsp_bridge.py"
  "Full path of lsp_bridge.py file on remote host."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-log "~/lsp-bridge/lbr_log.txt"
  "Full path of log file on remote host."
  :type 'string
  :safe #'stringp
  :group 'lsp-bridge)

(defcustom lsp-bridge-remote-start-automatically nil
  "Whether start remote lsp-bridge.py automatically."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defvar lsp-bridge-last-change-command nil
  "Name of the command that made the last buffer change.")

(defvar lsp-bridge-last-change-position nil
  "Buffer, modification tick and cursor of the last completion change.")

(defvar lsp-bridge-last-change-is-delete-command-p nil
  "Whether the last buffer change deleted text.")

(defvar lsp-bridge-server nil
  "The LSP-Bridge Server.")

(defvar lsp-bridge-python-file (expand-file-name "lsp_bridge.py" (if load-file-name
                                                                     (file-name-directory load-file-name)
                                                                   default-directory))
  "Full path of lsp_bridge.py, the Python side of lsp-bridge.")

(defvar-local lsp-bridge-mark-ring nil
  "The list of saved lsp-bridge marks, most recent first.")

(defvar lsp-bridge-server-port nil
  "Port on which the EPC server of the Python side listens.")

(defun lsp-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p lsp-bridge-server)
    (setq lsp-bridge-server
          (lsp-bridge-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (lsp-bridge-epc-define-method mngr 'eval-in-emacs 'lsp-bridge--eval-in-emacs-func)
               (lsp-bridge-epc-define-method mngr 'get-emacs-vars 'lsp-bridge--get-emacs-vars-func)
               (lsp-bridge-epc-define-method mngr 'get-project-path 'lsp-bridge--get-project-path-func)
               (lsp-bridge-epc-define-method mngr 'get-language-id 'lsp-bridge--get-language-id-func)
               (lsp-bridge-epc-define-method mngr 'get-workspace-folder 'lsp-bridge--get-workspace-folder-func)
               (lsp-bridge-epc-define-method mngr 'get-multi-lang-server 'lsp-bridge--get-multi-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-single-lang-server 'lsp-bridge--get-single-lang-server-func)
               (lsp-bridge-epc-define-method mngr 'get-user-emacs-directory 'lsp-bridge--user-emacs-directory-func)
               (lsp-bridge-epc-define-method mngr 'get-user-tsdk-path 'lsp-bridge--user-tsdk-path-func)
               (lsp-bridge-epc-define-method mngr 'get-buffer-content 'lsp-bridge--get-buffer-content-func)
               (lsp-bridge-epc-define-method mngr 'get-current-line 'lsp-bridge--get-current-line-func)
               (lsp-bridge-epc-define-method mngr 'get-ssh-password 'lsp-bridge--get-ssh-password-func)
               ))))
    (if lsp-bridge-server
        (setq lsp-bridge-server-port (process-contact lsp-bridge-server :service))
      (error "[LSP-Bridge] lsp-bridge-server failed to start")))
  lsp-bridge-server)

(defun lsp-bridge--eval-in-emacs-func (sexp-string)
  "Read and evaluate the elisp form printed in SEXP-STRING.
Always return nil so the EPC reply carries the expected number of
arguments."
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun lsp-bridge--get-emacs-var-func (var-name)
  "Return the value of the Emacs variable named VAR-NAME.
The result is a list of the value and of a string that says whether
the value is boolean, because the Python side converts nil to []."
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun lsp-bridge--get-emacs-vars-func (&rest vars)
  "Return the values of the Emacs variables named in VARS.
Each element of VARS is a variable name string; see
`lsp-bridge--get-emacs-var-func' for the shape of the result."
  (mapcar #'lsp-bridge--get-emacs-var-func vars))

(defvar lsp-bridge-epc-process nil
  "EPC manager that talks to the lsp_bridge.py process.")

(defvar lsp-bridge-internal-process nil
  "Process object of the running lsp_bridge.py server.")

(defvar lsp-bridge-internal-process-prog nil
  "Program used to start the lsp_bridge.py server.")

(defvar lsp-bridge-internal-process-args nil
  "Argument list used to start the lsp_bridge.py server.")

(defvar lsp-bridge-position-before-jump nil
  "Position recorded before jumping to a definition.
Used to return to the original location afterwards.")

(defcustom lsp-bridge-name "*lsp-bridge*"
  "Name of LSP-Bridge buffer."
  :type 'string)

(defcustom lsp-bridge-python-command (cond ((memq system-type '(cygwin windows-nt ms-dos))
                                            (cond ((executable-find "pypy3.exe")
                                                   "pypy3.exe")
                                                  ((executable-find "python3.exe")
                                                   "python3.exe")
                                                  ((executable-find "python.exe")
                                                   "python.exe")))
                                           (t (cond ((executable-find "python-lsp-bridge")
                                                     "python-lsp-bridge")
                                                    ((executable-find "pypy3")
                                                     "pypy3")
                                                    ((executable-find "python3")
                                                     "python3")
                                                    ((executable-find "python")
                                                     "python"))))
  "The Python interpreter used to run lsp_bridge.py."
  :type 'string)

(defcustom lsp-bridge-enable-debug nil
  "Whether to start the lsp-bridge server under gdb for debugging.
Set this when Emacs segfaults; lsp-bridge then starts Python with gdb
and the next crash report can be sent with the `*lsp-bridge*' buffer
content."
  :type 'boolean)

(defcustom lsp-bridge-log-level 'default
  "Log level used for the messages printed to the `*lsp-bridge*' buffer.
Accepted values are `default' (INFO), `debug', `warning', `error' and
`critical'; they select the minimum Python logging level, see
https://docs.python.org/3/library/logging.html#levels."
  :type '(choice
          (const :tag "default log level, which is INFO level" default)
          (const :tag "only for debug purpose" debug)
          (const :tag "warning level" warning)
          (const :tag "error level" error)
          (const :tag "critical level" critical)))

(defcustom lsp-bridge-enable-profile nil
  "Enable this option to output performance data to ~/lsp-bridge.prof."
  :type 'boolean)

(defcustom lsp-bridge-enable-completion-in-minibuffer nil
  "Enable this option to completion in minibuffer."
  :type 'boolean)

(defcustom lsp-bridge-multi-lang-server-extension-list
  '(
    (("vue") . "volar_emmet")
    (("ts" "mts")  . "typescript_eslint")
    (("tsx") . "typescriptreact_eslint")
    (("component.html") . "angular_template_html")
    (("component.ts") . "angular_template_typescript")
    )
  "The multi lang server rule for file extension."
  :type 'cons)

(defcustom lsp-bridge-single-lang-server-extension-list
  '(
    (("wxml") . "wxml-language-server")
    (("html") . "vscode-html-language-server")
    (("astro") . "astro-ls")
    (("typ") . "tinymist")
    )
  "The lang server rule for file extension."
  :type 'cons)

(defcustom lsp-bridge-c-lsp-server "clangd"
  "Default LSP server for C language, you can choose `clangd' or `ccls'."
  :type 'string)

(defcustom lsp-bridge-elixir-lsp-server "dexter"
  "LSP server used for Elixir.
Accepted values are `dexter', `elixirLS', `lexical' and `nextls';
the default is `dexter'."
  :type 'string
  :safe #'stringp)

(defcustom lsp-bridge-php-lsp-server "intelephense"
  "LSP server used for PHP.
Accepted values are `intelephense' and `phpactor'; the default is
`intelephense'."
  :type 'string
  :safe #'stringp)

(defcustom lsp-bridge-python-lsp-server "basedpyright"
  "LSP server used for Python.
Accepted values are `basedpyright', `pyright',
`pyright-background-analysis', `jedi', `python-ms', `pylsp' and
`ruff'; the default is `basedpyright'."
  :type 'string)

(defcustom lsp-bridge-ruby-lsp-server "solargraph"
  "Default LSP server for Ruby.
Possible choices are `solargraph', `ruby-lsp'."
  :type 'string)

(defcustom lsp-bridge-python-multi-lsp-server "basedpyright_ruff"
  "Multi LSP server pair used for Python.
Accepted values are `basedpyright_ruff', `pyright_ruff',
`pyright-background-analysis_ruff', `jedi_ruff', `python-ms_ruff' and
`pylsp_ruff'; the default is `basedpyright_ruff'."
  :type 'string)

(defcustom lsp-bridge-tex-lsp-server "texlab"
  "LSP server used for (La)TeX.
Accepted values are `texlab', `digestif' and `ltex-ls'; the default
is `texlab'."
  :type 'string)

(defcustom lsp-bridge-csharp-lsp-server "omnisharp-dotnet"
  "LSP server used for C#.
Accepted values are `omnisharp-mono', `omnisharp-dotnet' and
`csharp-ls'; the default is `omnisharp-dotnet'."
  :type 'string)

(defcustom lsp-bridge-nix-lsp-server "nixd"
  "LSP server used for Nix.
Accepted values are `nixd', `rnix-lsp' and nil; the default is
`nixd'."
  :type 'string)

(defcustom lsp-bridge-markdown-lsp-server "vale-ls"
  "Default LSP server for markdown, you can choose `vale-ls' or `marksman'."
  :type 'string)

(defcustom lsp-bridge-lua-lsp-server "sumneko"
  "LSP server used for Lua.
Accepted values are `sumneko' and `lua-lsp'; the default is
`sumneko'."
  :type 'string)

(defcustom lsp-bridge-verilog-lsp-server "verible"
  "LSP server used for Verilog.
Accepted values are `verible' and `svls'; the default is `verible'."
  :type 'string)

(defcustom lsp-bridge-xml-lsp-server "lemminx"
  "LSP server used for XML.
Accepted values are `lemminx' and `camells'; the default is
`lemminx'."
  :type 'string)

(defcustom lsp-bridge-cmake-lsp-server "cmake-language-server"
  "LSP server used for CMake.
Accepted values are `cmake-language-server' and `neocmakelsp'; the
default is `cmake-language-server'."
  :type 'string)

(defcustom lsp-bridge-r-lsp-server "rlanguageserver"
  "Default LSP server for R, you can choose `rlanguageserver' or `air'."
  :type 'string)

(defcustom lsp-bridge-r-multi-lsp-server "rlanguageserver_air"
  "Default Multi LSP server for R.
Possible choices are `rlanguageserver_air'."
  :type 'string)

(defcustom lsp-bridge-javascript-lsp-server "javascript"
  "Default Multi LSP server for JavaScript.
Possible choices are `javascript', `javascript-go' or `javascript-deno'."
  :type 'string)

(defcustom lsp-bridge-javascript-react-lsp-server "javascriptreact"
  "LSP server used for JSX (React) files.
Accepted values are `javascriptreact', `javascript-go-react' and
`javascript-deno-react'; the default is `javascriptreact'."
  :type 'string)

(defcustom lsp-bridge-typescript-lsp-server "typescript"
  "Default Multi LSP server for TypeScript.
Possible choices are `typescript', `typescript-go' or `typescript-deno'."
  :type 'string)

(defcustom lsp-bridge-typescript-react-lsp-server "typescriptreact"
  "LSP server used for TSX (React) files.
Accepted values are `typescriptreact', `typescript-go-react' and
`typescript-deno-react'; the default is `typescriptreact'."
  :type 'string)

(defcustom lsp-bridge-tsdk-path nil
  "Tsserver lib*.d.ts directory path in current system needed by some lsp servers.
If nil, lsp-bridge would try to detect by default."
  :type '(choice (const nil)
                 (string)))

(defcustom lsp-bridge-use-wenls-in-org-mode nil
  "Whether to use the `wen' LSP server in `org-mode'.")

(defcustom lsp-bridge-use-ds-pinyin-in-org-mode nil
  "Whether to use the `ds-pinyin' LSP server in `org-mode'.")

(defcustom lsp-bridge-complete-manually nil
  "When non-nil, popup the completion menu only on explicit request.
Users then call `lsp-bridge-popup-complete-menu' themselves instead of
letting lsp-bridge pop the menu up while typing.")

(defcustom lsp-bridge-multi-lang-server-mode-list
  '(((python-mode python-ts-mode) . lsp-bridge-python-multi-lsp-server)
    ((qml-mode qml-ts-mode) . "qmlls_javascript")
    (ess-r-mode . lsp-bridge-r-multi-lsp-server))
  "The multi lang server rule for file mode."
  :type 'cons)

(defcustom lsp-bridge-single-lang-server-mode-list
  '(
    ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode c-or-c++-mode c-or-c++-ts-mode) . lsp-bridge-c-lsp-server)
    ((cmake-mode cmake-ts-mode) .                                                lsp-bridge-cmake-lsp-server)
    ((java-mode java-ts-mode) .                                                  "jdtls")
    ((julia-mode) .                                                              "julials")
    ((python-mode python-ts-mode) .                                              lsp-bridge-python-lsp-server)
    ((ruby-mode ruby-ts-mode) .                                                  lsp-bridge-ruby-lsp-server)
    ((rust-mode rustic-mode rust-ts-mode rstml-ts-mode) .                        "rust-analyzer")
    (move-mode .                                                                 "move-analyzer")
    ((elixir-mode elixir-ts-mode heex-ts-mode) .                                 lsp-bridge-elixir-lsp-server)
    ((go-mode go-ts-mode) .                                                      "gopls")
    (groovy-mode .                                                               "groovy-language-server")
    (powershell-mode .                                                           "powershell-editor-services")
    (haskell-mode .                                                              "hls")
    ((lua-mode lua-ts-mode)  .                                                   lsp-bridge-lua-lsp-server)
    ((markdown-mode gfm-mode) .                                                  lsp-bridge-markdown-lsp-server)
    (dart-mode .                                                                 "dart-analysis-server")
    ((scala-mode scala-ts-mode) .                                                "metals")
    ((js2-mode js-mode js-ts-mode rjsx-mode) .                                   lsp-bridge-javascript-lsp-server)
    (js-jsx-mode .                                                               lsp-bridge-javascript-react-lsp-server)
    ((typescript-tsx-mode tsx-ts-mode) .                                         lsp-bridge-typescript-react-lsp-server)
    ((typescript-mode typescript-ts-mode) .                                      lsp-bridge-typescript-lsp-server)
    ((js-json-mode json-ts-mode json-mode) .                                     "vscode-json-language-server")
    (tuareg-mode .                                                               "ocamllsp")
    (erlang-mode .                                                               "erlang-ls")
    ((LaTeX-mode latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) . lsp-bridge-tex-lsp-server)
    ((clojure-mode
      clojurec-mode
      clojurescript-mode
      clojurex-mode
      clojure-ts-mode
      clojure-ts-clojurec-mode
      clojure-ts-clojurescript-mode
      clojure-ts-clojuredart-mode)  .                                                   "clojure-lsp")
    ((sh-mode bash-mode bash-ts-mode) .                                          "bash-language-server")
    (fish-mode .                                                                 "fish-lsp")
    ((css-mode css-ts-mode) .                                                    "vscode-css-language-server")
    (elm-mode   .                                                                "elm-language-server")
    ((php-mode php-ts-mode) .                                                    lsp-bridge-php-lsp-server)
    ((yaml-mode yaml-ts-mode) .                                                  "yaml-language-server")
    ((zig-mode zig-ts-mode) .                                                    "zls")
    ((dockerfile-mode dockerfile-ts-mode) .                                      "docker-langserver")
    (d-mode .                                                                    "serve-d")
    ((fortran-mode f90-mode) .                                                   "fortls")
    ((nix-mode nix-ts-mode) .                                                    lsp-bridge-nix-lsp-server)
    (nickel-mode .                                                               "nls")
    (ess-r-mode .                                                                lsp-bridge-r-lsp-server)
    ((graphql-mode graphql-ts-mode) .                                            "graphql-lsp")
    (swift-mode .                                                                "swift-sourcekit")
    ((csharp-mode csharp-ts-mode) .                                              lsp-bridge-csharp-lsp-server)
    (kotlin-mode .                                                               "kotlin-language-server")
    (verilog-mode .                                                              lsp-bridge-verilog-lsp-server)
    (vhdl-mode .                                                                 "vhdl-tool")
    (svelte-mode .                                                               "svelteserver")
    (fsharp-mode .                                                               "fsautocomplete")
    (beancount-mode .                                                            "beancount-language-server")
    (racket-mode    .                                                            "racket-langserver")
    (mojo-mode    .                                                              "mojo-lsp-server")
    (solidity-mode .                                                             "solidity")
    (gleam-ts-mode .                                                             "gleam")
    (ada-mode .                                                                  "ada-language-server")
    (hyprlang-ts-mode .                                                          "hyprls")
    (scad-mode .                                                                 "openscad-lsp")
    (sml-mode .                                                                  "millet")
    (fuzion-mode .                                                               "fuzion-language-server")
    (fennel-mode .                                                               "fennel-ls")
    (ttcn3-mode .                                                                "ntt")
    (v-mode .                                                                    "v-analyzer")
    (cwl-mode .                                                                  "benten")
    (lean4-mode .                                                                "lean")
    (lean4-select-mode .                                                         "lean")
    (odin-mode .                                                                 "ols")
    (ballerina-mode .                                                            "ballerina-lang-server")
    (bibtex-mode .                                                               "citation-langserver")
    (feature-mode .                                                              "cucumber-language-server")
    (rego-mode .                                                                 "regal")
    (puppet-mode .                                                               "puppet-languageserver")
    (nxml-mode .                                                                 lsp-bridge-xml-lsp-server)
    (robot-mode .                                                                "vscode-rf-language-server")
    (vimrc-mode .                                                                "vim-language-server")
    (wat-mode .                                                                  "wasm-language-tools")
    (terraform-mode .                                                            "terraform-ls")
    (jsonnet-mode .                                                              "jsonnet-language-server")
    (glsl-mode .                                                                 "glsl-language-server")
    (cobol-mode .                                                                "che-che4z-lsp-for-cobol")
    (hlasm-mode .                                                                "che-che4z-lsp-for-hlasm")
    (yang-mode .                                                                 "yang-lsp")
    (mint-mode .                                                                 "mint-ls")
    (purescript-mode .                                                           "purescript-language-server")
    (perl-mode .                                                                 "perl-language-server")
    (futhark-mode .                                                              "futhark-lsp")
    (conf-toml-mode .                                                            "taplo")
    (slint-mode .                                                                "slint-lsp")
    )
  "The lang server rule for file mode."
  :type 'cons)

(defcustom lsp-bridge-default-mode-hooks
  '(c-mode-hook
    c++-mode-hook
    cmake-mode-hook
    java-mode-hook
    julia-mode-hook
    python-mode-hook
    ruby-mode-hook
    ruby-ts-mode-hook
    lua-mode-hook
    lua-ts-mode-hook
    move-mode-hook
    rust-mode-hook
    rstml-ts-mode-hook
    markdown-mode-hook
    rust-ts-mode-hook
    rustic-mode-hook
    erlang-mode-hook
    elixir-mode-hook
    go-mode-hook
    haskell-mode-hook
    haskell-literate-mode-hook
    dart-mode-hook
    scala-mode-hook
    scala-ts-mode-hook
    typescript-mode-hook
    typescript-tsx-mode-hook
    js2-mode-hook
    js-mode-hook
    rjsx-mode-hook
    tuareg-mode-hook
    latex-mode-hook
    LaTeX-mode-hook
    Tex-latex-mode-hook
    texmode-hook
    context-mode-hook
    texinfo-mode-hook
    bibtex-mode-hook
    clojure-mode-hook
    clojurec-mode-hook
    clojurescript-mode-hook
    clojurex-mode-hook
    clojure-ts-mode-hook
    clojurec-ts-mode-hook
    clojurescript-ts-mode-hook
    clojure-dart-ts-mode-hook
    sh-mode-hook
    bash-mode-hook
    web-mode-hook
    css-mode-hook
    elm-mode-hook
    emacs-lisp-mode-hook
    ielm-mode-hook
    lisp-interaction-mode-hook
    org-mode-hook
    php-mode-hook
    php-ts-mode-hook
    yaml-mode-hook
    zig-mode-hook
    groovy-mode-hook
    powershell-mode-hook
    dockerfile-mode-hook
    dockerfile-ts-mode-hook
    d-mode-hook
    f90-mode-hook
    fortran-mode-hook
    nix-mode-hook
    nix-ts-mode-hook
    nickel-mode-hook
    ess-r-mode-hook
    verilog-mode-hook
    swift-mode-hook
    csharp-mode-hook
    csharp-ts-mode-hook
    telega-chat-mode-hook
    markdown-mode-hook
    kotlin-mode-hook
    vhdl-mode-hook
    typst-mode-hook
    typst-ts-mode-hook
    graphql-mode-hook
    graphql-ts-mode-hook
    c-ts-mode-hook
    c++-ts-mode-hook
    cmake-ts-mode-hook
    elixir-ts-mode-hook
    toml-ts-mode-hook
    css-ts-mode-hook
    java-ts-mode-hook
    js-ts-mode-hook
    json-ts-mode-hook
    python-ts-mode-hook
    bash-ts-mode-hook
    typescript-ts-mode-hook
    tsx-ts-mode-hook
    go-ts-mode-hook
    yaml-ts-mode-hook
    svelte-mode-hook
    fsharp-mode-hook
    beancount-mode-hook
    mojo-mode-hook
    solidity-mode-hook
    gleam-ts-mode-hook
    ada-mode-hook
    hyprlang-ts-mode-hook
    scad-mode-hook
    sml-mode-hook
    fuzion-mode-hook
    fennel-mode-hook
    ttcn3-mode-hook
    v-mode-hook
    cwl-mode-hook
    odin-mode-hook
    ballerina-mode-hook
    bibtex-mode-hook
    feature-mode-hook
    rego-mode-hook
    puppet-mode-hook
    nxml-mode-hook
    robot-mode-hook
    vimrc-mode-hook
    terraform-mode-hook
    jsonnet-mode-hook
    glsl-mode-hook
    cobol-mode-hook
    hlasm-mode-hook
    yang-mode-hook
    mint-mode-hook
    purescript-mode-hook
    perl-mode-hook
    futhark-mode-hook
    conf-toml-mode-hook
    fish-mode-hook
    wat-mode-hook
    slint-mode-hook
    )
  "The default mode hook to enable lsp-bridge."
  :type '(repeat variable))

(defcustom lsp-bridge-get-single-lang-server-by-project nil
  "Get lang server with project path and file path.")

(defcustom lsp-bridge-get-multi-lang-server-by-project nil
  "Get lang server with project path and file path.")

(defcustom lsp-bridge-get-language-id nil
  "Function that returns the languageId used for multi-server setup.

Some LSP servers, such as emmet-ls or tailwindcss, need the language
id to follow the framework and the project.  lsp-bridge calls this
function with the project path, the file path, the server name and
the file extension and uses the returned string as languageId.

When this function returns nil, lsp-bridge parses languageId from
file langserver/*.json instead.")

(defcustom lsp-bridge-get-project-path-by-filepath nil
  "Function that returns the project path for a file path.

By default lsp-bridge uses the output of `git rev-parse
--show-toplevel'; customize this function to derive the project path
from the given file path instead.")

(defcustom lsp-bridge-get-workspace-folder nil
  "Function that returns a workspace folder for a project path.

In Java, several projects sometimes have to share one workspace
folder; customize this function to map a project path to the workspace
folder that lsp-bridge should report to the server.")

(defvar lsp-bridge-indent-two-level 2
  "Indentation width of two for the modes that use it.")

(defvar lsp-bridge-indent-four-level 4
  "Indentation width of four for the modes that use it.")

(defvar lsp-bridge-indent-eight-level 8
  "Indentation width of eight for the modes that use it.")

(defvar lsp-bridge-formatting-indent-alist
  '((c-mode                     . c-basic-offset) ; C
    (c-ts-mode                  . c-basic-offset) ; C
    (c++-mode                   . c-basic-offset) ; C++
    (markdown-mode              . c-basic-offset) ; Markdown.
    (csharp-mode                . c-basic-offset) ; C#
    (csharp-ts-mode             . csharp-ts-mode-indent-offset) ; C#
    (d-mode                     . c-basic-offset)               ; D
    (julia-mode                 . c-basic-offset)             ; Julia
    (java-mode                  . c-basic-offset)             ; Java
    (java-ts-mode               . java-ts-mode-indent-offset) ; Java
    (jde-mode                   . c-basic-offset)   ; Java (JDE)
    (js-mode                    . js-indent-level)  ; JavaScript
    (js2-mode                   . js2-basic-offset) ; JavaScript-IDE
    (js3-mode                   . js3-indent-level) ; JavaScript-IDE
    (json-mode                  . js-indent-level)  ; JSON
    (json-ts-mode               . js-indent-level)  ; JSON
    (lua-mode                   . lua-indent-level) ; Lua
    (lua-ts-mode                . lua-ts-indent-offset) ; Lua
    (objc-mode                  . c-basic-offset)   ; Objective C
    (php-mode                   . c-basic-offset)   ; PHP
    (php-ts-mode                . php-ts-mode-indent-offset) ; PHP
    (perl-mode                  . perl-indent-level)         ; Perl
    (cperl-mode                 . cperl-indent-level)        ; Perl
    (raku-mode                  . raku-indent-offset)  ; Perl6/Raku
    (erlang-mode                . erlang-indent-level) ; Erlang
    (ada-mode                   . ada-indent)          ; Ada
    (hyprlang-ts-mode           . hyprlang-ts-mode-indent-offset) ; Hyprlang
    (scad-mode                  . lsp-bridge-indent-two-level) ; OpenSCAD
    (sml-mode                   . sml-indent-level) ; Standard ML
    (fuzion-mode                . lsp-bridge-indent-two-level) ; Fuzion
    (fennel-mode                . lsp-bridge-indent-two-level) ; Fennel
    (ttcn3-mode                 . lsp-bridge-indent-four-level) ; TTCN3
    (v-mode                     . lsp-bridge-indent-four-level) ; V
    (cwl-mode                   . lsp-bridge-indent-four-level) ; Common Workflow
    (odin-mode                  . lsp-bridge-indent-eight-level) ; Odin
    (ballerina-mode             . ballerina-indent-offset) ; Ballerina
    (bibtex-mode                . lsp-bridge-indent-two-level) ; BibTex
    (feature-mode               . feature-indent-level) ; Cucumber
    (rego-mode                  . lsp-bridge-indent-two-level) ; Rego
    (puppet-mode                . puppet-indent-level) ; Puppet
    (nxml-mode                  . lsp-bridge-indent-two-level) ; XML
    (robot-mode                 . robot-mode-basic-offset)     ; Robot
    (vimrc-mode                 . lsp-bridge-indent-four-level) ; Vim
    (terraform-mode             . terraform-indent-level) ; Terraform
    (jsonnet-mode               . jsonnet-indent-level)   ; Jsonnet
    (glsl-mode                  . lsp-bridge-indent-two-level)  ; GLSL
    (cobol-mode                 . lsp-bridge-indent-four-level) ; Cobol
    (hlasm-mode                 . lsp-bridge-indent-eight-level) ; HLASM
    (yang-mode                  . lsp-bridge-indent-two-level) ; Yang
    (mint-mode                  . lsp-bridge-indent-two-level) ; Mint
    (wat-mode                   . lsp-bridge-indent-two-level) ; WebAssembly Text Format
    (purescript-mode            . purescript-indent-offset) ; PureScript
    (futhark-mode               . futhark-indent-level)     ; Futhark
    (sgml-mode                  . sgml-basic-offset)        ; SGML
    (nxml-mode                  . nxml-child-indent)        ; XML
    (nickel-mode                . c-basic-offset)
    (nix-ts-mode                . nix-ts-mode-indent-offset) ; Nix
    (pascal-mode                . pascal-indent-level)       ; Pascal
    (typescript-mode            . typescript-indent-level) ; Typescript
    (typescript-ts-mode         . typescript-ts-mode-indent-offset) ; Typescript
    (tsx-ts-mode                . typescript-ts-mode-indent-offset) ; Typescript[TSX]
    (sh-mode                    . sh-basic-offset)   ; Shell Script
    (ruby-mode                  . ruby-indent-level) ; Ruby
    (ruby-ts-mode               . ruby-indent-level) ; Ruby
    (enh-ruby-mode              . enh-ruby-indent-level) ; Ruby
    (crystal-mode               . crystal-indent-level) ; Crystal (Ruby)
    (css-mode                   . css-indent-offset)    ; CSS
    (move-mode                  . move-indent-offset)   ; Move
    (rust-mode                  . rust-indent-offset)   ; Rust
    (rust-ts-mode               . rust-ts-mode-indent-offset) ; Rust
    (rstml-ts-mode              . rust-ts-mode-indent-offset) ; Rust
    (rustic-mode                . rustic-indent-offset)       ; Rust
    (scala-mode                 . scala-indent:step)          ; Scala
    (scala-ts-mode              . scala-indent:step)          ; Scala
    (powershell-mode            . powershell-indent)      ; PowerShell
    (ess-mode                   . ess-indent-offset)      ; ESS (R)
    (yaml-mode                  . yaml-indent-offset)     ; YAML
    (hack-mode                  . hack-indent-offset)     ; Hack
    (kotlin-mode                . c-basic-offset)         ; Kotlin
    (verilog-mode               . verilog-indent-level)   ; Verilog
    (vhdl-mode                  . vhdl-basic-offset)      ; VHDL
    (go-mode                    . c-basic-offset)         ;Golang
    (go-ts-mode                 . c-basic-offset)         ;Golang
    (svelte-mode                . js-indent-level)        ;Svelte
    (fsharp-mode                . fsharp-indent-offset)   ; F#
    (gleam-ts-mode              . gleam-ts-indent-offset) ; Gleam
    (conf-toml-mode             . toml-indent-offset)     ; Toml
    (default                    . standard-indent)) ; default fallback
  "A mapping from `major-mode' to its indent variable.")

(defcustom lsp-bridge-string-interpolation-open-chars-alist
  '(;; For {}
    (python-mode .        "[^\$]\{")
    (python-ts-mode .     "[^\$]\{")
    ;; For ${}
    (js-mode .            "\$\{")
    (js-ts-mode .         "\$\{")
    (js2-mode .           "\$\{")
    (js3-mode .           "\$\{")
    (typescript-mode .    "\$\{")
    (typescript-ts-mode . "\$\{")
    (sh-mode .            "\$\{")
    (bash-mode .          "\$\{")
    (bash-ts-mode .       "\$\{")
    (fish-mode .          "\$\{")
    (typst-ts-mode .      "\$\{")
    (typst--base-mode .   "\$\{")
    (typst--code-mode .   "\$\{")
    (typst--math-mode .   "\$\{")
    (typst--markup-mode . "\$\{")
    ;; For #{}
    (elixir-mode .        "\#\{")
    (elixir-ts-mode .     "\#\{")
    (ruby-mode .          "\#\{")
    (ruby-ts-mode .       "\#\{")
    ;; For {{}}
    (yaml-mode .          "\{\{"))
  "Alist of major modes and the regexp that opens their interpolation.
Each element is a cons cell of the form (major-mode . open-char-regexp)
and tells lsp-bridge how a string interpolation starts in that mode."
  :type 'cons)

(defvar lsp-bridge-enable-with-tramp t
  "Whether enable lsp-bridge when editing tramp file.")

(defvar lsp-bridge-remote-save-password nil
  "Whether save password in netrc file.")

(defun lsp-bridge-find-file-hook-function ()
  "Sync tramp connection info when visiting a remote file.
Runs in `find-file-hook' and does nothing when
`lsp-bridge-enable-with-tramp' is nil or the file is not remote."
  (when (and lsp-bridge-enable-with-tramp (file-remote-p (buffer-file-name)))
    (lsp-bridge-sync-tramp-remote nil)))

(defun lsp-bridge--setup-tramp-docker-buffer (tramp-file-name)
  "Setup the buffer of TRAMP-FILE-NAME as if it is a non-file buffer."
  (with-current-buffer (get-file-buffer tramp-file-name)
    (setq buffer-file-name nil)
    (setq buffer-file-truename nil)
    (message "set buffer %s buffer-file-name to nil" (buffer-name))))

(add-hook 'find-file-hook #'lsp-bridge-find-file-hook-function)

(defun lsp-bridge--get-indent-width (mode)
  "Return the indentation offset variable for MODE."
  (or (alist-get mode lsp-bridge-formatting-indent-alist)
      (lsp-bridge--get-indent-width (or (get mode 'derived-mode-parent) 'default))))

(cl-defmacro lsp-bridge--with-file-buffer (filename filehost &rest body)
  "Evaluate BODY in the buffer visiting FILENAME on FILEHOST.
FILEHOST is resolved to a known host first and the buffer is looked up
by path for local hosts or by host and path for remote ones."
  (declare (indent 1))
  `(let ((resolved-host (lsp-bridge--resolve-remote-host ,filehost)))
     (when-let* ((buffer (pcase resolved-host
                           ("" (lsp-bridge-get-match-buffer-by-filepath ,filename))
                           (_ (lsp-bridge-get-match-buffer-by-remote-file resolved-host ,filename)))))
       (with-current-buffer buffer
         ,@body))))

(cl-defmacro lsp-bridge-save-position (&rest body)
  "Evaluate BODY and restore point afterwards.
`save-excursion' is not enough for LSP code format, so the line, the
column and the indentation column are recorded before BODY runs and
restored after it."
  `(let* ((current-buf (current-buffer))
          (current-line (line-number-at-pos nil t))
          (current-column (lsp-bridge--calculate-column))
          (indent-column (save-excursion
                           (back-to-indentation)
                           (lsp-bridge--calculate-column))))
     ,@body
     (switch-to-buffer current-buf)
     (goto-line current-line)
     (back-to-indentation)
     (forward-char (max (- current-column indent-column) 0))))

(defun lsp-bridge-is-remote-file ()
  "Return non-nil if the current buffer visits a remote file.
Remote files are opened by lsp-bridge itself and mark the buffer with
the buffer-local `lsp-bridge-remote-file-flag'."
  (and (boundp 'lsp-bridge-remote-file-flag)
       lsp-bridge-remote-file-flag))

(defun lsp-bridge-get-buffer-file-name-text ()
  "Return the file name text of the current buffer.
Falls back to the source file of an active org `C-c '' edit buffer
when the buffer itself is not visiting a file."
  (or (lsp-bridge-buffer-file-name buffer-file-name)
      (when (bound-and-true-p org-src-mode)
        org-src-source-file-name)))

(defun lsp-bridge-buffer-file-name (name)
  "Return NAME without text properties when it is a string.
The variable `buffer-file-name' may carry face properties, which are
removed here; return nil when NAME is not a string."
  ;; `buffer-file-name' may contain face property, we need use `substring-no-properties' remove those face from buffer name.
  (when (stringp name)
    (substring-no-properties name)))

(defun lsp-bridge-get-buffer-truename (&optional filename)
  "Return the truename of FILENAME or of the current buffer file.
For remote files the path recorded in `lsp-bridge-remote-file-path' is
returned instead; return nil when no file name is available."
  (if (lsp-bridge-is-remote-file)
      lsp-bridge-remote-file-path
    (let ((name (or filename
                    (lsp-bridge-get-buffer-file-name-text))))
      (when name
        (file-truename name)))))

(defun lsp-bridge-get-match-buffer-by-remote-file (host path)
  "Return the buffer visiting PATH on the remote HOST.
The buffer is looked up among all buffers through their buffer-local
`lsp-bridge-remote-file-path' and `lsp-bridge-remote-file-host';
host \"127.0.0.1\" matches any buffer because the server reports it
when it runs inside a container."
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'lsp-bridge-remote-file-path)
                 (lsp-bridge-path-equal lsp-bridge-remote-file-path path)
                 (boundp 'lsp-bridge-remote-file-host)
                 (or (string-equal lsp-bridge-remote-file-host host)
                     ;; host is "127.0.0.1" sent from get_lsp_file_host() when server running inside container
                     ;; client connect to lsp bridge server from local host
                     (string-equal "127.0.0.1" host)))
        (cl-return buffer)))))

(defun lsp-bridge-get-match-buffer-by-filepath (name)
  "Return the buffer whose file path is NAME.
Each buffer file name is compared with both its literal name and its
truename, so symlinked paths still match; return nil when no buffer
visits the file."
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((file-name (buffer-file-name buffer))
                  (match-buffer (or (lsp-bridge-path-equal file-name name)
                                    (lsp-bridge-path-equal (file-truename file-name) name))))
        (cl-return buffer)))))

(defun lsp-bridge-path-equal (path-a path-b)
  "Return non-nil if PATH-A and PATH-B name the same path.
Comparison is case-insensitive on Windows and Cygwin systems and case
sensitive everywhere else."
  (cond ((memq system-type '(cygwin windows-nt ms-dos))
         (string-equal (downcase path-a) (downcase path-b)))
        (t (string-equal path-a path-b))))

(defun lsp-bridge--get-project-path-func (filename)
  "Return the project root path of FILENAME, search order:

1. Follow the rule of `lsp-bridge-get-project-path-by-filepath'
2. Search up `.dir-locals.el'
3. Search up `.git'"
  (if lsp-bridge-get-project-path-by-filepath
      ;; Fetch project root path by `lsp-bridge-get-project-path-by-filepath' if it set by user.
      (funcall lsp-bridge-get-project-path-by-filepath filename)
    ;; Otherwise try to search up `.dir-locals.el' file
    (let* ((result (dir-locals-find-file filename))
           (dir (if (consp result) (car result) result)))
      (when dir (directory-file-name dir)))))

(defun lsp-bridge--get-language-id-func (project-path file-path server-name extension-name)
  "Return the languageId for FILE-PATH.
PROJECT-PATH, SERVER-NAME and EXTENSION-NAME describe the file and the
server.  `lsp-bridge-get-language-id' wins when it is set, otherwise
the extension of tailwindcss files is mapped to the React language id."
  (if lsp-bridge-get-language-id
      (funcall lsp-bridge-get-language-id project-path file-path server-name extension-name)
    ;; Some LSP server, such as Tailwindcss, languageId is a dynamically field follow with file extension,
    ;; we can't not receive respond to `completionItem/resolve` request if send wrong languageId to tailwindcss.
    ;;
    ;; Please reference issue https://github.com/tailwindlabs/tailwindcss-intellisense/issues/925.
    (when (string-equal server-name "tailwindcss")
      (if (string-equal extension-name "jsx")
          "javascriptreact"
        extension-name))))

(defun lsp-bridge--get-workspace-folder-func (project-path)
  "Return a workspace folder for PROJECT-PATH.
The value comes from `lsp-bridge-get-workspace-folder' and is nil when
that option is unset."
  (when lsp-bridge-get-workspace-folder
    (funcall lsp-bridge-get-workspace-folder project-path)))

(defun lsp-bridge--get-multi-lang-server-func (project-path filename)
  "Return the multi lang server name for FILENAME in PROJECT-PATH.
The project function wins when set, then the extension and the file
mode rules are tried."
  (or (when lsp-bridge-get-multi-lang-server-by-project
        (funcall lsp-bridge-get-multi-lang-server-by-project project-path filename))
      (lsp-bridge-get-multi-lang-server-by-extension filename)
      (lsp-bridge-get-multi-lang-server-by-file-mode filename)))

(defun lsp-bridge--get-single-lang-server-func (project-path filename)
  "Return the single lang server name for FILENAME in PROJECT-PATH.
The project function wins when set, then the extension and the file
mode rules are tried."
  (or (when lsp-bridge-get-single-lang-server-by-project
        (funcall lsp-bridge-get-single-lang-server-by-project project-path filename))
      (lsp-bridge-get-single-lang-server-by-extension filename)
      (lsp-bridge-get-single-lang-server-by-file-mode filename)))

(defun lsp-bridge--user-emacs-directory-func ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun lsp-bridge--user-tsdk-path-func ()
  "Get tsserver lib*.d.ts directory path."
  (when-let* (((null lsp-bridge-tsdk-path))
              (bin (executable-find "tsc"))
              (tsdk (expand-file-name "../../lib" (file-truename bin)))
              ((file-exists-p tsdk)))
    (setq lsp-bridge-tsdk-path tsdk))
  (or lsp-bridge-tsdk-path ""))

(defun lsp-bridge--get-buffer-content-func (buffer-name &optional no-org-babel)
  "Return the content of BUFFER-NAME for the language server.
BUFFER-NAME is the name returned by `buffer-name'.  When
`lsp-bridge-enable-org-babel' is enabled and the buffer is an org
buffer, the current src block content is returned unless
NO-ORG-BABEL is non-nil."
  (when-let* ((buf (get-buffer buffer-name)))
    (if (and lsp-bridge-enable-org-babel
             (eq major-mode 'org-mode) (not no-org-babel))
        (and lsp-bridge-org-babel--info-cache
             (org-element-property :value lsp-bridge-org-babel--info-cache))
      (with-current-buffer buf
        (if (and (eq major-mode 'org-mode) buffer-file-name)
            ;; Org buffer: prefer active C-c ' sub-buffer content.
            ;; When a C-c ' buffer is editing a src block in this org file,
            ;; return the pure src block content (no org markup).
            (let ((c-sub (car (delq nil
                              (mapcar (lambda (b)
                                        (with-current-buffer b
                                          (when (and (bound-and-true-p org-src-mode)
                                                     org-src--beg-marker
                                                     (eq (marker-buffer org-src--beg-marker) buf))
                                            b)))
                                      (buffer-list))))))
              (if c-sub
                  (with-current-buffer c-sub
                    (buffer-substring-no-properties (point-min) (point-max)))
                (buffer-substring-no-properties (point-min) (point-max))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun lsp-bridge--get-current-line-func ()
  "Return the current line of this buffer without text properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defvar-local lsp-bridge-tramp-sync-var nil
  "Buffer-local cache used while syncing a tramp file with the server.")

(defun lsp-bridge--get-ssh-password-func (user host port)
  "Return the SSH password of USER on HOST at PORT.
The password is looked up with `auth-source-search', which may prompt
the user; it is saved when `lsp-bridge-remote-save-password' is
non-nil and nil is returned when the user quits the prompt."
  (condition-case nil
      (let* ((auth-source-creation-prompts
              '((secret . "password for %u@%h: ")))
             (found (nth 0 (auth-source-search :max 1
                                               :host host
                                               :user user
                                               :port port
                                               :require '(:secret)
                                               :create t))))
        (when (and lsp-bridge-remote-save-password (plist-get found :save-function))
          (funcall (plist-get found :save-function)))
        (if found
            (auth-info-password found)
          nil))
    (quit (progn
            (message "Cancelled password input.")
            nil))))

(defun lsp-bridge-find-langserver-info-by-extension (file-extension extension-list)
  "Return the first langserver entry of EXTENSION-LIST for FILE-EXTENSION.
An entry key is matched literally when it is a string and with
`member' when it is a list of extensions."
  (cl-find-if
   (lambda (pair)
     (let ((extension (car pair)))
       (if (eq (type-of extension) 'string)
           (string-equal file-extension extension)
         (member file-extension extension))))
   extension-list))

(defun lsp-bridge-get-lang-server-by-extension (filename extension-list)
  "Return the language server entry of EXTENSION-LIST for FILENAME.
The full extension after the first dot is matched first, then the file
extension; the cdr of the matching entry is returned."
  ;; Don't search from extension list if filename not include any extension name.
  (when-let* ((dot-pos (cl-position ?. filename))
              (file-extension (when dot-pos
                                (substring filename (1+ dot-pos) (length filename)))))
    (let (langserver-info)
      ;; Search multi-extension first, to support Angular file, reference https://github.com/manateelazycat/lsp-bridge/pull/1144
      (setq langserver-info (lsp-bridge-find-langserver-info-by-extension file-extension extension-list))

      ;; Search file extension if multi-extension not found in extension list.
      (unless langserver-info
        (setq langserver-info (lsp-bridge-find-langserver-info-by-extension (file-name-extension filename) extension-list)))

      ;; Pick up langserver info.
      (when langserver-info
        (cdr langserver-info)))))

(defun lsp-bridge-get-multi-lang-server-by-extension (filename)
  "Return the multi lang server name for FILENAME.
The rule is looked up in `lsp-bridge-multi-lang-server-extension-list'."
  (lsp-bridge-get-lang-server-by-extension filename lsp-bridge-multi-lang-server-extension-list))

(defun lsp-bridge-get-single-lang-server-by-extension (filename)
  "Return the lang server name for FILENAME.
The rule is looked up in
`lsp-bridge-single-lang-server-extension-list'."
  (lsp-bridge-get-lang-server-by-extension filename lsp-bridge-single-lang-server-extension-list))

(defun lsp-bridge-lang-server-by-mode (target-mode mode-list)
  "Return the first entry of MODE-LIST that matches TARGET-MODE."
  (cl-find-if
   (lambda (pair)
     (let ((mode (car pair)))
       (if (symbolp mode)
           (eq target-mode mode)
         (member target-mode mode))))
   mode-list))

(defun lsp-bridge-get-symbol-string-value (info)
  "Return the value of INFO when it is a string or a symbol.
A string is returned as is, a symbol is resolved with `symbol-value';
this lets langserver rules hold either a name or a variable."
  (pcase (format "%s" (type-of info))
    ("string" info)
    ("symbol" (symbol-value info))
    ))

(defun lsp-bridge-get-mode-name-from-file-path (file-path)
  "Return the major mode that `auto-mode-alist' picks for FILE-PATH."
  (cdr (assoc file-path
              auto-mode-alist
              'string-match-p)))

(defun lsp-brige-get-mode (filepath)
  "Return the major mode of the buffer visiting FILEPATH.
The mode of an open buffer is preferred and
`lsp-bridge-get-mode-name-from-file-path' is used for files that are
not open."
  (let ((buffer (lsp-bridge-get-match-buffer-by-filepath filepath)))
    (if buffer
        (with-current-buffer buffer
          major-mode)
      (lsp-bridge-get-mode-name-from-file-path filepath))))

(defun lsp-bridge-get-multi-lang-server-by-file-mode (filename)
  "Return the multi lang server name for FILENAME.
The major mode of the file is looked up in
`lsp-bridge-multi-lang-server-mode-list'."
  (when-let* ((mode (lsp-brige-get-mode filename))
              (langserver-info (lsp-bridge-lang-server-by-mode mode lsp-bridge-multi-lang-server-mode-list)))
    (lsp-bridge-get-symbol-string-value (cdr langserver-info))))

(defun lsp-bridge-get-single-lang-server-by-file-mode (filename)
  "Return the single lang server name for FILENAME.
For .org files, scan all buffers for an active org src edit buffer
\(`org-src-mode') editing this file and use its major-mode (e.g.
groovy-mode) for server lookup.  This avoids depending on the
calling buffer's local variables, which may not be set when
Python queries get-single-lang-server.
If no sub-buffer is active, use the Org buffer for inline Babel lookup."
  (if (string-suffix-p ".org" filename)
      (let ((c-sub (car (delq nil
                        (mapcar (lambda (b)
                                  (with-current-buffer b
                                    (when (and (bound-and-true-p org-src-mode)
                                               org-src--beg-marker
                                               (let ((obuf (marker-buffer org-src--beg-marker)))
                                                 (and obuf (equal (buffer-file-name obuf) filename))))
                                      b)))
                                (buffer-list))))))
        (if c-sub
            (with-current-buffer c-sub
              (when-let* ((info (lsp-bridge-lang-server-by-mode
                                 major-mode
                                 lsp-bridge-single-lang-server-mode-list)))
                (lsp-bridge-get-symbol-string-value (cdr info))))
          ;; Inline Babel editing has no separate org-src buffer.
          (when-let* ((buffer (lsp-bridge-get-match-buffer-by-filepath filename)))
            (with-current-buffer buffer
              (cond
               (lsp-bridge-use-wenls-in-org-mode "wen")
               (lsp-bridge-use-ds-pinyin-in-org-mode "ds-pinyin")
               (lsp-bridge-enable-org-babel
                (lsp-bridge-org-babel-check-lsp-server)))))))
    (let* ((mode (lsp-brige-get-mode filename))
           (langserver-info (lsp-bridge-lang-server-by-mode mode lsp-bridge-single-lang-server-mode-list)))
      (cond (langserver-info
             (lsp-bridge-get-symbol-string-value (cdr langserver-info)))
            ((eq mode 'org-mode)
             (cond
              (lsp-bridge-use-wenls-in-org-mode
               "wen")
              (lsp-bridge-use-ds-pinyin-in-org-mode
               "ds-pinyin")
              (lsp-bridge-enable-org-babel
               (lsp-bridge-org-babel-check-lsp-server))))))))

(defun lsp-bridge-has-lsp-server-p ()
  "Return the LSP server names that apply to the current buffer.
Org buffers use the Babel check, a started server reuses
`acm-backend-lsp-server-names', otherwise the server is looked up from
the file name through the multi and single lang server rules."
  (cond ((and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
         (setq-local acm-is-elisp-mode-in-org nil)
         (lsp-bridge-org-babel-check-lsp-server))
        ;; `acm-backend-lsp-server-names' is set after LSP server start,
        ;; we don't need search below LSP rules if `acm-backend-lsp-server-names' is set
        ((and (boundp 'acm-backend-lsp-server-names)
              acm-backend-lsp-server-names)
         acm-backend-lsp-server-names)
        ;; Search LSP rules.
        (t
         (when-let* ((filename (or (ignore-errors (file-truename
                                                   (lsp-bridge-get-buffer-file-name-text)))
                                   (when (lsp-bridge-is-remote-file)
                                     lsp-bridge-remote-file-path))))
           (let* ((multi-lang-server-name (or (lsp-bridge-get-multi-lang-server-by-extension filename)
                                              (lsp-bridge-get-multi-lang-server-by-file-mode filename)))
                  (lang-server-by-name (or (lsp-bridge-get-single-lang-server-by-extension filename)
                                           (lsp-bridge-get-single-lang-server-by-file-mode filename))))
             (if multi-lang-server-name
                 multi-lang-server-name
               lang-server-by-name)
             )))))

(defun lsp-bridge-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) args)))

(defvar-local lsp-bridge-buffer-file-deleted nil
  "Whether the file of this buffer has been deleted on disk.
While it is non-nil lsp-bridge sends no LSP request for the buffer and
clears the flag as soon as the file exists again.")

(defun lsp-bridge-process-live-p ()
  "Return non-nil if the lsp-bridge EPC process is alive."
  (lsp-bridge-epc-live-p lsp-bridge-epc-process))

(defun lsp-bridge-call-file-api-p ()
  "Return non-nil if file LSP requests may be sent from this buffer.
That requires `lsp-bridge-mode', an applicable language server, an
available server command and a live lsp-bridge process."
  (and lsp-bridge-mode
       (lsp-bridge-has-lsp-server-p)
       (if (boundp 'acm-backend-lsp-server-command-exist)
           acm-backend-lsp-server-command-exist
         t)
       (lsp-bridge-process-live-p)))

(defun lsp-bridge-call-file-api (method &rest args)
  "Send the file LSP request METHOD with ARGS for the current buffer.
The request is forwarded to the remote server for remote buffers,
skipped while a tramp file is still being updated, and otherwise sent
with the buffer file path prepended to ARGS.  A `closeFile' request is
sent and the buffer is marked deleted once its file no longer exists."
  (if (lsp-bridge-is-remote-file)
      (lsp-bridge-remote-send-lsp-request method args)
    (if (and buffer-file-name (file-remote-p (buffer-file-name)))
        (message "[LSP-Bridge] remote file \"%s\" is updating info... skip call %s."
                 (buffer-file-name) method)
      (when (lsp-bridge-call-file-api-p)
        (if (and (boundp 'acm-backend-lsp-filepath)
                 (file-exists-p acm-backend-lsp-filepath))
            (if lsp-bridge-buffer-file-deleted
                ;; If buffer's file create again (such as switch branch back), we need save buffer first,
                ;; send the LSP request after the file is changed next time.
                (progn
                  (save-buffer)
                  (setq-local lsp-bridge-buffer-file-deleted nil)
                  (message "[LSP-Bridge] %s is back, will send the %s LSP request after the file is changed next time." acm-backend-lsp-filepath method))
              (when (and acm-backend-lsp-filepath
                         (not (string-equal acm-backend-lsp-filepath "")))
                (lsp-bridge-deferred-chain
                  (lsp-bridge-epc-call-deferred lsp-bridge-epc-process (read method) (append (list acm-backend-lsp-filepath) args)))))
          ;; We need send `closeFile' request to lsp server if we found buffer's file is not exist,
          ;; it is usually caused by switching branch or other tools to delete file.
          ;;
          ;; We won't send any lsp request until buffer's file create again.
          (unless lsp-bridge-buffer-file-deleted
            (lsp-bridge-close-buffer-file)
            (setq-local lsp-bridge-buffer-file-deleted t)
            (message "[LSP-Bridge] %s is not exist, stop send the %s LSP request until file create again." acm-backend-lsp-filepath method)))))))

(defvar lsp-bridge-log-buffer-window nil
  "Window state of the `*lsp-bridge*' buffer before a restart.
Holds a cons of the log buffer window and the window to select again.")

(defun lsp-bridge--process-log-output (process output)
  "Insert PROCESS OUTPUT into log buffer with ANSI text properties."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (start (marker-position (process-mark process))))
          (save-excursion
            (goto-char start)
            (insert output)
            (when (or ansi-color-context (string-match-p "\e" output))
              (ansi-color-apply-on-region start (point)))
            (set-marker (process-mark process) (point))))))))

(defun lsp-bridge-restart-process ()
  "Stop and restart LSP-Bridge process."
  (interactive)
  ;; Record log buffer window before restart lsp-bridge process.
  (setq lsp-bridge-log-buffer-window
        (cl-dolist (buffer (buffer-list))
          (when (string-equal (buffer-name buffer) lsp-bridge-name)
            (cl-return (cons (get-buffer-window buffer) (selected-window)))
            )))

  ;; Restart lsp-bridge process.
  (lsp-bridge-kill-process)
  (lsp-bridge-start-process)

  ;; Try restore lsp-bridge log buffer after restart.
  (when lsp-bridge-log-buffer-window
    (save-excursion
      (when (window-live-p (car lsp-bridge-log-buffer-window))
        (select-window (car lsp-bridge-log-buffer-window))
        (switch-to-buffer lsp-bridge-name))
      (select-window (cdr lsp-bridge-log-buffer-window))))

  (message "[LSP-Bridge] Process restarted."))

(defun lsp-bridge-profile-dump ()
  "Ask the server to dump its collected profiling data.
The data is written to ~/lsp-bridge.prof when
`lsp-bridge-enable-profile' is enabled."
  (interactive)
  (lsp-bridge-call-async "profile_dump"))

(defun lsp-bridge-start-process ()
  "Start LSP-Bridge process if it isn't started."
  (if (and (process-live-p lsp-bridge-server)
           (process-live-p lsp-bridge-internal-process))
      (remove-hook 'post-command-hook #'lsp-bridge-start-process)
    ;; start epc server and set `lsp-bridge-server-port'
    (lsp-bridge--start-epc-server)
    (let* ((lsp-bridge-args (append
                             (when (member lsp-bridge-python-command '("pipx" "uv"))
                               (list "run"))
                             (list lsp-bridge-python-file)
                             (list (number-to-string lsp-bridge-server-port))
                             (when lsp-bridge-enable-profile
                               (list "profile"))
                             )))

      ;; Set process arguments.
      (if lsp-bridge-enable-debug
          (progn
            (setq lsp-bridge-internal-process-prog "gdb")
            (setq lsp-bridge-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" lsp-bridge-python-command) lsp-bridge-args)))
        (setq lsp-bridge-internal-process-prog lsp-bridge-python-command)
        (setq lsp-bridge-internal-process-args lsp-bridge-args))

      ;; Start python process.
      (let ((process-connection-type (not (lsp-bridge--called-from-wsl-on-windows-p))))
        (setq lsp-bridge-internal-process
              (apply 'start-process
                     lsp-bridge-name lsp-bridge-name
                     lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)))
      (with-current-buffer (process-buffer lsp-bridge-internal-process)
        (setq-local ansi-color-context nil))
      (set-process-filter lsp-bridge-internal-process #'lsp-bridge--process-log-output)
      (set-process-query-on-exit-flag lsp-bridge-internal-process nil))))

(defun lsp-bridge--called-from-wsl-on-windows-p ()
  "Check whether lsp-bridge is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" lsp-bridge-python-command)))

(defvar lsp-bridge-stop-process-hook nil
  "Hook run before the lsp-bridge server process is killed.
Each function is called with no argument from
`lsp-bridge-kill-process'.")

(defun lsp-bridge-kill-process ()
  "Stop LSP-Bridge process and kill all LSP-Bridge buffers."
  (interactive)

  ;; Clean up.
  (lsp-bridge--cleanup)

  ;; Run stop process hooks.
  (run-hooks 'lsp-bridge-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (lsp-bridge--kill-python-process))

(add-hook 'kill-emacs-hook #'lsp-bridge-kill-process)

(defalias 'lsp-bridge-stop-process #'lsp-bridge-kill-process)

(defun lsp-bridge--kill-python-process ()
  "Kill LSP-Bridge background python process."
  (when (lsp-bridge-process-live-p)
    ;; Cleanup before exit LSP-Bridge server process.
    (lsp-bridge-call-async "cleanup")
    ;; Delete LSP-Bridge server process.
    (lsp-bridge-epc-stop-epc lsp-bridge-epc-process)
    ;; Kill *lsp-bridge* buffer.
    (when (get-buffer lsp-bridge-name)
      (kill-buffer lsp-bridge-name))
    (setq lsp-bridge-epc-process nil)
    (setq lsp-bridge-internal-process nil)
    (message "[LSP-Bridge] Process terminated.")))

(defun lsp-bridge--first-start (lsp-bridge-epc-port)
  "Create the EPC manager for the server at LSP-BRIDGE-EPC-PORT.
Also indexes search words and syncs the elisp symbols to the server."
  ;; Make EPC process.
  (setq lsp-bridge-epc-process (make-lsp-bridge-epc-manager
                                :server-process lsp-bridge-internal-process
                                :commands (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args)
                                :title (mapconcat 'identity (cons lsp-bridge-internal-process-prog lsp-bridge-internal-process-args) " ")
                                :port lsp-bridge-epc-port
                                :connection (lsp-bridge-epc-connect "127.0.0.1" lsp-bridge-epc-port)
                                ))
  (lsp-bridge-epc-init-epc-layer lsp-bridge-epc-process)

  ;; Search words from opened files.
  (lsp-bridge-search-words-index-files)

  ;; Synchronize elisp symbol to Python side.
  (lsp-bridge-elisp-symbols-update))

(defvar-local lsp-bridge-last-cursor-position 0
  "Cursor position seen by the last `post-command-hook' call.")

(defvar-local lsp-bridge-prohibit-completion nil
  "Whether completion is temporarily prohibited in this buffer.")

(defvar-local lsp-bridge-cursor-before-command 0
  "Cursor position recorded before the current command runs.")

(defvar-local lsp-bridge-cursor-after-command 0
  "Cursor position recorded after the current command ran.")

(defun lsp-bridge-monitor-pre-command ()
  "Record the cursor before a command and prepare acm state.
Runs in `pre-command-hook': it saves the cursor position, completes a
pending acm preview and rewrites `this-command' while an acm filter
overlay accepts the typed characters."
  (setq-local lsp-bridge-cursor-before-command (point))

  ;; Tab-and-go
  (when (and acm-preview-overlay
             (not (string-prefix-p "acm" (format "%s" this-command))))
    (acm-complete))

  (when acm-filter-overlay
    (let ((this-command-string (format "%s" this-command)))
      (cond ((member this-command-string '("self-insert-command" "org-self-insert-command"))
             (setq this-command 'acm-filter-insert-char)
             (setq last-command 'acm-filter-insert-char))
            ((member this-command-string '("delete-block-backward" "grammatical-edit-backward-delete"))
             (setq this-command 'acm-filter-delete-char)
             (setq last-command 'acm-filter-delete-char))
            ))))

(defun lsp-bridge-monitor-post-command ()
  "React to the command that just ran.
Runs in `post-command-hook': it records the cursor position, tries
completion after insert commands, sends `change_cursor' and
`document_symbol' requests when the cursor moved, and hides the
documentation, diagnostic, signature and code action popups."
  (setq-local lsp-bridge-cursor-after-command (point))

  (let ((this-command-string (format "%s" this-command)))
    (when (and lsp-bridge-mode
               (member this-command-string '("self-insert-command" "org-self-insert-command" "lsp-bridge-popup-complete-menu")))
      (lsp-bridge-try-completion))

    (when (lsp-bridge-has-lsp-server-p)
      (unless (equal lsp-bridge-last-cursor-position
                     (setq-local lsp-bridge-last-cursor-position (point)))
        ;; Only show hover when cursor move.
        (when (and lsp-bridge-enable-hover-diagnostic
                   (not (member this-command-string
                                '("self-insert-command" "org-self-insert-command"
                                  "lsp-bridge-diagnostic-jump-next" "lsp-bridge-diagnostic-jump-prev"))))
          (lsp-bridge-diagnostic-maybe-display-error-at-point))

        ;; Only send `change_cursor' request when user change cursor, except cause by mouse wheel.
        (unless (eq last-command 'mwheel-scroll)
          (lsp-bridge-call-file-api "change_cursor" (lsp-bridge--position))
          (if (and lsp-bridge-symbols-enable-which-func
                   (featurep 'which-func) which-function-mode)
              (lsp-bridge-call-file-api "document_symbol" (lsp-bridge--position)))))

      ;; Hide hover tooltip.
      (unless (string-prefix-p "lsp-bridge-popup-documentation-scroll" this-command-string)
        (lsp-bridge-hide-doc-tooltip))

      ;; Hide diagnostic tooltip.
      (unless (member this-command-string '("lsp-bridge-diagnostic-jump-next"
                                            "lsp-bridge-diagnostic-jump-prev"))
        (lsp-bridge-diagnostic-hide-tooltip))

      ;; Hide signature tooltip.
      (lsp-bridge-hide-signature-tooltip)

      ;; Hide code action frame when Emacs got focus.
      (unless (string-prefix-p "lsp-bridge-code-action" this-command-string)
        (unless (member this-command-string '("handle-switch-frame"))
          (ignore-errors
            (lsp-bridge-code-action-popup-quit))))

      ;; Try send inlay hint if window scroll.
      (lsp-bridge-inlay-hint-monitor-window-scroll))))

(defun lsp-bridge-close-buffer-file ()
  "Tell the server that this buffer file was closed.
Remote files get a `close_file' request, local files a `close_file'
and a `search_file_words_close_file' request."
  (if (lsp-bridge-is-remote-file)
      (progn
        (lsp-bridge-remote-send-func-request "close_file" (list lsp-bridge-remote-file-path))
        (lsp-bridge-remote-send-func-request "search_file_words_close_file" (list lsp-bridge-remote-file-path)))

    (when (lsp-bridge-process-live-p)
      (when (and (lsp-bridge-has-lsp-server-p)
                 (boundp 'acm-backend-lsp-filepath))
        (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath))

      (when buffer-file-name
        (lsp-bridge-call-async "search_file_words_close_file" (lsp-bridge-get-buffer-file-name-text))))))

(defun lsp-bridge-set-prefix-style (prefix-style)
  "Set the acm input bound style of this buffer to PREFIX-STYLE.
The Wen LSP server needs an ASCII keyword prefix, other servers need
the symbol bounds as keyword prefix."
  ;; Wen LSP server need `acm-get-input-prefix-bound' return ASCII keyword prefix,
  ;; other LSP server need use `bounds-of-thing-at-point' of symbol as keyword prefix.
  (setq-local acm-input-bound-style prefix-style))

(defun lsp-bridge-set-server-names (filename filehost server-names)
  "Store SERVER-NAMES for the buffer visiting FILENAME on FILEHOST.
The buffer is located with `lsp-bridge--with-file-buffer' and the
names are saved in its buffer-local `acm-backend-lsp-server-names'."
  (lsp-bridge--with-file-buffer filename filehost
                                (setq-local acm-backend-lsp-server-names server-names)))

(defun lsp-bridge-completion--record-items (filename
                                            filehost
                                            candidates
                                            position
                                            server-name
                                            completion-trigger-characters
                                            server-names)
  "Record completion CANDIDATES for the buffer of FILENAME and FILEHOST.
POSITION is where the completion started, SERVER-NAME names the server
that answered and COMPLETION-TRIGGER-CHARACTERS and SERVER-NAMES are
stored for later completion requests.  The items are cached in the
buffer and the completion menu is tried immediately."
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC
  (let ((gc-cons-threshold most-positive-fixnum))
    (lsp-bridge--with-file-buffer filename filehost
                                  ;; Save completion items.
                                  (setq-local acm-backend-lsp-cache-candidates nil)
                                  (setq-local acm-backend-lsp-completion-position position)
                                  (setq-local acm-backend-lsp-completion-trigger-characters completion-trigger-characters)
                                  (setq-local acm-backend-lsp-server-names server-names)
                                  (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)

                                  (let* ((lsp-items acm-backend-lsp-items)
                                         (completion-table (make-hash-table :test 'equal)))
                                    (dolist (item candidates)
                                      (plist-put item :annotation (capitalize (plist-get item :icon)))
                                      (puthash (plist-get item :key) item completion-table))
                                    (puthash server-name completion-table lsp-items)
                                    (setq-local acm-backend-lsp-items lsp-items))
                                  (lsp-bridge-try-completion))))

(defun lsp-bridge-completion-workspace-symbol--record-items (filename
                                                             filehost
                                                             candidates
                                                             server-name
                                                             server-names)
  "Record workspace symbol CANDIDATES for the buffer of FILENAME.
FILEHOST locates the buffer, SERVER-NAME names the answering server
and SERVER-NAMES is stored for later requests.  The candidates are
cached in the buffer and the completion menu is tried immediately."
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC
  (let ((gc-cons-threshold most-positive-fixnum))
    (lsp-bridge--with-file-buffer filename filehost
                                  ;; Save completion items.
                                  (setq-local acm-backend-lsp-workspace-symbol-cache-candidates nil)
                                  (setq-local acm-backend-lsp-workspace-symbol-server-names server-names)

                                  (let* ((lsp-items acm-backend-lsp-workspace-symbol-items)
                                         (completion-table (make-hash-table :test 'equal)))
                                    (dolist (item candidates)
                                      (plist-put item :annotation (capitalize (plist-get item :icon)))
                                      (puthash (plist-get item :key) item completion-table))
                                    (puthash server-name completion-table lsp-items)
                                    (setq-local acm-backend-lsp-workspace-symbol-items lsp-items))
                                  (lsp-bridge-try-completion))))

(defun lsp-bridge-check-predicate (pred current-function)
  "Return non-nil if PRED allows completion for CURRENT-FUNCTION.
PRED is called with no argument; errors are reported and treated as
non-nil so that a broken predicate does not stop `post-command-hook'.
CURRENT-FUNCTION only names the caller in the error message."
  (if (functionp pred)
      (condition-case err
          (let ((result (funcall pred)))
            (when (eq lsp-bridge-log-level 'debug)
              (unless result
                (with-current-buffer (get-buffer-create lsp-bridge-name)
                  (save-excursion
                    (goto-char (point-max))
                    (insert (format "\n*** %s execute predicate '%s' failed with result: '%s'\n"
                                    current-function pred result))))))
            result)
        (error
         (message "Error in predicate '%s': %S at point %S in buffer %S, mode %S"
                  pred err (point) (current-buffer) major-mode)
         ;; return `t' if got error, avoid break `post-command-hook'
         t))
    t))

(defun lsp-bridge-try-completion ()
  "Popup the completion menu when the buffer state allows it.
The menu is shown only when the buffer and cursor are unchanged since
the completion request and every predicate in
`lsp-bridge-completion-popup-predicates' returns non-nil; when a
predicate refuses, the menu is hidden instead."
  (cond (lsp-bridge-prohibit-completion
         (setq-local lsp-bridge-prohibit-completion nil))
        (t
         ;; Don't popup completion menu when `lsp-bridge-last-change-position' (cursor before send completion request) is not equal current cursor position.
         (when (equal lsp-bridge-last-change-position
                      (list (current-buffer) (buffer-chars-modified-tick) (point)))
           ;; Try popup completion frame.
           (if (cl-every (lambda (pred)
                           (lsp-bridge-check-predicate pred "lsp-bridge-try-completion"))
                         lsp-bridge-completion-popup-predicates)
               (progn
                 (acm-template-candidate-init)
                 (acm-update)

                 ;; We need reset `lsp-bridge-manual-complete-flag' if completion menu popup by `lsp-bridge-popup-complete-menu'.
                 (when lsp-bridge-complete-manually
                   (setq-local lsp-bridge-manual-complete-flag nil)))
             (acm-hide)
             )))))

(defun lsp-bridge-record-last-change-position ()
  "Record the current buffer, modification tick and cursor position.
The record is stored in `lsp-bridge-last-change-position' and lets
`lsp-bridge-try-completion' detect buffer changes made since a
completion request was sent."
  (setq lsp-bridge-last-change-position
        (list (current-buffer) (buffer-chars-modified-tick) (point))))

(defun lsp-bridge-popup-complete-menu ()
  "Popup the completion menu on explicit request.
Sets `lsp-bridge-manual-complete-flag', records the current position,
syncs an org src block when Babel is enabled and sends a
`try_completion' request to the server."
  (interactive)
  ;; Set `lsp-bridge-manual-complete-flag' to non-nil, make sure poup completion menu once.
  (setq-local lsp-bridge-manual-complete-flag t)

  ;; Record last change position make sure `lsp-bridge-try-completion' will popup completion menu.
  (lsp-bridge-record-last-change-position)

  ;; Sync src block content to lsp server make sure lsp-bridge can popup completion menu even user don't change code in src block.
  (when (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
    (setq-local lsp-bridge-org-babel--update-file-before-change t)
    (lsp-bridge-org-babel-send-src-block-to-lsp-server))

  ;; We send `try_completion' request directly, because user input nothing before call command `lsp-bridge-popup-complete-menu'.
  (lsp-bridge-call-file-api "try_completion"
                            (lsp-bridge--position)
                            (acm-char-before)
                            (acm-get-input-prefix))

  ;; Complete other non-LSP backends.
  (lsp-bridge-complete-other-backends))

(defun lsp-bridge--not-match-stop-commands ()
  "Return non-nil if the last command is not a completion stop command.
`lsp-bridge-last-change-command' and `last-command' are both checked,
as command names, against `lsp-bridge-completion-stop-commands'."
  (not (or (member (format "%s" lsp-bridge-last-change-command)
                   lsp-bridge-completion-stop-commands)
           (member (format "%s" last-command) lsp-bridge-completion-stop-commands))))

(defun lsp-bridge-string-interpolation-p (string-interpolation-open-chars-alist)
  "Return non-nil if point is inside a string interpolation.
STRING-INTERPOLATION-OPEN-CHARS-ALIST maps the major mode to the
regexp that opens an interpolation."
  (when-let* ((search-char (cdr (assoc (buffer-local-value 'major-mode (current-buffer))
                                       string-interpolation-open-chars-alist)))
              (open-pos (save-excursion (search-backward-regexp search-char nil t))))
    (not (save-excursion (search-backward-regexp "\}" open-pos t)))))

(defun lsp-bridge--not-in-string ()
  "Hide completion if cursor in string area."
  (or
   ;; Allow completion in string.
   lsp-bridge-enable-completion-in-string
   ;; For some languages, allow popup completion menu in string.
   (and (boundp 'acm-backend-lsp-filepath)
        acm-backend-lsp-filepath
        (member (file-name-extension acm-backend-lsp-filepath) lsp-bridge-completion-in-string-file-types))
   ;; Other language not allowed popup completion in string, it's annoy
   (not (acm-in-string-p))
   ;; Allow popup completion menu for string interpolation
   (lsp-bridge-string-interpolation-p lsp-bridge-string-interpolation-open-chars-alist)
   ;; Allow file path completion in string area
   (ignore-errors
     (and (lsp-bridge-elisp-get-filepath)
          (or (file-exists-p (file-name-directory (lsp-bridge-elisp-get-filepath)))
              ;; Allow string in lsp-bridge-remote file.
              (lsp-bridge-is-remote-file))))))

(defun lsp-bridge--not-execute-macro ()
  "Hide completion during executing macros."
  (not executing-kbd-macro))

(defun lsp-bridge--not-in-mark-macro ()
  "Hide completion markmacro enable."
  (not (and (featurep 'markmacro)
            markmacro-overlays)))

(defun lsp-bridge--not-delete-command ()
  "Hide completion menu if last command is delete command."
  (not lsp-bridge-last-change-is-delete-command-p))

(defun lsp-bridge--not-follow-complete ()
  "Hide completion if last command is `acm-complete'."
  (or
   (not (member (format "%s" last-command) '("acm-complete" "acm-complete-quick-access")))
   (member (format "%s" this-command) '("self-insert-command" "org-self-insert-command"))
   ))

(defun lsp-bridge--not-only-blank-before-cursor ()
  "Hide completion if only blank before cursor, except in roam bracket."
  (or (not
       (null
        (split-string (buffer-substring-no-properties
                       (max (1- (point)) (line-beginning-position))
                       (point)))))
      ;; Keep completion after space in Org roam bracket.
      (acm-in-roam-bracket-p)))

(defun lsp-bridge--not-match-hide-characters ()
  "Return non-nil if the character before point allows completion.
A character listed in `lsp-bridge-completion-hide-characters' hides
the menu unless it is a trigger character of the language server."
  (let ((char (ignore-errors (char-to-string (char-before)))))
    (or (and lsp-bridge-completion-obey-trigger-characters-p
             (member char (if (boundp 'acm-backend-lsp-completion-trigger-characters)
                              (symbol-value 'acm-backend-lsp-completion-trigger-characters))))
        (not (member char lsp-bridge-completion-hide-characters)))))

(defun lsp-bridge--not-match-hide-keywords ()
  "Hide completion if string before cursor match some special keywords."
  (let ((string (if (bounds-of-thing-at-point 'symbol)
                    (buffer-substring-no-properties (car (bounds-of-thing-at-point 'symbol))
                                                    (point))
                  (char-to-string (char-before)))))
    (not (when (and (or (derived-mode-p 'ruby-mode)
                        (derived-mode-p 'ruby-ts-mode)
                        (derived-mode-p 'elixir-mode)
                        (derived-mode-p 'elixir-ts-mode))
                    (member string '("do" "end")))))))

(defun lsp-bridge--is-evil-state ()
  "If `evil' mode is enable, only show completion when evil is in insert mode."
  (or (not (featurep 'evil))
      (eq (evil-insert-state-p) evil-mode)
      (evil-emacs-state-p)))

(defun lsp-bridge--is-meow-state ()
  "If `meow' mode is enable, only show completion when meow is in insert mode."
  (or (not (featurep 'meow))
      (eq meow-insert-mode meow-mode)
      (minibufferp)))

(defun lsp-bridge--not-in-multiple-cursors ()
  "If `multiple-cursors' mode is enable, hide completion menu."
  (not (and (featurep 'multiple-cursors)
            multiple-cursors-mode)))

(defun lsp-brige--not-in-chatgpt-response ()
  "Don't popup completion menu if ChatGPT is responsing."
  (not (and (boundp 'mind-wave-is-response-p)
            mind-wave-is-response-p)))

(defvar-local lsp-bridge-manual-complete-flag nil
  "Non-nil means the next completion pops up on user request.
Set by `lsp-bridge-popup-complete-menu' and reset once the menu is
shown.")

(defun lsp-bridge--not-complete-manually ()
  "If `lsp-bridge-complete-manually' is non-nil, hide completion menu."
  (or
   ;; Don't hide completion menu if it has show up.
   (acm-frame-visible-p acm-menu-frame)

   ;; Show completion menu when `lsp-bridge-complete-manually' and `lsp-bridge-manual-complete-flag' are non-nil.
   ;; If `lsp-bridge-complete-manually' is nil, not check `lsp-bridge-manual-complete-flag'.
   (not lsp-bridge-complete-manually)
   lsp-bridge-manual-complete-flag
   ))

(defun lsp-bridge--not-in-org-table ()
  "Return non-nil if point is not inside an org table."
  (not (and (boundp 'org-at-table-p)
            (org-at-table-p))))

(defun lsp-bridge--point-position (pos)
  "Get position of POS."
  (save-excursion
    (goto-char pos)
    (lsp-bridge--position)))

(defun lsp-bridge--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun lsp-bridge--position ()
  "Get position of cursor."
  ;; we should use ABSOLUTE line number to be compatible with narrowed buffer
  (list :line (1- (line-number-at-pos nil t))
        :character (lsp-bridge--calculate-column)))

(defun lsp-bridge--position-in-org ()
  "Get position in org source block.
Return a plist with :line and :character.
The line number is relative to the beginning of the source block."
  (if (and (derived-mode-p 'org-mode)
           (org-in-src-block-p))
      (let* ((element (org-element-context))
             (begin-pos (plist-get (car (cdr element)) :begin))
             (current-line (line-number-at-pos nil t))
             (block-line (line-number-at-pos begin-pos t))
             (relative-line (max 0 (- current-line block-line 1))))
        (list :line relative-line
              :character (lsp-bridge--calculate-column)))
    (lsp-bridge--position)))

(defvar-local lsp-bridge--before-change-begin-pos nil
  "LSP position of the beginning of the last recorded change.")

(defvar-local lsp-bridge--before-change-end-pos nil
  "LSP position of the end of the last recorded change.")

(defvar-local lsp-bridge--before-change-begin-point nil
  "Buffer position of the beginning of the last recorded change.")

(defvar-local lsp-bridge--before-change-end-point nil
  "Buffer position of the end of the last recorded change.")

(defun lsp-bridge-monitor-before-change (begin end)
  "Record the buffer state before a change from BEGIN to END.
Runs in `before-change-functions': it sends the current org src block
to the server when appropriate and, for buffers with an LSP server or
for remote files, saves the change bounds and their LSP positions in
the `lsp-bridge--before-change-*' buffer-local variables."
  ;; Use `save-match-data' protect match data, avoid conflict with command call `search-regexp'.
  (save-restriction
    (widen)
    (save-match-data
      (when (lsp-bridge-has-lsp-server-p)
        ;; Send whole org src block to lsp server.
        (lsp-bridge-org-babel-send-src-block-to-lsp-server))

      ;; Set `lsp-bridge--before-change-begin-pos' and `lsp-bridge--before-change-end-pos'
      ;; if `lsp-bridge-has-lsp-server-p' or `lsp-bridge-is-remote-file'
      (when (or (lsp-bridge-has-lsp-server-p)
                (lsp-bridge-is-remote-file))
        (setq-local lsp-bridge--before-change-begin-point begin)
        (setq-local lsp-bridge--before-change-end-point end)

        (setq-local lsp-bridge--before-change-begin-pos (lsp-bridge--point-position begin))
        (setq-local lsp-bridge--before-change-end-pos (lsp-bridge--point-position end))
        ))))

(defun lsp-bridge-monitor-post-self-insert ()
  "Record the position after a self insert finished.
Runs in `post-self-insert-hook', after pairing modes such as
`electric-pair-mode' and `smartparens-mode' ran, so that the recorded
position is the final one and `lsp-bridge-try-completion' does not
misjudge it."
  ;; Make sure this function be called after `electric-pair-mode'
  ;; `smartparens-mode' or something similar if they are enabled.
  ;;
  ;; Because pairing parethness or quotes will call `after-change-functions'
  ;; multiple times. For example: (| represents the cursor)
  ;;
  ;;         dic|
  ;;
  ;; Press `[', then the following situation will be observed in
  ;; `lsp-bridge-monitor-after-change':
  ;;
  ;;         dic[|
  ;;         dic|
  ;;         dic[|
  ;;         dic[]|
  ;;
  ;; The last position of cursor is wrong, that makes a misjudgment in
  ;; `lsp-bridge-try-completion'.
  (lsp-bridge-record-last-change-position))

(defun lsp-bridge-monitor-after-change (begin end length)
  "Send LSP requests for the change from BEGIN to END of LENGTH.
Runs in `after-change-functions'.  The change is ignored when it only
repeats the bounds recorded by `lsp-bridge-monitor-before-change' or
while the buffer is being reverted; otherwise the last command and
change position are recorded and `change_file', inlay hint, other
backends and search words requests are triggered."
  ;; Nothing change actual if `begin' and `end' equal `lsp-bridge--before-change-begin-point' and `lsp-bridge--before-change-end-point'
  ;; Then we should not send any request to search backend.
  (when (lsp-bridge-process-live-p)
    (unless (and (equal begin lsp-bridge--before-change-begin-point)
                 (equal end lsp-bridge--before-change-end-point))
      ;; Use `save-match-data' protect match data, avoid conflict with command call `search-regexp'.
      (save-restriction
        (widen)
        (save-match-data
          (unless lsp-bridge-revert-buffer-flag
            (let ((change-text (buffer-substring-no-properties begin end)))
              ;; Record last command to `lsp-bridge-last-change-command'.
              (setq lsp-bridge-last-change-command (format "%s" this-command))

              ;; Record last change position to avoid popup outdate completions.
              (lsp-bridge-record-last-change-position)

              ;; Set `lsp-bridge-last-change-is-delete-command-p'
              (setq lsp-bridge-last-change-is-delete-command-p (> length 0))

              ;; Sync change for org babel if we enable it
              (lsp-bridge-org-babel-monitor-after-change begin end length)

              ;; Send LSP requests.
              (when (or (lsp-bridge-call-file-api-p)
                        (lsp-bridge-is-remote-file))

                ;; Uncomment below code to debug `change_file' protocol.
                ;; (message (format "change_file: '%s' '%s' '%s' '%s' '%s' '%s'"
                ;;                  length
                ;;                  lsp-bridge--before-change-begin-pos
                ;;                  lsp-bridge--before-change-end-pos
                ;;                  (lsp-bridge--position)
                ;;                  change-text
                ;;                  (buffer-substring-no-properties (line-beginning-position) (point))
                ;;                  ))

                ;; Send change_file request to trigger LSP completion.
                (lsp-bridge-call-file-api "change_file"
                                          lsp-bridge--before-change-begin-pos
                                          lsp-bridge--before-change-end-pos
                                          length
                                          change-text
                                          (lsp-bridge--position)
                                          (acm-char-before)
                                          (buffer-name)
                                          (acm-get-input-prefix))

                ;; Send inlay hint request.
                (lsp-bridge-inlay-hint-try-send-request))

              ;; Complete other non-LSP backends.
              (lsp-bridge-complete-other-backends)

              ;; Update search words backend.
              ;;
              ;; disable it for org-mode when we enable `lsp-bridge-enable-org-babel'
              ;; it will trigger with wrong pos due to we only update `pos' on src block
              (unless (eq major-mode 'org-mode)
                (lsp-bridge-search-words-update
                 lsp-bridge--before-change-begin-pos
                 lsp-bridge--before-change-end-pos
                 change-text))
              )))))))

(defun lsp-bridge-complete-other-backends ()
  "Trigger the non-LSP completion backends.
Copilot, jupyter, tabby and ctags are each asked for candidates when
their acm option is enabled and the current command and buffer allow
it."
  (let* ((this-command-string (format "%s" this-command))
         (current-word (thing-at-point 'word t))
         (current-symbol (thing-at-point 'symbol t)))
    ;; Copilot search.
    (when (and acm-enable-copilot
               (lsp-bridge-process-live-p)
               ;; Copilot backend not support remote file now, disable it temporary.
               (not (lsp-bridge-is-remote-file))
               ;; Don't enable copilot on Markdown mode, Org mode, ielm and minibuffer, very disruptive to writing.
               (not (or (derived-mode-p 'markdown-mode)
                        (eq major-mode 'org-mode)
                        (derived-mode-p 'inferior-emacs-lisp-mode)
                        (minibufferp))))
      (lsp-bridge-copilot-complete))

    ;; emacs jupyter
    (when (and acm-enable-jupyter
               (lsp-bridge-process-live-p)
               (and lsp-bridge-enable-org-babel
                    (eq major-mode 'org-mode)
                    (not (lsp-bridge-is-org-temp-buffer-p)))
               (org-in-src-block-p 'inside)
               (string-prefix-p "jupyter" (plist-get (car (cdr (org-element-context))) :language)))
      (acm-backend-jupyter-record current-symbol))

    ;; tabby complete
    (when (and acm-enable-tabby
               (lsp-bridge-process-live-p))
      (acm-backend-tabby-record current-symbol))

    (when (and acm-enable-ctags
               (lsp-bridge-process-live-p))
      (unless (or (string-equal current-word "") (null current-word))
        (if (lsp-bridge-is-remote-file)
            ;; remote file buffer do not associate with an actual file on the disk
            ;; the buffer is created by lsp-bridge-opne-remote-file--response
            ;; hence (buffer-file-name) will return nil
            ;; should use buffer local variable lsp-bridge-remote-file-path
            (with-current-buffer (buffer-name)
              (lsp-bridge-remote-send-func-request "ctags_complete"
                                                   (list
                                                    current-word
                                                    (tramp-file-local-name lsp-bridge-remote-file-path)
                                                    acm-backend-ctags-max-candidates
                                                    (1- (point)))))
          (lsp-bridge-call-async "ctags_complete" current-word (buffer-file-name) acm-backend-ctags-max-candidates (1- (point))))))

    ;; Search elisp symbol.
    (lsp-bridge-elisp-symbols-search current-symbol)

    ;; Send change file to search-words backend.
    (unless lsp-bridge-prohibit-completion
      (when (or buffer-file-name
                (lsp-bridge-is-remote-file))
        (let ((current-word (acm-backend-search-file-words-get-point-string)))
          ;; Search words if current prefix is not empty.
          (unless (or (string-equal current-word "") (null current-word))
            (if (lsp-bridge-is-remote-file)
                (lsp-bridge-remote-send-func-request "search_file_words_search" (list current-word))
              (lsp-bridge-call-async "search_file_words_search" current-word))))))

    ;; Send path search request when detect path string.
    (if (acm-in-string-p)
        (when-let* ((filename (lsp-bridge-elisp-get-filepath))
                    (dirname (ignore-errors (expand-file-name (file-name-directory filename)))))
          (if (lsp-bridge-is-remote-file)
              (let ((path (if (tramp-tramp-file-p dirname)
                              (tramp-file-name-localname (tramp-dissect-file-name dirname))
                            dirname)))
                (lsp-bridge-remote-send-func-request "search_paths_search"
                                                     (list path (file-name-base filename))))

            (when (file-exists-p dirname)
              (lsp-bridge-call-async "search_paths_search"
                                     dirname
                                     (file-name-base filename)
                                     ))))
      ;; We need cleanup `acm-backend-path-items' when cursor not in string.
      ;; Otherwise, other completion backend won't show up.
      (setq-local acm-backend-path-items nil))))

(defun lsp-bridge-elisp-get-filepath ()
  "Return the file path at point.
Paths that contain spaces are supported."
  (let* ((file-end (point))
         (filepath (save-excursion
                     (catch 'break
                       (let* ((file-path "")
                              (file-beg 0))
                         (while (acm-in-string-p)
                           (setq file-beg (car (bounds-of-thing-at-point 'filename)))
                           (if file-beg
                               (progn
                                 (setq file-path (buffer-substring file-beg file-end))
                                 (if (and (file-name-directory file-path) (file-exists-p (file-name-directory file-path)))
                                     (progn
                                       (throw 'break file-path))
                                   (if (eq file-beg 1)
                                       ;; Reached beginning of buffer
                                       (throw 'break nil)
                                     (goto-char (1- file-beg)))))
                             (throw 'break nil))))))))
    filepath))

(defun lsp-bridge-elisp-symbols-update ()
  "We need synchronize elisp symbols to Python side when idle."
  (when (lsp-bridge-process-live-p)
    (let* ((symbols (acm-backend-elisp-get-symbols))
           (symbols-size (length symbols)))
      ;; Only synchronize when new symbol created.
      (unless (equal acm-backend-elisp-symbols-update-size symbols-size)
        (lsp-bridge-call-async "search_list_update"
                               "elisp"
                               symbols
                               acm-backend-elisp-search-max-number)
        (setq acm-backend-elisp-symbols-update-size symbols-size)))))

(defun lsp-bridge-elisp-symbols-search (current-symbol)
  "Search elisp symbols matching CURRENT-SYMBOL.
Skip the search when the buffer is not an elisp buffer, when point is
in a comment, or when CURRENT-SYMBOL is empty; the search runs in the
elisp search backend on the Python side."
  (when (and (acm-is-elisp-mode-p)
             (not (acm-in-comment-p)))
    ;; Search words if current prefix is not empty.
    (unless (or (string-equal current-symbol "") (null current-symbol))
      (lsp-bridge-call-async "search_list_search" "elisp" current-symbol))))

(defun lsp-bridge-search-words-index-files ()
  "Index files when lsp-bridge python process finish."
  (when lsp-bridge-enable-search-words
    (if (lsp-bridge-is-remote-file)
        (let* ((host lsp-bridge-remote-file-host)
               (buffers (cl-remove-if-not (lambda (buf)
                                            (with-current-buffer buf
                                              (and (lsp-bridge-is-remote-file)
                                                   (string-equal lsp-bridge-remote-file-host host))))
                                          (buffer-list)))
               (files (mapcar (lambda (buf)
                                (with-current-buffer buf
                                  lsp-bridge-remote-file-path))
                              buffers)))
          (lsp-bridge-remote-send-func-request "search_file_words_index_files" (list files)))
      (let ((files (cl-remove-if (lambda (elt)
                                   (or (null elt)
                                       (file-remote-p elt)
                                       (member (file-name-extension elt)
                                               lsp-bridge-search-words-prohibit-file-extensions)))
                                 (mapcar (lambda (b) (lsp-bridge-buffer-file-name (buffer-file-name b))) (buffer-list)))))
        (lsp-bridge-call-async "search_file_words_index_files" files)))))

(defun lsp-bridge-search-words-update (begin-pos end-pos change-text)
  "Update the search words backend for a change of CHANGE-TEXT.
BEGIN-POS and END-POS delimit the change in LSP positions.  Remote
files reload their words from the remote server, local files send a
`search_file_words_change_buffer' request for the current buffer."
  (if (lsp-bridge-is-remote-file)
      (progn
        (lsp-bridge-remote-send-func-request "search_file_words_load_file" (list lsp-bridge-remote-file-path t)))
    (when (lsp-bridge-process-live-p)
      (lsp-bridge-call-async "search_file_words_change_buffer"
                             (substring-no-properties (buffer-name))
                             begin-pos
                             end-pos
                             change-text
                             ))))

(defun lsp-bridge-completion-ui-visible-p ()
  "Return non-nil if the acm completion menu frame is visible."
  (acm-frame-visible-p acm-menu-frame))

(defun lsp-bridge-monitor-after-save ()
  "Notify the server that the current buffer was saved.
Runs in `after-save-hook' and sends a `save_file' request with the
buffer name."
  (lsp-bridge-call-file-api "save_file" (buffer-name)))

(defcustom lsp-bridge-find-def-fallback-function nil
  "Fallback for find definition failure."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-find-ref-fallback-function nil
  "Fallback for find referecences failure."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-find-def-select-in-open-windows nil
  "Whether to open definitions in already open windows.
When non-nil, an already open window showing the definition is
selected instead of switching buffers; off by default."
  :type 'boolean
  :group 'lsp-bridge)

(defvar-local lsp-bridge-jump-to-def-in-other-window nil
  "Whether the next definition jump should use another window.")

(defun lsp-bridge-find-def ()
  "Jump to the definition of the symbol at point in the current window.
Elisp buffers use the elisp backend, other buffers ask the language
server or `lsp-bridge-find-def-fallback-function' when no server is
available."
  (interactive)
  (cond
   ((acm-is-elisp-mode-p)
    (lsp-bridge--record-mark-ring)
    (acm-backend-elisp-find-def)
    (lsp-bridge--set-mark-ring-in-new-buffer))
   (t
    (setq-local lsp-bridge-jump-to-def-in-other-window nil)
    (if (lsp-bridge-has-lsp-server-p)
        (lsp-bridge-call-file-api "find_define" (lsp-bridge--position))
      (lsp-bridge-find-def-fallback (lsp-bridge--position))))))

(defun lsp-bridge-find-def-other-window ()
  "Jump to the definition of the symbol at point in another window.
Like `lsp-bridge-find-def' but sets
`lsp-bridge-jump-to-def-in-other-window' for the jump."
  (interactive)
  (cond
   ((acm-is-elisp-mode-p)
    (lsp-bridge--record-mark-ring)
    (acm-backend-elisp-find-def)
    (lsp-bridge--set-mark-ring-in-new-buffer))
   (t
    (setq-local lsp-bridge-jump-to-def-in-other-window t)
    (if (lsp-bridge-has-lsp-server-p)
        (lsp-bridge-call-file-api "find_define" (lsp-bridge--position))
      (lsp-bridge-find-def-fallback (lsp-bridge--position))))))

(defun lsp-bridge-find-def-return ()
  "Pop off lsp-bridge-mark-ring and jump to the top location."
  (interactive)
  ;; Pop entries that refer to non-existent buffers.
  (while (and lsp-bridge-mark-ring (not (marker-buffer (car lsp-bridge-mark-ring))))
    (setq-local lsp-bridge-mark-ring (cdr lsp-bridge-mark-ring)))
  (or lsp-bridge-mark-ring
      (error "[LSP-Bridge] No lsp-bridge mark set"))
  (let* ((this-buffer (current-buffer))
         (marker (pop lsp-bridge-mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "[LSP-Bridge] mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (unless (equal buffer this-buffer)
      (switch-to-buffer buffer))
    (recenter)
    ))

(defun lsp-bridge-find-type-def ()
  "Jump to the type definition of the symbol at point.
Sends a `find_type_define' request for the current position in the
current window."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-file-api "find_type_define" (lsp-bridge--position)))

(defun lsp-bridge-find-type-def-other-window ()
  "Jump to the type definition of the symbol at point in another window.
Like `lsp-bridge-find-type-def' but sets
`lsp-bridge-jump-to-def-in-other-window' for the jump."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-file-api "find_type_define" (lsp-bridge--position)))

(defun lsp-bridge-find-impl ()
  "Jump to the implementation of the symbol at point.
Sends a `find_implementation' request for the current position in the
current window."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window nil)
  (lsp-bridge-call-file-api "find_implementation" (lsp-bridge--position)))

(defun lsp-bridge-find-impl-other-window ()
  "Jump to the implementation of the symbol at point in another window.
Like `lsp-bridge-find-impl' but sets
`lsp-bridge-jump-to-def-in-other-window' for the jump."
  (interactive)
  (setq-local lsp-bridge-jump-to-def-in-other-window t)
  (lsp-bridge-call-file-api "find_implementation" (lsp-bridge--position)))

(defun lsp-bridge-find-references ()
  "List the references of the symbol at point.
Sends a `find_references' request when a language server applies,
otherwise calls `lsp-bridge-find-ref-fallback-function'."
  (interactive)
  (if (lsp-bridge-has-lsp-server-p)
      (lsp-bridge-call-file-api "find_references" (lsp-bridge--position))
    (lsp-bridge-find-ref-fallback (lsp-bridge--position))))

(defun lsp-bridge-find-def-fallback (position)
  "Report that no definition was found at POSITION.
When a peek buffer is open it is closed and its target buffer becomes
current; `lsp-bridge-find-def-fallback-function' is called with
POSITION when it is set."
  (if (not (= (length lsp-bridge-peek-ace-list) 0))
      (progn
	    (if (nth 0 lsp-bridge-peek-ace-list)
	        (kill-buffer (nth 0 lsp-bridge-peek-ace-list)))
	    (switch-to-buffer (nth 2 lsp-bridge-peek-ace-list))
	    (goto-char (nth 1 lsp-bridge-peek-ace-list))))
  (message "[LSP-Bridge] No definition found.")
  (if (functionp lsp-bridge-find-def-fallback-function)
      (funcall lsp-bridge-find-def-fallback-function position)))

(defun lsp-bridge-find-ref-fallback (position)
  "Report that no reference was found at POSITION.
Calls `lsp-bridge-find-ref-fallback-function' with POSITION when it
is set."
  (message "[LSP-Bridge] No references found.")
  (if (functionp lsp-bridge-find-ref-fallback-function)
      (funcall lsp-bridge-find-ref-fallback-function position)))

(defun lsp-bridge-references--popup (references-content references-counter position)
  "Show the references collected for POSITION.
REFERENCES-CONTENT holds the reference entries and
REFERENCES-COUNTER their number; the lsp-bridge ref buffer is opened
when there is at least one reference, otherwise
`lsp-bridge-find-ref-fallback' is called for POSITION."
  (if (> references-counter 0)
      (progn
        (lsp-bridge-ref-popup references-content references-counter)
        (message "[LSP-Bridge] Found %s references" references-counter))
    (lsp-bridge-find-ref-fallback position)))

(defun lsp-bridge-rename ()
  "Rename the symbol at point.
Sends a `prepare_rename' request, asks for the new name with
`read-string' and sends a `rename' request for the current position."
  (interactive)
  (lsp-bridge-call-file-api "prepare_rename" (lsp-bridge--position))
  (let ((new-name (substring-no-properties (read-string "Rename to: " (thing-at-point 'symbol 'no-properties)))))
    (lsp-bridge-call-file-api "rename" (lsp-bridge--position) new-name)))

(defun lsp-bridge-flash-region (start-pos end-pos)
  "Flash the region from START-POS to END-POS.
The region is highlighted with `lsp-bridge-font-lock-flash' for
`lsp-bridge-flash-region-delay' seconds."
  (require 'pulse)
  (let ((pulse-iterations 1)
        (pulse-delay lsp-bridge-flash-region-delay))
    (pulse-momentary-highlight-region start-pos end-pos 'lsp-bridge-font-lock-flash)))

(defun lsp-bridge-flash-line ()
  "Flash the line around point.
The whole current line is passed to `lsp-bridge-flash-region'."
  (lsp-bridge-flash-region
   (save-excursion
     (vertical-motion 0) (point))
   (save-excursion
     (vertical-motion 1) (point))))

(defun lsp-bridge-rename--highlight (filename filehost bound-start bound-end)
  "Flash the renamed region of FILENAME on FILEHOST.
BOUND-START and BOUND-END are LSP positions and are converted to
buffer positions before `lsp-bridge-flash-region' is called."
  (lsp-bridge--with-file-buffer filename filehost
                                (lsp-bridge-flash-region
                                 (acm-backend-lsp-position-to-point bound-start)
                                 (acm-backend-lsp-position-to-point bound-end))))

(defun lsp-bridge-show-documentation ()
  "Show the hover documentation of the symbol at point in its own buffer.
The active region, or point when there is none, selects the range
sent with the `hover' request."
  (interactive)
  (let* ((start (lsp-bridge--point-position (if (region-active-p) (region-beginning) (point))))
         (end (lsp-bridge--point-position (if (region-active-p) (region-end) (point)))))
    (lsp-bridge-call-file-api "hover" start end "show")))

(defun lsp-bridge-popup-documentation ()
  "Popup the hover documentation of the symbol at point.
The active region, or point when there is none, selects the range
sent with the `hover' request."
  (interactive)
  (let* ((start (lsp-bridge--point-position (if (region-active-p) (region-beginning) (point))))
         (end (lsp-bridge--point-position (if (region-active-p) (region-end) (point)))))
    (lsp-bridge-call-file-api "hover" start end "popup")))

(defun lsp-bridge-signature-help-fetch ()
  "Fetch the signature help of the call around point.
The request is skipped when no language server applies and when the
cursor did not move since the previous command; org buffers send the
position of the org src block instead of the buffer position."
  (interactive)
  (when (lsp-bridge-has-lsp-server-p)
    (unless (equal lsp-bridge-cursor-before-command lsp-bridge-cursor-after-command)
      (lsp-bridge-call-file-api "signature_help"
                                (if (and lsp-bridge-enable-org-babel (eq major-mode 'org-mode))
                                    (lsp-bridge--position-in-org)
                                  (lsp-bridge--position))
                                ))))

(defun lsp-bridge-pick-file-path (filename)
  "Return the local file path described by the LSP FILENAME.
A leading `file://' or `:file://' is removed, `%XX' sequences are
decoded and the leading slash before a Windows drive letter is
dropped."
  ;; Remove `file://' and `:file://' prefix.
  (cond ((string-prefix-p "file://" filename)
         (setq filename (string-remove-prefix "file://" filename)))
        ((string-prefix-p ":file://" filename)
         (setq filename (string-remove-prefix ":file://" filename))))

  ;; Convert `%XX' sequences to `:'
  (setq filename (url-unhex-string filename))

  ;; Remove / before drive letter on Windows
  (when (string-match "^/[A-Za-z]:" filename)
    (setq filename (substring filename 1)))

  filename)

(defun lsp-bridge-file-apply-edits (filename edits &optional temp-buffer)
  "Apply EDITS to the file FILENAME.
With TEMP-BUFFER the edits are applied in that buffer, otherwise the
file is visited and the edits applied there.  Completion is prohibited
in the buffer afterwards."
  (if temp-buffer
      ;; Apply edits to temp buffer.
      (with-current-buffer temp-buffer
        (acm-backend-lsp-apply-text-edits edits))

    ;; Pick filename from LSP return file string.
    (setq filename (lsp-bridge-pick-file-path filename))

    (find-file-noselect filename)
    (save-excursion
      (find-file filename)
      (acm-backend-lsp-apply-text-edits edits)))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge--record-mark-ring ()
  "Record the position to come back to before a jump.
Call it in the old buffer before jumping and
`lsp-bridge--set-mark-ring-in-new-buffer' in the new buffer
afterwards; the two functions must be used in pairs."
  ;; Record postion.
  (let ((marker (set-marker (mark-marker) (point) (current-buffer))))
    (setq lsp-bridge-position-before-jump (copy-marker marker)))
  (setq mark-ring lsp-bridge-mark-ring))

(defun lsp-bridge--set-mark-ring-in-new-buffer ()
  "Push the recorded jump position onto `lsp-bridge-mark-ring'.
Must be called in the new buffer after `lsp-bridge--record-mark-ring'
was called in the old one."
  (setq-local lsp-bridge-mark-ring (append (list lsp-bridge-position-before-jump) mark-ring)))

(defun lsp-bridge-define--jump (filename filehost position)
  "Jump to POSITION in FILENAME on FILEHOST.
The current position is recorded on the mark ring first.  Remote hosts
are resolved and the file is opened in an existing window when one
shows it, otherwise it is visited in the current or another window;
remote files are opened asynchronously through the server."
  (let (lsp-bridge-position-before-jump)
    (lsp-bridge--record-mark-ring)

    (if (and (not (string= filehost ""))
             (not lsp-bridge-enable-with-tramp))
        (lsp-bridge-call-async "open_remote_file" (format "%s:%s" filehost filename) position)
      ;; filehost is not empty or lsp-bridge-enable-with-tramp is t
      ;; Resolve IP-to-hostname for ProxyCommand hosts where the remote
      ;; server reports client_address IP instead of the SSH hostname.
      (setq filehost (lsp-bridge--resolve-remote-host filehost))

      (let ((match-window (lsp-bridge--with-file-buffer filename filehost (get-buffer-window))))
        ;; select the window to display definition
        (if match-window
            ;; if match-window is found, avoid using find-file to open the file twice
            (progn
              (cond
               (lsp-bridge-find-def-select-in-open-windows (select-window match-window))
               (lsp-bridge-jump-to-def-in-other-window (select-window match-window))
               (t (switch-to-buffer (window-buffer match-window)))))
          ;; match-window not found, we need to open the file
          (let* ((tramp-file-name (concat (cdr (assoc filehost lsp-bridge-tramp-alias-alist)) filename)))
            (if lsp-bridge-jump-to-def-in-other-window
                (find-file-other-window tramp-file-name)
              (find-file tramp-file-name))))

        ;; Init jump history in new buffer.
        (lsp-bridge--set-mark-ring-in-new-buffer)

        (lsp-bridge-define--jump-flash position)))))

(defun lsp-bridge-define--jump-flash (position)
  "Jump to POSITION and flash the target line.
POSITION is an LSP position; the window is recentered around the
target and `lsp-bridge-flash-line' highlights it."
  ;; We need call `display' before `goto-char',
  ;; otherwise Emacs won't jump to target line if file has opened.
  (redisplay)

  ;; Jump to define postion.
  (goto-char (acm-backend-lsp-position-to-point position))
  (recenter)

  ;; Flash define line.
  (lsp-bridge-flash-line))

(defun lsp-bridge-switch-to-documentation-buffer (norecord)
  "Show the documentation buffer in another window.
NORECORD is passed to `switch-to-buffer-other-window'."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create lsp-bridge-buffer-documentation-buffer) norecord))

(defun lsp-bridge-replace-html-entities ()
  "Replace `&nbsp;' entities with spaces in the current buffer.
The whole buffer is searched from the beginning."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "&nbsp;" nil t)
      (replace-match " "))))

(defun lsp-bridge-show-documentation--callback (value)
  "Render the hover documentation VALUE in the documentation buffer.
The buffer named by `lsp-bridge-buffer-documentation-buffer' is
reused, filled with the markdown rendered VALUE and displayed."
  (let ((buffer (get-buffer-create lsp-bridge-buffer-documentation-buffer)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert value)
      (lsp-bridge-replace-html-entities)
      (setq-local truncate-lines nil)
      (acm-markdown-render-content t)
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer 'display-buffer-reuse-window)))

(defvar lsp-bridge-popup-documentation-frame nil
  "Popup frame used to show hover documentation near point.")

(defun lsp-bridge-popup-documentation-scroll-up (&optional arg)
  "Scroll the hover documentation frame up by ARG lines.
ARG is passed to `scroll-up-command'; the call is ignored when the
documentation frame is not live."
  (interactive)
  (when (frame-live-p lsp-bridge-popup-documentation-frame)
    (with-selected-frame lsp-bridge-popup-documentation-frame
      (apply #'scroll-up-command arg))))

(defun lsp-bridge-popup-documentation-scroll-down (&optional arg)
  "Scroll the hover documentation frame down by ARG lines.
ARG is passed to `scroll-down-command'; the call is ignored when the
documentation frame is not live."
  (interactive)
  (when (frame-live-p lsp-bridge-popup-documentation-frame)
    (with-selected-frame lsp-bridge-popup-documentation-frame
      (apply #'scroll-down-command arg))))

(defun lsp-bridge-popup-documentation--callback (value)
  "Show the hover documentation VALUE in a popup acm frame.
The VALUE is rendered as markdown in
`lsp-bridge-popup-documentation-buffer' and the frame is created next
to the Emacs frame."
  (let ((emacs-frame (or acm-frame--emacs-frame (selected-frame))))
    (with-current-buffer (get-buffer-create lsp-bridge-popup-documentation-buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert value)
      (lsp-bridge-replace-html-entities)
      (setq-local truncate-lines nil)
      (acm-markdown-render-content))

    (acm-frame-new lsp-bridge-popup-documentation-frame
                   lsp-bridge-popup-documentation-buffer
                   "lsp bridge popup documentation frame"
                   (/ (frame-width emacs-frame) 2)
                   (/ (frame-height emacs-frame) 2)
                   )))

(defun lsp-bridge-hide-doc-tooltip ()
  "Hide the hover documentation popup frame."
  (acm-frame-hide-frame lsp-bridge-popup-documentation-frame))

(defcustom lsp-bridge-signature-show-function 'message
  "Function used to render the signature help.
Set it to `lsp-bridge-signature-show-with-frame' to show the
signature in a popup frame instead of the echo area."
  :type 'function
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-show-with-frame-position "bottom-right"
  "Popup position of the signature frame.

Accepted values are `bottom-right' (the default), `top-left',
`top-right', `bottom-left' and `point'."
  :type 'string
  :group 'lsp-bridge)

(defcustom lsp-bridge-signature-buffer " *lsp-bridge-signature*"
  "Buffer for display signature information."
  :type 'string
  :group 'lsp-bridge)

(defvar lsp-bridge-signature-frame nil
  "Popup frame used to show the signature help.")

(defun lsp-bridge-hide-signature-tooltip ()
  "Hide the signature help popup frame."
  (acm-frame-hide-frame lsp-bridge-signature-frame))

(defun lsp-bridge-signature-show-with-frame (str)
  "Use popup frame to show the STR signatureHelp string."
  (if (not (string-empty-p str))
      (progn
        (with-current-buffer (get-buffer-create lsp-bridge-signature-buffer)
          (erase-buffer)
          (insert str)
          (visual-line-mode 1)
          (current-buffer))

        (acm-frame-new lsp-bridge-signature-frame
                       lsp-bridge-signature-buffer
                       "lsp bridge signature frame"
                       nil
                       nil
                       lsp-bridge-signature-show-with-frame-position
                       ))
    (lsp-bridge-hide-signature-tooltip)))

(defun lsp-bridge-signature-help--update (help-infos help-index)
  "Render the signature help built from HELP-INFOS.
HELP-INFOS is the list of signature strings and HELP-INDEX marks the
active one, which is highlighted; the result is shown through
`lsp-bridge-signature-show-function' and empty help is not shown."
  (let ((index 0)
        (help ""))
    (dolist (help-info help-infos)
      (setq help (concat help
                         (propertize help-info 'face (if (equal index help-index) 'font-lock-function-name-face 'default))
                         (if (equal index (1- (length help-infos))) "" ", ")))
      (setq index (1+ index)))

    (unless (string-equal help "")
      (let ((message-log-max nil))
        (funcall lsp-bridge-signature-show-function help)))))

(defvar lsp-bridge--last-buffer nil
  "Buffer that was current during the previous window change check.")

(defvar-local lsp-bridge--work-done-progress nil
  "Buffer local LSP work-done progress message for the mode line.")

(defvar-local lsp-bridge--work-done-progress-timer nil
  "Buffer-local Timer to clear `lsp-bridge--work-done-progress' after inactivity.")

(defun lsp-bridge-monitor-window-buffer-change ()
  "Hide the lsp-bridge popups when the current buffer changed.
Runs in `post-command-hook': it hides the documentation, diagnostic
and signature popups and records the new current buffer unless the
minibuffer or the `*Messages*' buffer is current."
  ;; Hide completion, diagnostic and signature frame when buffer or window changed.
  (unless (eq (current-buffer)
              lsp-bridge--last-buffer)
    (lsp-bridge-hide-doc-tooltip)
    (lsp-bridge-diagnostic-hide-tooltip)
    (lsp-bridge-hide-signature-tooltip))

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq lsp-bridge--last-buffer (current-buffer))))

(add-hook 'post-command-hook 'lsp-bridge-monitor-window-buffer-change)

(defun lsp-bridge-enable-in-minibuffer ()
  "Enable `lsp-bridge-mode' in a minibuffer that completes.
Runs in `minibuffer-setup-hook' when
`lsp-bridge-enable-completion-in-minibuffer' is non-nil and the local
map provides a `completion-at-point' binding."
  (when (and lsp-bridge-enable-completion-in-minibuffer
             (where-is-internal #'completion-at-point (list (current-local-map))))
    (lsp-bridge-mode 1)
    ))

(add-hook 'minibuffer-setup-hook #'lsp-bridge-enable-in-minibuffer)

(defconst lsp-bridge--internal-hooks
  '((before-change-functions lsp-bridge-monitor-before-change nil t)
    (after-change-functions lsp-bridge-monitor-after-change nil t)
    (pre-command-hook lsp-bridge-monitor-pre-command nil t)
    (post-command-hook lsp-bridge-monitor-post-command nil t)
    (after-save-hook lsp-bridge-monitor-after-save nil t)
    (kill-buffer-hook lsp-bridge-close-buffer-file nil t)
    (before-revert-hook lsp-bridge-close-buffer-file nil t)
    (post-self-insert-hook lsp-bridge-monitor-post-self-insert 90 t)
    )
  "Hooks added buffer-locally while lsp-bridge mode is enabled.
Each element is a hook list in the form accepted by `add-hook'.")

(defvar lsp-bridge-mode-map (make-sparse-keymap)
  "Keymap used by `lsp-bridge-mode'.")

(defvar lsp-bridge-signature-help-timer nil
  "Idle timer that triggers `lsp-bridge-signature-help-fetch'.")

(defvar lsp-bridge-search-words-timer nil
  "Idle timer that updates the search words backend.")

(defvar lsp-bridge-auto-format-code-timer nil
  "Idle timer that triggers `lsp-bridge-auto-format-code'.")

(defcustom lsp-bridge-mode-lighter " LSPB"
  "Mode line lighter for LSP Bridge Mode."
  :type 'string
  :group 'lsp-bridge)

;;;###autoload
(define-minor-mode lsp-bridge-mode
  "LSP Bridge mode."
  :keymap lsp-bridge-mode-map
  :lighter lsp-bridge-mode-lighter
  :init-value nil
  (if lsp-bridge-mode
      (lsp-bridge--enable)
    (lsp-bridge--disable)))

(defun lsp-bridge--enable ()
  "Enable LSP Bridge mode."

  (add-hook 'post-command-hook #'lsp-bridge-start-process)

  ;; Don't enable lsp-bridge when current buffer is acm buffer.
  (unless (or (equal (buffer-name (current-buffer)) acm-buffer)
              (equal (buffer-name (current-buffer)) acm-doc-buffer))
    ;; Disable backup file.
    ;; Please use my another plugin `https://github.com/manateelazycat/auto-save' and use git for file version management.
    (when lsp-bridge-disable-backup
      (setq make-backup-files nil)
      (setq auto-save-default nil)
      (setq create-lockfiles nil))

    ;; Add `lsp-bridge-symbols--current-defun' to `whic-func-functions'.
    (if (and lsp-bridge-symbols-enable-which-func
             (featurep 'which-func) which-function-mode)
        (setq-local which-func-functions
                    (add-to-list 'which-func-functions #'lsp-bridge-symbols--current-defun)))

    (setq-local lsp-bridge-revert-buffer-flag nil)

    (acm-run-idle-func acm-backend-elisp-symbols-update-timer lsp-bridge-elisp-symbols-update-idle 'lsp-bridge-elisp-symbols-update)

    (when (or (lsp-bridge-has-lsp-server-p)
              ;; Init acm backend for org babel, and need elimination org temp buffer.
              (and lsp-bridge-enable-org-babel
                   (eq major-mode 'org-mode)
                   (not (lsp-bridge-is-org-temp-buffer-p))))
      ;; When user open buffer by `ido-find-file', lsp-bridge will throw `FileNotFoundError' error.
      ;; So we need save buffer to disk before enable `lsp-bridge-mode'.
      (when (not (lsp-bridge-is-remote-file))
        (when (and (buffer-file-name)
                   (not (file-exists-p (buffer-file-name))))
          (save-buffer)))

      ;; Init backends.
      (acm-backend-lsp-init)
      (acm-backend-lsp-workspace-symbol-init)

      (when lsp-bridge-enable-signature-help
        (acm-run-idle-func lsp-bridge-signature-help-timer lsp-bridge-signature-help-fetch-idle 'lsp-bridge-signature-help-fetch))
      (when lsp-bridge-enable-auto-format-code
        (acm-run-idle-func lsp-bridge-auto-format-code-timer lsp-bridge-auto-format-code-idle 'lsp-bridge-auto-format-code))

      (when lsp-bridge-enable-document-highlight
        (lsp-bridge-enable-document-highlight-timer)
        (lsp-bridge-document-highlight-setup)))

    (dolist (hook lsp-bridge--internal-hooks)
      (apply #'add-hook hook))

    (advice-add #'acm-hide :after #'lsp-bridge--completion-hide-advisor)))

(defun lsp-bridge-is-org-temp-buffer-p ()
  "Return non-nil if the current buffer is an org temporary buffer.
Such buffers are created by org fontification, org src editing or
capture and must not start lsp-bridge."
  (or (string-match "\*org-src-fontification:" (buffer-name))
      (string-match "\*Org Src" (buffer-name))
      (string-match ".org-src-babel" (buffer-name))
      (equal "*Capture*" (buffer-name))))

(defun lsp-bridge--disable ()
  "Disable LSP Bridge mode."
  ;; Remove hooks.
  (dolist (hook lsp-bridge--internal-hooks)
    (remove-hook (nth 0 hook) (nth 1 hook) (nth 3 hook)))

  ;; Cancel idle timer.
  (acm-cancel-timer lsp-bridge-signature-help-timer)
  (acm-cancel-timer lsp-bridge-search-words-timer)
  (acm-cancel-timer lsp-bridge-auto-format-code-timer)
  (acm-cancel-timer acm-backend-elisp-symbols-update-timer)

  ;; Reset `acm-backend-elisp-symbols-update-size' to zero.
  (setq acm-backend-elisp-symbols-update-size 0)

  ;; Remove hide advice.
  (advice-remove #'acm-hide #'lsp-bridge--completion-hide-advisor))

(defun lsp-bridge--turn-off-lsp-feature (filename filehost)
  "Mark the buffer of FILENAME on FILEHOST as having no LSP server.
Sets its buffer-local `acm-backend-lsp-server-command-exist' to nil
so that no further file request is sent."
  (lsp-bridge--with-file-buffer filename filehost
                                (setq-local acm-backend-lsp-server-command-exist nil)
                                ))

(defcustom lsp-bridge-workspace-symbol-kind-to-face
  [("    " . nil)                          ; Unknown - 0
   ("File" . font-lock-builtin-face)       ; File - 1
   ("Modu" . font-lock-keyword-face)       ; Module - 2
   ("Nmsp" . font-lock-keyword-face)       ; Namespace - 3
   ("Pack" . font-lock-keyword-face)       ; Package - 4
   ("Clss" . font-lock-type-face)          ; Class - 5
   ("Meth" . font-lock-function-name-face) ; Method - 6
   ("Prop" . font-lock-constant-face)      ; Property - 7
   ("Fld " . font-lock-constant-face)      ; Field - 8
   ("Cons" . font-lock-function-name-face) ; Constructor - 9
   ("Enum" . font-lock-type-face)          ; Enum - 10
   ("Intf" . font-lock-type-face)          ; Interface - 11
   ("Func" . font-lock-function-name-face) ; Function - 12
   ("Var " . font-lock-variable-name-face) ; Variable - 13
   ("Cnst" . font-lock-constant-face)      ; Constant - 14
   ("Str " . font-lock-string-face)        ; String - 15
   ("Num " . font-lock-builtin-face)       ; Number - 16
   ("Bool " . font-lock-builtin-face)      ; Boolean - 17
   ("Arr " . font-lock-builtin-face)       ; Array - 18
   ("Obj " . font-lock-builtin-face)       ; Object - 19
   ("Key " . font-lock-constant-face)      ; Key - 20
   ("Null" . font-lock-builtin-face)       ; Null - 21
   ("EmMm" . font-lock-constant-face)      ; EnumMember - 22
   ("Srct" . font-lock-type-face)          ; Struct - 23
   ("Evnt" . font-lock-builtin-face)       ; Event - 24
   ("Op  " . font-lock-function-name-face) ; Operator - 25
   ("TPar" . font-lock-type-face)]         ; TypeParameter - 26
  "Mapping between eacho of LSP's SymbolKind and a face.
A vector of 26 cons cells, where the ith cons cell contains
the string representation and face to use for the i+1th
SymbolKind (defined in the LSP)."
  :group 'lsp-bridge
  :type '(vector
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)
          (cons string face)))

(defun lsp-bridge-workspace-list-symbol-at-point ()
  "List workspace symbols matching the symbol or region at point.
The active region text is used as query when there is one, otherwise
the symbol at point is searched."
  (interactive)
  (if (region-active-p)
      (lsp-bridge-workspace-list-symbols (buffer-substring-no-properties (mark) (point)))
    (lsp-bridge-workspace-list-symbols (substring-no-properties (symbol-name (symbol-at-point))))))

(defun lsp-bridge-workspace-list-symbols (query)
  "Ask the server for workspace symbols matching QUERY.
QUERY is read from the minibuffer when the command is called
interactively; the result arrives in
`lsp-bridge-workspace--list-symbols'."
  (interactive "sWorkspace symbol query: ")
  (lsp-bridge-call-file-api "workspace_symbol" query))

(defun lsp-bridge-workspace--list-symbols (info)
  "Let the user pick one of the workspace symbols in INFO.
INFO is the list of symbol plists from the server; the chosen symbol
is opened at its location through `lsp-bridge-jump-to-file', and a
message is shown when the server returned no symbol."
  (if (zerop (length info))
      (message "LSP server did not return any symbols.")
    (let* ((symbols (mapcar #'lsp-bridge-workspace-transform-info info))
           (match-symbol (completing-read "Workspace symbol: " symbols))
           (match-info (seq-filter (lambda (i) (string-equal match-symbol (lsp-bridge-workspace-transform-info i))) info)))
      (when match-info
        (let* ((symbol-info (plist-get (car match-info) :location))
               (symbol-file (lsp-bridge-pick-file-path (format "%s" (plist-get symbol-info :uri))))
               (symbol-position (plist-get (plist-get symbol-info :range) :start)))
          (lsp-bridge-jump-to-file symbol-file symbol-position)
          )))))

(defun lsp-bridge-jump-to-file (file position)
  "Open FILE and move to POSITION.
Virtual schemes such as jdt, deno and csharp metadata are resolved by
the server first; ordinary files are visited directly and POSITION is
converted to a buffer position."
  (cond ((string-prefix-p "jdt://" file)
         (lsp-bridge-call-file-api "jdt_uri_resolver" (url-encode-url file) position "lsp-bridge-define--jump"))
        ((string-prefix-p "deno:" file)
         (lsp-bridge-call-file-api "deno_uri_resolver" (url-encode-url file) position "lsp-bridge-define--jump"))
        ((string-prefix-p "csharp:/metadata/" file)
         (lsp-bridge-call-file-api "csharp_uri_resolver" (url-encode-url file) position "lsp-bridge-define--jump"))
        (t
         (find-file file)
         (goto-char (acm-backend-lsp-position-to-point position)))))

(defun lsp-bridge-workspace-transform-info (info)
  "Return a completion string for the workspace symbol INFO.
The string is prefixed with the colored symbol kind and suffixed with
the file path of the symbol relative to the project root."
  (let* ((name (plist-get info :name))
         (uri (plist-get (plist-get info :location) :uri))
         (kind (plist-get info :kind))
         (sanitized-kind (if (< kind (length lsp-bridge-workspace-symbol-kind-to-face)) kind 0))
         (type (elt lsp-bridge-workspace-symbol-kind-to-face sanitized-kind))
         (typestr (propertize (format "[%s] " (car type)) 'face (cdr type)))
         (project-root (locate-dominating-file default-directory ".git"))
         (pathstr (propertize (format " · %s" (file-relative-name (substring uri 7 nil) project-root))
                              'face font-lock-comment-face)))
    (concat typestr name pathstr)))

(defun lsp-bridge-auto-format-code ()
  "Format the current buffer when the idle timer fires.
Formatting is skipped while the mouse wheel scrolls."
  (unless (eq last-command 'mwheel-scroll)
    (lsp-bridge-code-format)))

(defun lsp-bridge-code-format ()
  "Ask the server to format the region or the current line.
The request is skipped unless a language server applies, the
completion menu is hidden and neither yasnippet nor tempel has an
active snippet; the major mode indentation width is sent along."
  (interactive)
  (when (and
         ;; Current buffer has LSP server.
         (lsp-bridge-has-lsp-server-p)
         ;; Completion menu not show.
         (not (lsp-bridge-completion-ui-visible-p))
         ;; Yasnippet not active.
         (or (not (boundp 'yas--active-snippets))
             (not yas--active-snippets))
         ;; Tempel not active.
         (or (not (boundp 'tempel--active))
             (not tempel--active)))
    (let* ((indent (symbol-value (lsp-bridge--get-indent-width major-mode)))
           (start (lsp-bridge--point-position (if (region-active-p) (region-beginning) (point))))
           (end (lsp-bridge--point-position (if (region-active-p) (region-end) (point)))))
      (lsp-bridge-call-file-api "try_formatting"
                                start
                                end
                                ;; Sometimes `c-basic-offset' return string `set-from-style', make some lsp server broken, such as, gopls,
                                ;; so we need convert indent to integer `4' to make sure code format works expectantly.
                                (if (eq indent 'set-from-style)
                                    4
                                  indent)))))

(defun lsp-bridge-format--update (filename edits)
  "Apply the formatting EDITS to FILENAME.
The edits are applied with modification hooks inhibited and the
original position is restored; remote files are uploaded again and the
server is told that the whole file changed."
  ;; We need set `inhibit-modification-hooks' to t to avoid GC freeze Emacs.
  (lsp-bridge-save-position
   (let ((inhibit-modification-hooks t))
     ;; Apply code format edits, not sort, just reverse order.
     (lsp-bridge-file-apply-edits filename edits)
     (when (lsp-bridge-is-remote-file)
       (lsp-bridge-call-async "update_remote_file"
                              lsp-bridge-remote-file-host
                              lsp-bridge-remote-file-path
                              (buffer-substring-no-properties (point-min) (point-max))))
     ;; Make LSP server update full content.
     (lsp-bridge-call-file-api "update_file" (buffer-name))
     ;; Notify format complete.
     (message "[LSP-BRIDGE] Complete code formatting.")
     )))

(defun lsp-bridge-workspace-apply-edit (info &optional temp-buffer)
  "Apply the workspace edit described by INFO.
Both the `documentChanges' and the `changes' shape are accepted; with
TEMP-BUFFER the edits go to that buffer.  Completion is prohibited in
the buffer afterwards and the original position is restored."
  (lsp-bridge-save-position
   (cond ((plist-get info :documentChanges)
          (dolist (change (plist-get info :documentChanges))
            (lsp-bridge-file-apply-edits
             (plist-get (plist-get change :textDocument) :uri)
             (plist-get change :edits)
             temp-buffer)))
         ((plist-get info :changes)
          (let* ((changes (plist-get info :changes))
                 (changes-number (/ (length changes) 2)))
            (dotimes (changes-index changes-number)
              (lsp-bridge-file-apply-edits
               (format "%s" (nth (* changes-index 2) changes))
               (nth (+ (* changes-index 2) 1) changes) temp-buffer))))))

  (setq-local lsp-bridge-prohibit-completion t))

(defun lsp-bridge-completion-item--update (info filehost)
  "Apply the resolved completion item INFO for FILEHOST.
INFO carries the file path, item key, server name, documentation and
additional text edits of the item; the cached item of the buffer is
updated and the documentation frame is shown or hidden accordingly."
  (let* ((filename (plist-get info :filepath))
         (key (plist-get info :key))
         (server-name (plist-get info :server))
         (additional-text-edits (plist-get info :additionalTextEdits))
         (documentation (plist-get info :documentation))
         (has-doc-p (and documentation
                         (not (string-equal documentation "")))))
    (lsp-bridge--with-file-buffer
        filename filehost
        ;; When lsp-bridge running in server, `acm-backend-lsp-items' maybe nil when receive `lsp-bridge-completion-item--update' response.
        ;; So we need check `acm-backend-lsp-items' value before update item documentation.
        (when-let* ((server-lsp-items (gethash server-name acm-backend-lsp-items)))
          ;; Update `documentation' and `additionalTextEdits'
          (when-let* ((item (gethash key server-lsp-items)))
            (when additional-text-edits
              (plist-put item :additionalTextEdits additional-text-edits))

            (when has-doc-p
              (plist-put item :documentation documentation))

            (puthash key item (gethash server-name acm-backend-lsp-items)))

          (if has-doc-p
              ;; Show doc frame if `documentation' exist and not empty.
              (acm-doc-try-show t)
            ;; Hide doc frame immediately.
            (acm-doc-hide))
          ))))

(defun lsp-bridge--not-acm-doc-markdown-buffer ()
  "Return non-nil when the current buffer is not an acm doc temp buffer.
The `markdown-code-fontification:' buffers created while rendering acm
documentation are excluded from the change monitors."
  (not (string-prefix-p " markdown-code-fontification:" (buffer-name (current-buffer)))))

(defun lsp-bridge--not-mind-wave-chat-buffer ()
  "Not mind-wave's `*.chat' buffer."
  (or
   (not (buffer-file-name))
   (not (string-equal (file-name-extension (buffer-file-name)) "chat"))))

(defun lsp-bridge--not-zsh-buffer ()
  "Check if the file extension matches .zsh, .zshenv, or .zshrc."
  (not (ignore-errors
         (or (string-equal (file-name-extension (buffer-file-name)) "zsh")
             (member (buffer-name) '(".zshenv" ".zshrc"))
             ))))

;;;###autoload
(defun global-lsp-bridge-mode ()
  "Enable `lsp-bridge-mode' in every configured major mode hook.
The hooks listed in `lsp-bridge-default-mode-hooks' and
`acm-backend-capf-mode-hooks' get a function that enables the mode
only when all predicates in `lsp-bridge-enable-predicates' allow it."
  (interactive)

  (dolist (hook (append
                 lsp-bridge-default-mode-hooks
                 acm-backend-capf-mode-hooks))
    (add-hook hook (lambda ()
                     (when (cl-every (lambda (pred)
                                       (lsp-bridge-check-predicate pred "global-lsp-bridge-mode"))
                                     lsp-bridge-enable-predicates)
                       (lsp-bridge-mode 1))
                     ))))

(with-eval-after-load 'evil
  (evil-add-command-properties #'lsp-bridge-find-def :jump t)
  (evil-add-command-properties #'lsp-bridge-find-references :jump t)
  (evil-add-command-properties #'lsp-bridge-find-impl :jump t)

  ;; Fix issue #516
  (add-hook 'acm-mode-hook #'evil-normalize-keymaps))

(defun lsp-bridge--rename-file-advisor (orig-fun &optional arg &rest args)
  "Tell the server about a rename before `rename-file' runs.
ORIG-FUN, ARG and ARGS describe the advised call.  When the renamed
file is the file of this buffer a `rename_file' request is sent, the
old file is closed and the buffer file name is updated."
  (let* ((current-file-name (buffer-file-name))
         (current-file-name (and current-file-name
                                 (expand-file-name current-file-name)))
         (new-name (expand-file-name (nth 0 args))))
    (when (and lsp-bridge-mode
               (boundp 'acm-backend-lsp-filepath)
               current-file-name
               (string-equal current-file-name new-name))
      (lsp-bridge-call-file-api "rename_file" new-name)
      (if (lsp-bridge-is-remote-file)
          (lsp-bridge-remote-send-func-request "close_file" (list acm-backend-lsp-filepath))
        (lsp-bridge-call-async "close_file" acm-backend-lsp-filepath))
      (set-visited-file-name new-name t t)
      (setq-local acm-backend-lsp-filepath new-name)))
  (apply orig-fun arg args))
(advice-add #'rename-file :around #'lsp-bridge--rename-file-advisor)

(defun lsp-bridge--cleanup ()
  "Remove the lsp-bridge overlays before the buffer changes.
Hides the diagnostic and inlay hint overlays."
  ;; We need clean diagnostic overlays before revert buffer.
  (lsp-bridge-diagnostic-hide-overlays)

  ;; Hide inlay hint overlays before revert buffer.
  (lsp-bridge-inlay-hint-hide-overlays))

;; We use `lsp-bridge-revert-buffer-flag' var avoid lsp-bridge send change_file request while execute `revert-buffer' command.
(defun lsp-bridge--revert-buffer-advisor (orig-fun &optional arg &rest args)
  "Clean up lsp-bridge overlays around a buffer revert.
ORIG-FUN, ARG and ARGS describe the advised call; the
`lsp-bridge-revert-buffer-flag' buffer-local variable is set while
the revert runs so that no `change_file' request is sent for it."
  ;; Clean up.
  (lsp-bridge--cleanup)

  (setq-local lsp-bridge-revert-buffer-flag t)
  (apply orig-fun arg args)
  (setq-local lsp-bridge-revert-buffer-flag nil))
(advice-add #'revert-buffer :around #'lsp-bridge--revert-buffer-advisor)

(defun lsp-bridge--completion-hide-advisor (&rest args)
  "Reset the resolved completion item ticker when the menu hides.
ARGS are ignored; runs as :after advice on `acm-hide' so the next menu
resolves its items again."
  (when lsp-bridge-mode
    ;; Clean LSP backend completion tick.
    (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)))

(defun lsp-bridge-copilot-complete ()
  "Ask the copilot backend to complete at point.
The whole buffer text, the major mode, the file path and the relative
path are sent to the server, which returns the completion candidates."
  (interactive)
  (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)
  (let ((all-text (buffer-substring-no-properties (point-min) (point-max)))
        (relative-path
         ;; from copilot.el
         (cond
          ((not buffer-file-name)
           "")
          ((fboundp 'projectile-project-root)
           (file-relative-name
            (lsp-bridge-get-buffer-file-name-text)
            (projectile-project-root)))
          ((boundp 'vc-root-dir)
           (file-relative-name
            (lsp-bridge-get-buffer-file-name-text)
            (vc-root-dir)))
          (t
           (file-name-nondirectory buffer-file-name)))))
    (if (lsp-bridge-is-remote-file)
        (lsp-bridge-remote-send-func-request "copilot_complete"
                                             (list
                                              (lsp-bridge--position)
                                              (symbol-name major-mode)
                                              (buffer-file-name)
                                              relative-path
                                              tab-width
                                              all-text
                                              (not indent-tabs-mode)))
      (lsp-bridge-call-async "copilot_complete"
                             (lsp-bridge--position)
                             (symbol-name major-mode)
                             (buffer-file-name)
                             relative-path
                             tab-width
                             all-text
                             (not indent-tabs-mode)))))

(defun lsp-bridge-search-backend--record-items (backend-name items)
  "Store ITEMS as the candidates of BACKEND-NAME.
Both the candidates and their cache are saved in buffer-local
variables named after BACKEND-NAME and the completion menu is tried."
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC
  (let ((gc-cons-threshold most-positive-fixnum))
    (set (make-local-variable (intern (format "acm-backend-%s-items" backend-name))) items)
    (set (make-local-variable (intern (format "acm-backend-%s-cache-candiates" backend-name))) nil)

    (lsp-bridge-try-completion)))

;;; support which-func-mode
;;;

(defvar-local lsp-bridge-symbols-current-defun nil
  "Symbol of the function around point, as reported by the server.")

(defun lsp-bridge-symbols--record-current-defun (current-defun)
  "Record `CURRENT-DEFUN' from lsp."
  (setq-local lsp-bridge-symbols-current-defun current-defun))

(defun lsp-bridge-symbols--current-defun ()
  "Add `lsp-bridge-symbols-current-defun' to `which-func-functions'."
  lsp-bridge-symbols-current-defun)


(defun lsp-bridge--record-work-done-progress (progress file-paths)
  "Show PROGRESS for the buffers visiting FILE-PATHS.
PROGRESS is echoed when `acm-backend-lsp-show-progress' is non-nil and
is stored in the buffer-local `lsp-bridge--work-done-progress' for
the mode line, with a timer that clears it after a while."
  ;; Optional message output, controlled by acm-backend-lsp-show-progress.
  (when acm-backend-lsp-show-progress
    (unless (active-minibuffer-window)
      (message progress)))
  ;; Update mode-line progress indicator.
  (dolist (filepath file-paths)
    (when-let* ((buf (lsp-bridge-get-match-buffer-by-filepath filepath)))
      (with-current-buffer buf
        (when lsp-bridge--work-done-progress-timer
          (cancel-timer lsp-bridge--work-done-progress-timer)
          (setq-local lsp-bridge--work-done-progress-timer nil))
        (setq-local lsp-bridge--work-done-progress
                    (if (string-equal progress "")
                        nil
                      (lsp-bridge--format-progress-message progress)))
        (unless (string-equal progress "")
          (let ((buf buf))
            (setq-local lsp-bridge--work-done-progress-timer
                        (run-with-timer 1 nil
                                        (lambda ()
                                          (when (buffer-live-p buf)
                                            (with-current-buffer buf
                                              (setq lsp-bridge--work-done-progress nil)
                                              (force-mode-line-update t))))))))
        (force-mode-line-update)))))

(defun lsp-bridge--format-progress-message (progress)
  "Return the mode line text for the work done PROGRESS message."
  (let* (;; Extract percentage like (96%%)
         (percent-match (string-match "\\(([0-9]+%%)\\)" progress))
         (percent (if percent-match
                      (replace-regexp-in-string "[()]" "" (match-string 1 progress))
                    ""))
         ;; Extract stage name: strip rustAnalyzer/ prefix, take word before space/paren
         (stage (when (string-match "\\(?:rustAnalyzer/\\)?\\([A-Za-z ]+?\\)\\(?:[[:space:]]*(\\|[[:space:]]+[0-9]\\|$\\)" progress)
                  (string-trim (match-string 1 progress))))
         (icon (when (fboundp 'nerd-icons-codicon)
                 (nerd-icons-codicon "nf-cod-sync"))))
    (cond
     ;; Both stage and percent
     ((and stage (not (string-blank-p percent)))
      (format "%s %s %s" (or icon "") stage percent))
     ;; Only stage
     (stage
      (format "%s %s" (or icon "") stage))
     ;; Fallback
     (t ""))))

;;; Mode-line
;;;

(defface lsp-bridge-alive-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for activity in LSP-bridge's mode line.")

(defface lsp-bridge-kill-mode-line
  '((t (:inherit font-lock-comment-face :weight bold)))
  "Face for kill process in LSP-bridge's mode line.")

(defvar lsp-bridge--mode-line-format `(:eval (lsp-bridge--mode-line-format))
  "Mode line construct that shows the lsp-bridge server state.")

(put 'lsp-bridge--mode-line-format 'risky-local-variable t)

(defun lsp-bridge--mode-line-format ()
  "Compose the LSP-bridge's mode-line."
  (setq-local mode-face
              (if (lsp-bridge-process-live-p)
                  'lsp-bridge-alive-mode-line
                'lsp-bridge-kill-mode-line))

  (when lsp-bridge-server
    (concat (propertize "lsp-bridge"'face mode-face)
            (when lsp-bridge--work-done-progress
              (propertize (concat " " lsp-bridge--work-done-progress)
                          'face 'lsp-bridge-alive-mode-line)))
    ))

(when lsp-bridge-enable-mode-line
  (add-to-list 'mode-line-misc-info
               `(lsp-bridge-mode ("" lsp-bridge--mode-line-format " "))))

(defvar-local lsp-bridge-remote-file-flag nil
  "Non-nil means this buffer was opened for a remote file.")

(defvar-local lsp-bridge-remote-file-tramp-method nil
  "Tramp method of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-user nil
  "Remote user of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-host nil
  "Remote host of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-port nil
  "Remote port of the remote file of this buffer.")

(defvar-local lsp-bridge-remote-file-path nil
  "Path of the remote file of this buffer on the remote host.")
;; TODO need to consider tramp docker file pattern
(defvar lsp-bridge-remote-file-pattern
  (rx bos (? "/")
      ;; username
      (? (seq (any "a-z_") (* (any "a-z0-9_.-"))) "@")
      ;; host ip
      (repeat 3 (seq (repeat 1 3 (any "0-9")) ".")) (repeat 1 3 (any "0-9"))
      ;; ssh port
      (? ":" (+ digit))
      ;; path
      (? ":") (? "~") (* nonl) eol
      )
  "Regexp matching the `host:path' text of a remote file request.
Used to detect that a minibuffer input names a remote file; the
TODO notes that tramp docker patterns are not covered yet.")

(defun lsp-bridge-open-or-create-file (filepath)
  "Open FILEPATH, if it exists.  If not, create it and its parents."
  (if (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (buffer-string))
    (make-directory (file-name-directory filepath) t)
    (with-temp-file filepath)
    ""))

(defun lsp-bridge-open-remote-file ()
  "Ask for a remote `ip:path' file and open it on the server.
Recent entries are offered from the ip.txt file in the user Emacs
directory, which is created when missing."
  (interactive)
  (let* ((ip-file (concat (lsp-bridge--user-emacs-directory-func)
                          (file-name-as-directory "lsp-bridge")
                          (file-name-as-directory "remote_file")
                          "ip.txt"))
         (path (completing-read "Open remote file (ip:path): "
                                (with-temp-buffer
                                  (insert (lsp-bridge-open-or-create-file ip-file))
                                  (split-string (buffer-string) "\n" t)))))
    (lsp-bridge-call-async "open_remote_file" path (list :line 0 :character 0))))

(cl-defmacro lsp-bridge--conditional-update-tramp-file-info (tramp-file-name path host &rest body)
  "Conditionally execute BODY with the buffer associated with TRAMP-FILE-NAME.

If the buffer is created by TRAMP with TRAMP-FILE-NAME, BODY is executed within
the context of that buffer.

If the buffer is created by `lsp-bridge-open-remote-file--response',
`lsp-bridge--with-file-buffer' is used with PATH and HOST to find the buffer,
then BODY is executed within that buffer."

  `(if (get-file-buffer ,tramp-file-name)
       (with-current-buffer (get-file-buffer ,tramp-file-name)
         ,@body)
     ;; lsp-bridge--with-file-buffer is another macro, carefully expand it
     ,(macroexpand `(lsp-bridge--with-file-buffer ,path ,host ,@body))))

(defun lsp-bridge-update-tramp-file-info (tramp-file-name tramp-connection-info tramp-method user host path)
  "Store the tramp connection info of TRAMP-FILE-NAME and its buffer.
TRAMP-CONNECTION-INFO, TRAMP-METHOD, USER, HOST and PATH describe the
connection and the file; both alias alists are updated and the buffer
of the file is marked as a remote file."
  (unless (assoc host lsp-bridge-tramp-alias-alist)
    (push `(,host . ,tramp-connection-info) lsp-bridge-tramp-alias-alist))

  (unless (assoc tramp-connection-info lsp-bridge-tramp-connection-info)
    (push `(,tramp-connection-info . ,host) lsp-bridge-tramp-connection-info))

  (lsp-bridge--conditional-update-tramp-file-info tramp-file-name path host
                                                  (setq-local lsp-bridge-remote-file-flag t)
                                                  (setq-local lsp-bridge-remote-file-tramp-method tramp-method)
                                                  (setq-local lsp-bridge-remote-file-user user)
                                                  (setq-local lsp-bridge-remote-file-host host)
                                                  (setq-local lsp-bridge-remote-file-path path)

                                                  (read-only-mode -1)

                                                  (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)
                                                  (setq lsp-bridge-tramp-sync-var t)
                                                  (message "[LSP-Bridge] remote file %s updated info successfully."
                                                           (buffer-file-name))))

(defun lsp-bridge-tramp-show-hostnames ()
  "Ask the server to message the host names it knows about."
  (interactive)
  (lsp-bridge-call-async "message_hostnames"))


(defun lsp-bridge-sync-tramp-remote (force)
  "Sync the tramp file of the current buffer with the remote server.
FORCE non-nil opens a fresh connection, otherwise a cached connection
is reused when one exists; sudo and blacklisted hosts are skipped."
  (interactive)
  (let* ((file-name (lsp-bridge-get-buffer-file-name-text))
         (tramp-vec (tramp-dissect-file-name file-name))
         (tramp-method (tramp-file-name-method tramp-vec))
         (user (tramp-file-name-user tramp-vec))
         (host (tramp-file-name-host tramp-vec))
         (port (tramp-file-name-port tramp-vec))
         (path (tramp-file-name-localname tramp-vec))
         (tramp-connection-info (substring file-name 0 (+ 1 (string-match ":" file-name (+ 1 (string-match ":" file-name))))))
         (connected-host (cdr (assoc tramp-connection-info lsp-bridge-tramp-connection-info))))

    (if (or force (not connected-host))
        (when (and (not (member tramp-method '("sudo" "sudoedit" "su" "doas")))
                   (not (member host lsp-bridge-tramp-blacklist)))
          (read-only-mode 1)
          (lsp-bridge-call-async "sync_tramp_remote" file-name tramp-method user host port path))

      (lsp-bridge--conditional-update-tramp-file-info file-name path connected-host
                                                      (setq-local lsp-bridge-remote-file-flag t)
                                                      (setq-local lsp-bridge-remote-file-tramp-method tramp-method)
                                                      (setq-local lsp-bridge-remote-file-user user)
                                                      (setq-local lsp-bridge-remote-file-host connected-host)
                                                      (setq-local lsp-bridge-remote-file-path path)

                                                      (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)
                                                      (setq lsp-bridge-tramp-sync-var t)))))

(defun lsp-bridge-get-match-buffer-by-filehost (remote-file-host)
  "Return the first buffer whose remote file host is REMOTE-FILE-HOST."
  (cl-dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((match-buffer (string-equal lsp-bridge-remote-file-host remote-file-host)))
        (cl-return buffer)))))

(defun lsp-bridge-construct-tramp-file-name (method user host port path)
  "Construct a TRAMP file name from components: METHOD USER HOST PORT PATH.

SSH tramp file name is like /ssh:user@host#port:path"
  (concat "/"
          method ":"
          (when user (concat user "@"))
          host
          (when port (concat "#" port))
          ":" path))

(defun lsp-bridge-remote-reconnect (remote-file-host force)
  "Reconnect the buffer of REMOTE-FILE-HOST to its tramp connection.
FORCE is passed to `lsp-bridge-sync-tramp-remote'; the call only runs
when `lsp-bridge-remote-start-automatically' is non-nil."
  (when lsp-bridge-remote-start-automatically
    (with-current-buffer (lsp-bridge-get-match-buffer-by-filehost remote-file-host)
      ;; unify handling logic for buffer created by `lsp-bridge-open-remote-file' or open by tramp
      ;; override the buffer-file-name used by `lsp-bridge-sync-tramp-remote'
      ;; if buffer is created by lsp-bridge-open-remote-file then (buffer-file-name) returns nil
      (let ((buffer-file-name (or (buffer-file-name)
                                  (lsp-bridge-construct-tramp-file-name lsp-bridge-remote-file-tramp-method
                                                                        lsp-bridge-remote-file-user
                                                                        lsp-bridge-remote-file-host
                                                                        lsp-bridge-remote-file-port
                                                                        lsp-bridge-remote-file-path))))
        (lsp-bridge-sync-tramp-remote force)))))

(defvar lsp-bridge-remote-file-window nil
  "Window to select again after a remote file response was handled.")

(defun lsp-bridge-open-remote-file--response (tramp-method user host port path content position)
  "Show the remote file content received from the server.
TRAMP-METHOD, USER, HOST, PORT and PATH describe the connection and
the file, CONTENT is the base64 encoded file body and POSITION where
to move point.  The `[LBR]' buffer is created or reused, marked as a
remote file and lsp-bridge is enabled in it."
  (if lsp-bridge-remote-file-window
      (progn
        (select-window lsp-bridge-remote-file-window)
        (setq lsp-bridge-remote-file-window nil)))
  (let ((buf-name (format "[LBR] %s" (file-name-nondirectory path))))
    (lsp-bridge--record-mark-ring)

    (with-current-buffer (get-buffer-create buf-name)
      (text-mode)
      (read-only-mode -1)
      (erase-buffer)
      (insert (lsp-bridge-decode-base64 content))
      (goto-char (point-min))

      (add-hook 'kill-buffer-hook 'lsp-bridge-remote-kill-buffer nil t)

      (let ((mode (lsp-bridge-get-mode-name-from-file-path path)))
        (when mode
          (let ((lsp-bridge-remote-file-flag t)
                (lsp-bridge-remote-file-tramp-method tramp-method)
                (lsp-bridge-remote-file-user user)
                (lsp-bridge-remote-file-host host)
                (lsp-bridge-remote-file-port port)
                (lsp-bridge-remote-file-path path))
            (funcall mode)))))

    (switch-to-buffer buf-name)

    (unless (equal position (list :line 0 :character 0))
      (lsp-bridge--set-mark-ring-in-new-buffer)

      (lsp-bridge-define--jump-flash position))

    (setq-local lsp-bridge-remote-file-flag t)
    (setq-local lsp-bridge-remote-file-tramp-method tramp-method)
    (setq-local lsp-bridge-remote-file-user user)
    (setq-local lsp-bridge-remote-file-host host)
    (setq-local lsp-bridge-remote-file-port port)
    (setq-local lsp-bridge-remote-file-path path)

    ;; Always enable lsp-bridge for remote file.
    ;; Remote file can always edit and update content even some file haven't corresponding lsp server, such as *.txt
    (lsp-bridge-mode 1))

  (when lsp-bridge-ref-open-remote-file-go-back-to-ref-window
    (lsp-bridge-switch-to-ref-window)
    (setq lsp-bridge-ref-open-remote-file-go-back-to-ref-window nil)))

(defun lsp-bridge-remote-kill-buffer ()
  "Tell the server that a remote file buffer was killed.
Runs in `kill-buffer-hook' of remote buffers and sends a
`close_remote_file' request with the host and path."
  (when lsp-bridge-remote-file-flag
    (lsp-bridge-call-async "close_remote_file" lsp-bridge-remote-file-host lsp-bridge-remote-file-path)
    ))

(defun lsp-bridge-decode-base64 (base64-string)
  "Return BASE64-STRING decoded as UTF-8 text."
  (decode-coding-string (base64-decode-string base64-string) 'utf-8))

(defun lsp-bridge-remote-send-lsp-request (method &rest args)
  "Send the LSP request METHOD with ARGS for the current remote file.
The remote file host and path of the buffer are sent along with the
request."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process
                                  (read "lsp_request")
                                  (append
                                   (list lsp-bridge-remote-file-host lsp-bridge-remote-file-path method) args))))

(defun lsp-bridge-remote-send-func-request (method &rest args)
  "Send the lsp-bridge function METHOD with ARGS for the remote file.
The remote file host and path of the buffer are sent along with the
request."
  (lsp-bridge-deferred-chain
    (lsp-bridge-epc-call-deferred lsp-bridge-epc-process
                                  (read "func_request")
                                  (append
                                   (list lsp-bridge-remote-file-host lsp-bridge-remote-file-path method) args))))

(defun lsp-bridge-remote-save-buffer ()
  "Save the remote file of this buffer on the remote host.
A message is shown when the current buffer is not a remote file."
  (interactive)
  (if lsp-bridge-remote-file-flag
      (lsp-bridge-call-async "save_remote_file" lsp-bridge-remote-file-host lsp-bridge-remote-file-path)
    (message "lsp-bridge-remote-save-buffer only for lsp-bridge-remote file.")))

(defun lsp-bridge-indent-left (start end)
  "Indent the region from START to END to the left by one level.
The indentation width comes from `lsp-bridge--get-indent-width' of
the major mode."
  (interactive "r")
  (let ((indent (symbol-value (lsp-bridge--get-indent-width major-mode))))
    (indent-rigidly start end (- indent))))

(defun lsp-bridge-indent-right (start end)
  "Indent the region from START to END to the right by one level.
The indentation width comes from `lsp-bridge--get-indent-width' of
the major mode."
  (interactive "r")
  (let ((indent (symbol-value (lsp-bridge--get-indent-width major-mode))))
    (indent-rigidly start end indent)))


(defcustom lsp-bridge-enable-document-highlight nil
  "Enable document highlight."
  :type 'boolean
  :group 'lsp-bridge)

(defvar lsp-bridge-document-highlight-timer nil
  "Idle timer that triggers `lsp-bridge-document-highlight'.")

(defun lsp-bridge-document-highlight ()
  "Ask the server for the highlights of the symbol at point.
The request is skipped when lsp-bridge mode or document highlighting
is off, or while the minibuffer is active."
  (when (and lsp-bridge-mode
             lsp-bridge-enable-document-highlight
             (not (minibufferp)))
    (lsp-bridge-call-file-api "document_highlight" (lsp-bridge--position))))

(defun lsp-bridge-document-highlight-render (highlights)
  "Draw an overlay for every range in HIGHLIGHTS.
Existing highlight overlays are removed first and each overlay uses
`lsp-bridge-document-highlight-face' in the selected window."
  (remove-overlays (point-min) (point-max) 'face 'lsp-bridge-document-highlight-face)
  (dolist (highlight highlights)
    (let* ((range (plist-get highlight :range))
           (start (plist-get range :start))
           (end (plist-get range :end))
           (start-line (plist-get start :line))
           (start-character (plist-get start :character))
           (end-line (plist-get end :line))
           (end-character (plist-get end :character))
           (start-pos (lsp-bridge-document-highlight--position-to-point start-line start-character))
           (end-pos (lsp-bridge-document-highlight--position-to-point end-line end-character))
           (overlay (make-overlay start-pos end-pos)))
      (overlay-put overlay 'face 'lsp-bridge-document-highlight-face)
      (overlay-put overlay 'window (selected-window)))))

(defun lsp-bridge-document-highlight--position-to-point (line character)
  "Convert the LSP position LINE and CHARACTER to a buffer position."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)
    (point)))

(defface lsp-bridge-document-highlight-face
  '((t :inherit highlight :background "gray20"))
  "Face used for document highlight overlays."
  :group 'lsp-bridge)

(defun lsp-bridge-document-highlight-cleanup ()
  "Remove all document highlight overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'face 'lsp-bridge-document-highlight-face))

(defun lsp-bridge-enable-document-highlight-timer ()
  "Start the repeating document highlight idle timer when it is not set.
The timer runs `lsp-bridge-document-highlight' every half second of
idle time and is stored in `lsp-bridge-document-highlight-timer'."
  (unless lsp-bridge-document-highlight-timer
    (setq lsp-bridge-document-highlight-timer
          (run-with-idle-timer 0.5 t #'lsp-bridge-document-highlight))))

(defun lsp-bridge-toggle-document-highlight ()
  "Toggle document highlight."
  (interactive)
  (if lsp-bridge-enable-document-highlight
      (progn
        (setq lsp-bridge-enable-document-highlight nil)
        (lsp-bridge-document-highlight-cleanup)
        (message "Document highlight disabled"))
    (setq lsp-bridge-enable-document-highlight t)
    (lsp-bridge-enable-document-highlight-timer)
    (lsp-bridge-document-highlight-setup)
    (lsp-bridge-document-highlight)
    (message "Document highlight enabled")))

(defun lsp-bridge-document-highlight-setup ()
  "Add the document highlight cleanup to the local `post-command-hook'."
  (add-hook 'post-command-hook #'lsp-bridge-document-highlight-cleanup nil t))

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
