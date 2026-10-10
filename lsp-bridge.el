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

;;; Declarations of the definitions that live in other files
;;
;; The Emacs side is split into several files that require each other in one
;; direction only: this file knows about `lsp-bridge-ui', for one, while that
;; file knows about this one only through the declarations at its top.  A symbol
;; used here but defined there has to be declared, which is also what keeps the
;; byte-compile gate free of "not known to be defined" warnings.

(declare-function lsp-bridge-symbols--current-defun "lsp-bridge-ui" ())
(declare-function lsp-bridge--calculate-column "lsp-bridge")
(declare-function lsp-bridge--rename-file-advisor "lsp-bridge")
(declare-function lsp-bridge--revert-buffer-advisor "lsp-bridge")
(declare-function lsp-bridge-enable-in-minibuffer "lsp-bridge")
(declare-function lsp-bridge-monitor-window-buffer-change "lsp-bridge")
(declare-function lsp-bridge-call-async "lsp-bridge")
(declare-function lsp-bridge-call-file-api "lsp-bridge")
(declare-function lsp-bridge-call-file-api-p "lsp-bridge")
(declare-function lsp-bridge-kill-process "lsp-bridge")
(declare-function lsp-bridge-process-live-p "lsp-bridge")
(declare-function lsp-bridge-start-process "lsp-bridge")
(declare-function lsp-bridge--is-evil-state "lsp-bridge")
(declare-function lsp-bridge--is-meow-state "lsp-bridge")
(declare-function lsp-bridge--not-acm-doc-markdown-buffer "lsp-bridge")
(declare-function lsp-bridge--not-complete-manually "lsp-bridge")
(declare-function lsp-bridge--not-delete-command "lsp-bridge")
(declare-function lsp-bridge--not-execute-macro "lsp-bridge")
(declare-function lsp-bridge--not-follow-complete "lsp-bridge")
(declare-function lsp-bridge--not-in-mark-macro "lsp-bridge")
(declare-function lsp-bridge--not-in-multiple-cursors "lsp-bridge")
(declare-function lsp-bridge--not-in-org-table "lsp-bridge")
(declare-function lsp-bridge--not-in-string "lsp-bridge")
(declare-function lsp-bridge--not-match-hide-characters "lsp-bridge")
(declare-function lsp-bridge--not-match-hide-keywords "lsp-bridge")
(declare-function lsp-bridge--not-match-stop-commands "lsp-bridge")
(declare-function lsp-bridge--not-mind-wave-chat-buffer "lsp-bridge")
(declare-function lsp-bridge--not-only-blank-before-cursor "lsp-bridge")
(declare-function lsp-bridge--not-zsh-buffer "lsp-bridge")
(declare-function lsp-bridge-check-predicate "lsp-bridge")
(declare-function lsp-bridge-complete-other-backends "lsp-bridge")
(declare-function lsp-bridge-record-last-change-position "lsp-bridge")
(declare-function lsp-bridge-try-completion "lsp-bridge")
(declare-function lsp-bridge-auto-format-code "lsp-bridge")
(declare-function lsp-bridge-document-highlight-setup "lsp-bridge")
(defvar lsp-bridge-enable-document-highlight)
(declare-function lsp-bridge-enable-document-highlight-timer "lsp-bridge")
(declare-function lsp-bridge-find-def "lsp-bridge")
(declare-function lsp-bridge-find-impl "lsp-bridge")
(declare-function lsp-bridge-find-references "lsp-bridge")
(declare-function lsp-bridge-hide-doc-tooltip "lsp-bridge")
(declare-function lsp-bridge-hide-signature-tooltip "lsp-bridge")
(declare-function lsp-bridge-signature-help-fetch "lsp-bridge")
(defvar lsp-bridge-signature-help-timer)

(require 'lsp-bridge-core)

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

(defvar lsp-bridge-python-file (expand-file-name "lsp_bridge.py" (if load-file-name
                                                                     (file-name-directory load-file-name)
                                                                   default-directory))
  "Full path of lsp_bridge.py, the Python side of lsp-bridge.")

(defvar-local lsp-bridge-mark-ring nil
  "The list of saved lsp-bridge marks, most recent first.")

(defvar lsp-bridge-position-before-jump nil
  "Position recorded before jumping to a definition.
Used to return to the original location afterwards.")

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

(add-hook 'kill-emacs-hook #'lsp-bridge-kill-process)

(defalias 'lsp-bridge-stop-process #'lsp-bridge-kill-process)

(add-hook 'post-command-hook 'lsp-bridge-monitor-window-buffer-change)

(add-hook 'minibuffer-setup-hook #'lsp-bridge-enable-in-minibuffer)

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

(with-eval-after-load 'evil
  (evil-add-command-properties #'lsp-bridge-find-def :jump t)
  (evil-add-command-properties #'lsp-bridge-find-references :jump t)
  (evil-add-command-properties #'lsp-bridge-find-impl :jump t)

  ;; Fix issue #516
  (add-hook 'acm-mode-hook #'evil-normalize-keymaps))

(advice-add #'rename-file :around #'lsp-bridge--rename-file-advisor)

(advice-add #'revert-buffer :around #'lsp-bridge--revert-buffer-advisor)

;; The Emacs side UI glue of lsp-bridge lives in its own file, which declares
;; what it needs from this one instead of requiring it.
(require 'lsp-bridge-ui)
(require 'lsp-bridge-transport)
(require 'lsp-bridge-completion)
(require 'lsp-bridge-workspace)
(require 'lsp-bridge-jump)
(require 'lsp-bridge-document)
(require 'lsp-bridge-remote)

(provide 'lsp-bridge)

;;; lsp-bridge.el ends here
