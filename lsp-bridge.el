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

(defun lsp-bridge-monitor-after-save ()
  "Notify the server that the current buffer was saved.
Runs in `after-save-hook' and sends a `save_file' request with the
buffer name."
  (lsp-bridge-call-file-api "save_file" (buffer-name)))

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

  ;; Don't enable lsp-bridge when the current buffer is a popup of the menu.
  (unless (acm-session-popup-buffer-p)
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
