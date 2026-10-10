;;; lsp-bridge-lsp-installer.el --- LSP server installer   -*- lexical-binding: t -*-

;; Filename: lsp-bridge-lsp-installer.el
;; Description: LSP server installer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-09-27 09:28:09
;; Version: 0.1
;; Last-Updated: 2022-09-27 09:28:09
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-lsp-installer
;; Keywords:
;; Compatibility: GNU Emacs 28.1
;;
;; Features that might be required by this library:
;;
;;
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
;; LSP server installer
;;

;;; Installation:
;;
;; Put lsp-bridge-lsp-installer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-lsp-installer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-lsp-installer RET
;;

;;; Change log:
;;
;; 2022/09/27
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'cl-lib)
;;; Code:

(defun lsp-bridge-install-omnisharp ()
  (interactive)
  (let* ((to-append (if (string= lsp-bridge-csharp-lsp-server "omnisharp-dotnet")
                        (cond
                         ((eq system-type 'windows-nt) "omnisharp-win-x64-net6.0.zip")
                         ((eq system-type 'darwin)
                          (if (eq (car (split-string system-configuration "-")) 'aarch64)
			                  "omnisharp-osx-arm64-net6.0.zip"
			                "omnisharp-osx-x64-net6.0.zip"))
                         (t "omnisharp-linux-x64-net6.0.zip"))
		              "omnisharp-mono.zip"))
	     (url (concat "https://github.com/OmniSharp/omnisharp-roslyn/releases/latest/download/" to-append))
         (down-des (if (eq system-type 'windows-nt)
                       (substitute-in-file-name (concat "\\$USERPROFILE\\AppData\\Local\\Temp\\" to-append))
                     "/tmp/omnisharp-mono.zip"))
         (install-des (expand-file-name (concat user-emacs-directory (file-name-as-directory ".cache") (file-name-as-directory "omnisharp")))))
    (url-copy-file url down-des 1)
    (unless (file-directory-p install-des)
      (make-directory install-des t))
    (call-process-shell-command (format "%s -xf %s -C %s" "tar" down-des install-des))
    (call-process-shell-command (format "chmod +x %s" (format "%sOmniSharp" install-des)))
    ))

(defun lsp-bridge-copilot-login ()
  (interactive)
  (lsp-bridge-call-async "copilot_login"))

(defun lsp-bridge-copilot-logout ()
  (interactive)
  (lsp-bridge-call-async "copilot_logout"))

(defun lsp-bridge-copilot-status ()
  (interactive)
  (lsp-bridge-call-async "copilot_status"))

(provide 'lsp-bridge-lsp-installer)

;;; lsp-bridge-lsp-installer.el ends here
