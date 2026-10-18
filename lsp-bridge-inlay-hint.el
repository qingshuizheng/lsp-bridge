;;; lsp-bridge-inlay-hint.el --- Inlay hint protocol   -*- lexical-binding: t; no-byte-compile: t; -*-*-

;; Filename: lsp-bridge-inlay-hint.el
;; Description: Inlay hint protocol
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-10-03 21:35:08
;; Version: 0.1
;; Last-Updated: 2023-10-03 21:35:08
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-inlay-hint
;; Keywords:
;; Compatibility: GNU Emacs 30.0.50
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
;; Inlay hint protocol
;;

;;; Installation:
;;
;; Put lsp-bridge-inlay-hint.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-inlay-hint)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-inlay-hint RET
;;

;;; Change log:
;;
;; 2023/10/03
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

;;; Code:

;; The macro `lsp-bridge--with-file-buffer' lives in lsp-bridge-core.el,
;; the one file of the package that requires nothing else of it: a macro
;; that cannot be loaded without a cycle is compiled as a function call,
;; which fails with "invalid-function" at run time.
(require 'lsp-bridge-core)

(defcustom lsp-bridge-enable-inlay-hint nil
  "Whether to enable inlay hint."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-bridge)

(defface lsp-bridge-inlay-hint-face
  '((t :inherit shadow))
  "Face for inlay hint.
The hints are not part of the text, so they inherit `shadow' rather than
naming a grey, and follow the theme."
  :group 'lsp-bridge-inlay-hint)

(defvar-local lsp-bridge-inlay-hint-last-update-pos nil
  "The window end position at the last inlay hint request.")
(defvar-local lsp-bridge-inlay-hint-cache-time nil
  "When the inlay hints in `lsp-bridge-inlay-hint-cache' were drawn.
Used to drop an answer that arrives within half a second of the one before it,
which servers send when a change follows a request closely.")

(defvar-local lsp-bridge-inlay-hint-cache nil
  "We use `lsp-bridge-inlay-hint-cache' avoid screen flicker if two respond result is same.")
(defvar-local lsp-bridge-inlay-hint-overlays '()
  "The overlays that render inlay hints in this buffer.")

(defun lsp-bridge-inlay-hint-monitor-window-scroll ()
  "Request inlay hints when the window end position changed.
Do nothing unless `lsp-bridge-enable-inlay-hint' is non-nil.  The
last requested window end position is stored in
`lsp-bridge-inlay-hint-last-update-pos'."
  (when lsp-bridge-enable-inlay-hint
    (let ((window-pos (window-end nil t)))
      (when (not (equal lsp-bridge-inlay-hint-last-update-pos window-pos))
        (lsp-bridge-inlay-hint-try-send-request)
        (setq-local lsp-bridge-inlay-hint-last-update-pos window-pos)))))

(defun lsp-bridge-inlay-hint-try-send-request ()
  "Request inlay hints when `lsp-bridge-enable-inlay-hint' is non-nil."
  (when lsp-bridge-enable-inlay-hint
    (lsp-bridge-inlay-hint)))

(defun lsp-bridge-inlay-hint ()
  "Request inlay hints for the whole buffer.
The range from `point-min' to `point-max' is sent to the LSP
server, so the request does not depend on the visible window."
  ;; `window-start' won't return valid value of window start position if we not call `redisplay' before 'window-start',
  ;; but call `redisplay' will cause current line scroll to window start postion, like issue https://github.com/manateelazycat/lsp-bridge/issues/1029
  ;;
  ;; So we use `window-end' to test window is scroll or not, and always pass `point-min' and `point-max' to LSP server.
  (lsp-bridge-call-file-api "inlay_hint"
                            (lsp-bridge--point-position (point-min))
                            (lsp-bridge--point-position (point-max))))

(defun lsp-bridge-inlay-hint-hide-overlays ()
  "Delete all inlay hint overlays in the current buffer."
  (when lsp-bridge-inlay-hint-overlays
    (dolist (inlay-hint-overlay lsp-bridge-inlay-hint-overlays)
      (delete-overlay inlay-hint-overlay)))

  (setq-local lsp-bridge-inlay-hint-overlays nil))

(defun lsp-bridge-inlay-hint-padding-text (hint-padding is-left)
  "Return the padding string for HINT-PADDING.
HINT-PADDING is the :paddingLeft or :paddingRight value of the
hint, which is nil, :json-false or non-nil.  IS-LEFT non-nil means
the padding is inserted on the left side.  Return nil when no
padding is needed or when the neighboring character already is a
space or a tab."
  (and hint-padding
       (not (eq hint-padding :json-false))
       (not (memq (if is-left
                      (char-before)
                    (char-after))
                  '(32 9)))
       " "))

(defun lsp-bridge-inlay-hint-label-text (label-info)
  "Return the label text of LABEL-INFO as a string.
LABEL-INFO is either a string or a list of plists with a :value
key, which are concatenated without a separator."
  (format "%s"
          (if (listp label-info)
              ;; We concat value of list if label is list.
              (mapconcat (lambda (label)
                           (plist-get label :value))
                         label-info
                         "")            ; Separator for mapconcat
            ;; Otherwise label is string, just return itself.
            label-info)))

(defvar lsp-bridge-inlay-hint-lock (make-mutex "lsp-bridge-inlay-hint-lock")
  "Mutex that serializes inlay hint overlay rendering.")

(defun lsp-bridge-inlay-hint--render (filepath filehost inlay-hints)
  "Render INLAY-HINTS for the file FILEPATH on FILEHOST.
The hints are skipped when they equal
`lsp-bridge-inlay-hint-cache' or were cached less than 0.5 seconds
ago.  Otherwise the previous overlays are deleted and one overlay
per hint is created, using a before-string for type hints and an
after-string for parameter hints.  Rendering is serialized with
`lsp-bridge-inlay-hint-lock'."
  ;; lsp-bridge is too fast, use locks to avoid interface disorder issues caused by multi-threaded rendering of overlays
  (with-mutex lsp-bridge-inlay-hint-lock
    (lsp-bridge--with-file-buffer
        filepath filehost

        (unless (or (equal inlay-hints lsp-bridge-inlay-hint-cache)
                    (and lsp-bridge-inlay-hint-cache
                         (time-less-p (time-since lsp-bridge-inlay-hint-cache-time) 0.5)))
          ;; Hide previous overlays first.
          (lsp-bridge-inlay-hint-hide-overlays)

          (setq-local lsp-bridge-inlay-hint-cache inlay-hints)
          (setq-local lsp-bridge-inlay-hint-cache-time (current-time))

          (save-excursion
            (save-restriction
              (let ((hint-index 0))
                (dolist (hint inlay-hints)
                  (goto-char (acm-backend-lsp-position-to-point (plist-get hint :position)))
                  (let* ((hint-kind (plist-get hint :kind))
                         ;; InlayHintKind is 1 mean is an inlay hint that for a type annotation. (rendered after variable),
                         ;; 2 mean is an inlay hint that is for a parameter. (rendered before argument).
                         ;; type annotation is hint need render at end of line, we use `after-string' overlay to implement it.
                         (hint-render-use-after-string-p (eql hint-kind 1))
                         ;; Hint text need concat padding-left, label and padding-right.
                         (hint-text (concat
                                     (lsp-bridge-inlay-hint-padding-text (plist-get hint :paddingLeft) t)
                                     (lsp-bridge-inlay-hint-label-text (plist-get hint :label))
                                     (lsp-bridge-inlay-hint-padding-text (plist-get hint :paddingRight) nil)))
                         ;; For type hints (kind=1), add `cursor 1' so the cursor stops at the
                         ;; start of the hint text instead of jumping past it when moving right.
                         ;; See https://github.com/emacs-lsp/lsp-mode/issues/3263
                         (hint-text-propertized
                          (if hint-render-use-after-string-p
                              (propertize hint-text 'face 'lsp-bridge-inlay-hint-face 'cursor 1)
                            (propertize hint-text 'face 'lsp-bridge-inlay-hint-face)))
                         (overlay (if hint-render-use-after-string-p
                                      (make-overlay (point) (1+ (point)) nil t)
                                    (make-overlay (1- (point)) (point) nil nil nil))))
                    (overlay-put overlay
                                 (if hint-render-use-after-string-p 'before-string 'after-string)
                                 hint-text-propertized)
                    (overlay-put overlay 'evaporate t) ; NOTE, `evaporate' is important
                    (push overlay lsp-bridge-inlay-hint-overlays)
                    (setq hint-index (1+ hint-index)))))))))))

(defun lsp-bridge-inlay-hint-retry (filepath)
  "Retry the InlayHint request for FILEPATH.
The server answers with a content-modified error when an inlayHint request
follows a didChange immediately; lsp-bridge calls this again so that the
request does not go unanswered."
  (when (string-prefix-p "file://" filepath)
    (setq filepath (string-remove-prefix "file://" filepath)))
  (when (lsp-bridge-path-equal filepath (buffer-file-name))
    (lsp-bridge-inlay-hint-try-send-request)))

(provide 'lsp-bridge-inlay-hint)
;;; lsp-bridge-inlay-hint.el ends here
