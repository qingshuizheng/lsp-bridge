;;; lsp-bridge-dart.el --- Dart protocol   -*- lexical-binding: t; no-byte-compile: t; -*-*-

;; Filename: lsp-bridge-dart.el
;; Description: Dart protocol
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-10-03 21:35:08
;; Version: 0.1
;; Last-Updated: 2023-10-03 21:35:08
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge-dart
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
;; Dart protocol
;;

;;; Installation:
;;
;; Put lsp-bridge-dart.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lsp-bridge-dart)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lsp-bridge-dart RET
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

(defface lsp-bridge-dart-closing-label-face
  `((t :foreground "#aaaaaa"))
  "Face for dart closing tab."
  :group 'lsp-bridge-dart)

(defvar-local lsp-bridge-dart-closing-label-overlays '()
  "Overlays of the `// }' closing labels shown in the current buffer.
Buffer-local, so the labels of one file never leak into another.")

(defun lsp-bridge-dart-hide-closing-label-overlays ()
  "Delete every closing-label overlay of the current buffer and forget them."
  (when lsp-bridge-dart-closing-label-overlays
    (dolist (dart-overlay lsp-bridge-dart-closing-label-overlays)
      (delete-overlay dart-overlay)))

  (setq-local lsp-bridge-dart-closing-label-overlays nil))

(defvar lsp-bridge-dart-closing-labels-lock (make-mutex "lsp-bridge-dart-closing-labels-lock")
  "Mutex serialising closing-label rendering.
Dart sends labels while the editor may still be rendering the previous
response; without the lock the overlays interleave and end up in a
random order.")

(defun lsp-bridge-dart-closing-labels--render (filepath filehost closing-labels)
  "Render the Dart CLOSING-LABELS in the buffer visiting FILEPATH.
FILEHOST identifies the (possibly remote) host of that buffer.  Overlays
of the previous response are removed first, and the whole render runs
under `lsp-bridge-dart-closing-labels-lock'."
  ;; lsp-bridge is too fast, use locks to avoid interface disorder issues caused by multi-threaded rendering of overlays
  (with-mutex lsp-bridge-dart-closing-labels-lock
    (lsp-bridge--with-file-buffer
        filepath filehost
        ;; Hide previous overlays first.
        (lsp-bridge-dart-hide-closing-label-overlays)

        ;; Render new overlays.
        (save-excursion
          (save-restriction
            (dolist (closing-label closing-labels)
              (let* ((label (plist-get closing-label :label))
                     (range (plist-get closing-label :range))
                     (end (plist-get range :end)))
                (goto-char (acm-backend-lsp-position-to-point end))

                ;; Looks dart's closing label always at end of line.
                (end-of-line)

                (let* ((overlay (make-overlay (point) (1+ (point)) nil t)))
                  (overlay-put overlay 'before-string (propertize (concat " // " label) 'face 'lsp-bridge-dart-closing-label-face))
                  (overlay-put overlay 'evaporate t) ; NOTE, `evaporate' is import
                  (push overlay lsp-bridge-dart-closing-label-overlays)
                  ))))))))

(provide 'lsp-bridge-dart)

;;; lsp-bridge-dart.el ends here
