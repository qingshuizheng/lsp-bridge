;;; acm-backend-capf.el --- CAPF completion backend   -*- lexical-binding: t; -*-

;; Filename: acm-backend-capf.el
;; Description: CAPF completion backend
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2024, Andy Stewart, all rights reserved.
;; Created: 2024-07-05 22:35:41
;; Version: 0.1
;; Last-Updated: 2024-07-05 22:35:41
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-capf
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
;; CAPF completion backend
;;

;;; Installation:
;;
;; Put acm-backend-capf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-capf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-capf RET
;;

;;; Change log:
;;
;; 2024/07/05
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

(defcustom acm-enable-capf nil
  "Enable capf support."
  :type 'boolean
  :group 'acm-backend-capf)

(defcustom acm-backend-capf-mode-list '(
                                        haskell-interactive-mode
                                        llvm-mode
                                        inf-ruby-mode
                                        nimsuggest-mode
                                        merlin-mode
                                        racer-mode
                                        ledger-mode
                                        nix-repl-mode
                                        ensime-mode
                                        systemd-mode
                                        lmc-asm-mode
                                        fennel-proto-repl-minor-mode
                                        ipdb-track-mode
                                        cider-repl-mode
                                        ess-mode
                                        slime-mode
                                        sly-mrepl-mode
                                        ggtags-mode
                                        gdscript-mode
                                        robe-mode
                                        inf-clojure-minor-mode
                                        ahk-mode
                                        anaconda-mode
                                        redis-mode
                                        lisp-mode)
  "The mode list to support capf."
  :type 'cons)

(defvar acm-backend-capf-mode-hooks
  (mapcar (lambda (mode)
            (intern (concat (symbol-name mode) "-hook")))
          acm-backend-capf-mode-list)
  "Hooks of the modes listed in `acm-backend-capf-mode-list'.
`global-lsp-bridge-mode' enables `lsp-bridge-mode' from these hooks in
addition to `lsp-bridge-default-mode-hooks'.")

(declare-function acm-preview-create-overlay "acm")

(defun acm-backend-capf-candidate-expand (candidate-info bound-start &optional preview)
  "Expand the CAPF candidate CANDIDATE-INFO, replacing BOUND-START onwards.
The completion bounds and the `:exit-function' are asked from CAPF again
instead of being guessed from the symbol at point, and the exit function runs
after the label is inserted, which is what CAPF backends use for their post
completion work.  PREVIEW non-nil shows the candidate in an overlay instead of
inserting it."
  (let* ((label (plist-get candidate-info :label))
         (res (run-hook-wrapped 'completion-at-point-functions
                                #'completion--capf-wrapper 'all))
         (plist (pcase res
                  (`(,_ . ,(and (pred functionp) _f)) nil)
                  (`(,_hookfun . (,_start ,_end ,_collection . ,plist)) plist)))
         (bounds (pcase res
                   (`(,_ . ,(and (pred functionp) _f)) nil)
                   (`(,_hookfun . (,start ,end ,_collection . ,_plist))
                    (cons start end))))
         (exit-fn (plist-get plist :exit-function))
         (start (or (car bounds) bound-start))
         (end (or (cdr bounds) (point))))
    (if preview
        (acm-preview-create-overlay start end label)
      (delete-region start end)
      (insert label)
      (when exit-fn
        (funcall exit-fn label 'finished)))))

(defun acm-backend-capf-candidates (keyword)
  "Return CAPF completion candidates matching KEYWORD.
Ask `completion-at-point-functions' through
`completion--capf-wrapper' and turn each completion into a candidate
plist.  Return nil unless the major mode is a member of
`acm-backend-capf-mode-list'.  When CAPF leaves the text before point
out of its candidates, that prefix is prepended back to every
candidate."
  (when (member major-mode acm-backend-capf-mode-list)
    (let ((res (run-hook-wrapped 'completion-at-point-functions
                                 #'completion--capf-wrapper 'all))
          annotation-function)
      (mapcar (lambda (candidate)
                (list :key candidate
                      :icon "capf"
                      :label candidate
                      :displayLabel candidate
                      :annotation (if annotation-function
                                      (concat (funcall annotation-function candidate) " CAPF")
                                    "CAPF")
                      :backend "capf"))
              (pcase res
                (`(,_ . ,(and (pred functionp) f)) (funcall f))
                (`(,hookfun . (,start ,end ,collection . ,plist))
                 (unless (markerp start) (setq start (copy-marker start)))
                 (setq annotation-function (plist-get plist :annotation-function))
                 (let* ((completion-extra-properties plist)
                        (completion-in-region-mode-predicate
                         (lambda ()
                           ;; We're still in the same completion field.
                           (let ((newstart (car-safe (funcall hookfun))))
                             (and newstart (= newstart start)))))
                        (initial (buffer-substring-no-properties start end))
                        (candidates (completion-all-completions initial collection nil (length initial)))
                        prefix)
                   (when-let* ((z (last candidates)))
                     (setcdr z nil))
                   ;; Sometime CAPF excludes the prefix from the candidates.
                   ;; Adding it back based on the `keyword' and `initial'.
                   (when (> (length keyword) (length initial))
                     (setq prefix (substring keyword 0  (- (length keyword)
                                                           (length initial))))
                     (setq candidates (mapcar (lambda (candidate)
                                                (concat prefix candidate))
                                              candidates)))
                   candidates)))))))

(provide 'acm-backend-capf)

;;; acm-backend-capf.el ends here
