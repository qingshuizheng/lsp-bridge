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

(defun acm-backend-capf--hook-candidates ()
  "Ask `completion-at-point-functions' for the completions at point.
A CAPF that signals - ekg's tag completion does, with stale match data, when it
is asked from a background refresh - is skipped and reported, so that it cannot
hide what the other entries of `completion-at-point-functions' offer."
  (condition-case error
      (run-hook-wrapped 'completion-at-point-functions
                        #'completion--capf-wrapper 'all)
    (error (message "CAPF signalled and was skipped: %S" error) nil)))

(defun acm-backend-capf-candidate-expand (candidate-info bound-start &optional preview)
  "Expand the CAPF candidate CANDIDATE-INFO, replacing BOUND-START onwards.
The completion bounds and the `:exit-function' are asked from CAPF again
instead of being guessed from the symbol at point, and the exit function runs
after the label is inserted, which is what CAPF backends use for their post
completion work.  PREVIEW non-nil shows the candidate in an overlay instead of
inserting it."
  (let* ((label (plist-get candidate-info :label))
         (res (acm-backend-capf--hook-candidates))
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
        ;; The exit function does post completion work on text that is already in
        ;; the buffer - ekg wraps a tag in brackets, for instance.  It is called
        ;; with the text that was completed, and an error inside it is reported
        ;; rather than allowed to abort the completion the user just made: a
        ;; backend that signals here (ekg's transclusion does when it cannot find
        ;; the title it was given) must not leave the selection half applied.
        (condition-case error
            (funcall exit-fn (buffer-substring-no-properties start (point)) 'finished)
          (error (message "CAPF :exit-function signalled: %S" error)))))))

(defun acm-backend-capf-candidates (_keyword)
  "Return CAPF completion candidates matching KEYWORD.
Ask `completion-at-point-functions' through
`completion--capf-wrapper' and turn each completion into a candidate
plist.  Return nil unless the major mode is a member of
`acm-backend-capf-mode-list'.  KEYWORD itself is not used, because a CAPF
candidate already is the complete text that replaces the region its entry
returned."
  (when (member major-mode acm-backend-capf-mode-list)
      (let ((res (acm-backend-capf--hook-candidates))
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
                        )
                   (when-let* ((z (last candidates)))
                     (setcdr z nil))
                   ;; The CAPF contract is that every candidate is the complete
                   ;; text that replaces the region the CAPF returned, so nothing
                   ;; is prepended here.  Upstream used to prepend the part of
                   ;; KEYWORD before that region, which doubles it in the buffer
                   ;; (a candidate with the prefix in front is inserted after a
                   ;; region that already starts inside that prefix) and hands
                   ;; the `:exit-function' a string that its own tables do not
                   ;; know - it is what broke ekg's transclusion completion.
                   candidates)))))))

(provide 'acm-backend-capf)

;;; acm-backend-capf.el ends here
