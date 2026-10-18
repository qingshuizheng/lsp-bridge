;;; acm-backend-yas.el --- Yasnippet backend for acm  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Filename: acm-backend-yas.el
;; Description: Yasnippet backend for acm
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-06-07 08:52:11
;; Version: 0.1
;; Last-Updated: 2022-06-07 08:52:11
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/acm-backend-yas
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
;; Yasnippet backend for acm
;;

;;; Installation:
;;
;; Put acm-backend-yas.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'acm-backend-yas)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET acm-backend-yas RET
;;

;;; Change log:
;;
;; 2022/06/07
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

(require 'yasnippet)

;;; Code:

(defgroup acm-backend-yas nil
  "Yasnippet backend for ACM."
  :group 'acm)

(defcustom acm-enable-yas t
  "Popup yasnippet completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-candidate-min-length 0
  "Minimal length of a yas candidate."
  :type 'integer
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-candidates-number 2
  "Maximal number of yas candidate of menu."
  :type 'integer
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-show-trigger-keyword " (%s)"
  "Format for displaying extra info about yas candidaets.
If the value is nil, then format is \"filename\".
If the value is t or string $str contains one %s, then format is
\"filename trigger\" or \"trigger filename\" which is controlled by
`acm-backend-yas-match-by-trigger-keyword'."
  :type '(choice (boolean :tag "Enable or not, t to use default value")
                 (string :tag "Literal text with %s"))
  :group 'acm-backend-yas)

(defcustom acm-backend-yas-match-by-trigger-keyword nil
  "If non-nil, match yasnippet candidates by trigger keyword.
Default matching use snippet filename."
  :type 'boolean
  :group 'acm-backend-yas)

(defface acm-backend-yas-extra-info-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for yas extra information."
  :group 'acm-backend-yas)

;;; Variables

(defvar acm-backend-yas--cache nil
  "Cached list of yasnippet templates for `acm-backend-yas--cache-modes'.")
(defvar acm-backend-yas--cache-modes nil
  "Modes that `acm-backend-yas--cache' was computed for.")
(defvar acm-backend-yas--cache-expire-p nil
  "Non-nil when the yasnippet template cache must be rebuilt.")

;;; Functions

(defun acm-backend-yas--cache-expire (&rest _)
  "Mark the yasnippet template cache as stale.
It is attached with `:after' advice to the functions that load
snippets, so the next candidate request rebuilds the cache."
  (when acm-enable-yas
    (setq acm-backend-yas--cache-expire-p t)))

(advice-add #'yas-load-directory :after #'acm-backend-yas--cache-expire)
(advice-add #'yas-load-snippet-buffer :after #'acm-backend-yas--cache-expire)

(defun acm-backend-yas--template-cons (template)
  "Return the labelling cons of yasnippet TEMPLATE.
The car is the snippet name, or its trigger when both
`acm-backend-yas-show-trigger-keyword' and
`acm-backend-yas-match-by-trigger-keyword' are non-nil; the cdr holds
the other of the two."
  (let ((name (yas--template-name template))
        (trigger (yas--template-key template)))
    (if (and acm-backend-yas-show-trigger-keyword
             acm-backend-yas-match-by-trigger-keyword)
        (cons trigger name)
      (cons name trigger))))

(defun acm-backend-yas--template-sort (keyword templates)
  "Sort TEMPLATES for display when completing KEYWORD.
Templates whose label starts with KEYWORD come first, then the
shorter labels; TEMPLATES may be reordered in place, and is returned
unchanged when KEYWORD is nil."
  (when templates
    (if keyword
        (cl-sort templates
                 (lambda (a b)
                   (let* ((a-label (car (acm-backend-yas--template-cons a)))
                          (b-label (car (acm-backend-yas--template-cons b)))
                          (a-include-prefix (string-prefix-p keyword a-label))
                          (b-include-prefix (string-prefix-p keyword b-label)))
                     (cond
                      ((and b-include-prefix (not a-include-prefix))
                       nil)
                      ((and a-include-prefix (not b-include-prefix))
                       t)
                      ((and a-include-prefix b-include-prefix)
                       (string-lessp a-label b-label))
                      (t
                       (< (length a-label) (length b-label)))))))
      templates)))

(defun acm-backend-yas-candidates (keyword)
  "Return yasnippet candidates matching KEYWORD.
Return nil unless yasnippet is enabled and KEYWORD is non-empty.
The snippet tables are cached in `acm-backend-yas--cache' and
refreshed when they expire or the active modes change."
  (when (and acm-enable-yas
             yas-minor-mode
             (>= (length keyword) acm-backend-yas-candidate-min-length)
             (not (string-empty-p keyword)))
    (when (or acm-backend-yas--cache-expire-p
              (null acm-backend-yas--cache)
              (not (equal acm-backend-yas--cache-modes (yas--modes-to-activate))))
      (setq acm-backend-yas--cache (yas--all-templates (yas--get-snippet-tables)))
      (setq acm-backend-yas--cache-expire-p nil)
      (setq acm-backend-yas--cache-modes (yas--modes-to-activate)))
    (let* ((show-key acm-backend-yas-show-trigger-keyword)
           ;; WORKAROUND backward compatibility for boolean value
           (form (concat "%s" (or (and (booleanp show-key) show-key " (%s)") show-key)))
           (match-templates (acm-backend-yas--template-sort
                             keyword
                             (seq-filter (lambda (s)
                                           (acm-candidate-fuzzy-search
                                            keyword
                                            (car (acm-backend-yas--template-cons s))))
                                         acm-backend-yas--cache)))
           (limit (min (length match-templates) acm-backend-yas-candidates-number)))
      (mapcar
       (lambda (template)
         (let* ((pair (acm-backend-yas--template-cons template))
                (match (car pair))
                (display (format form
                                 match
                                 (propertize (cdr pair)
                                             'face
                                             'acm-backend-yas-extra-info-face)))
                (content (yas--template-content template)))
           (list :key match
                 :icon "snippet"
                 :label match
                 :displayLabel display
                 :content content
                 :annotation "Yas-Snippet"
                 :backend "yas")))
       (cl-subseq match-templates 0 limit)))))

(defun acm-backend-yas-candidate-expand (candidate bound-start)
  "Expand yasnippet CANDIDATE after deleting up to BOUND-START.
Delete the text between BOUND-START and point, then expand the
snippet body stored in CANDIDATE."
  (delete-region bound-start (point))
  (yas-expand-snippet (plist-get candidate :content)))

(defun acm-backend-yas-candidate-doc (candidate)
  "Return the snippet body of CANDIDATE, or nil when it has none."
  (ignore-errors (plist-get candidate :content)))

(defun yas-expand-snippet-acm-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, rewriting jdtls snippet placeholders.
When the buffer is served by jdtls, $TM_SELECTED_TEXT in the first
argument is replaced with `yas-selected-text' so that the selected
text is inserted; otherwise ARGS are passed through unchanged."
  ;; Use yas-selected-text to implement $TM_SELECTED_TEXT return by jdtls
  (if (and (boundp 'acm-backend-lsp-server-names)
           (equal acm-backend-lsp-server-names '("jdtls")))
      (apply orig-fun (string-replace "$TM_SELECTED_TEXT" "`yas-selected-text`" (car args)) (cdr args))
    ;; Otherwise, not change yassnippet behavior.
    (apply orig-fun args)))

(advice-add 'yas-expand-snippet :around
	        #'yas-expand-snippet-acm-advice)

(provide 'acm-backend-yas)
;;; acm-backend-yas.el ends here
