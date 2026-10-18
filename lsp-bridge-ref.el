;;; lsp-bridge-ref.el --- Show references for lsp-bridge  -*- lexical-binding: t -*-

;; Filename: lsp-bridge-ref.el
;; Description: Search and refacotry code with rg
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-26 14:22:12
;; Version: 5.6
;; Last-Updated: 2020-05-04 17:52:55
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/lsp-bridge-ref.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl-lib' `subr-x' `grep'
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
;;

;;; Installation:
;;
;;

;;; Customize:
;;
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

;;; Require
(require 'cl-lib)
(require 'subr-x)
(require 'grep)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Group ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup lsp-bridge-ref nil
  "Poup bottom window to show code references."
  :group 'lsp-bridge)

(defcustom lsp-bridge-ref-buffer "*lsp-bridge-ref*"
  "The buffer name of code references."
  :type 'string
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-temp-buffer " *lsp-bridge-ref temp* "
  "The buffer name of the clone temp buffer."
  :type 'string
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-mode-hook '()
  "The `lsp-bridge-ref-mode' hook."
  :type 'hook
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-kill-temp-buffer-p t
  "Default this option is true, it will kill temp buffer when quit lsp-bridge-ref buffer.
A buffer will killed if it is open by lsp-bridge-ref and not edit by lsp-bridge-ref.
A buffer won't kill is it open before lsp-bridge-ref command start.
A buffer won't kill if buffer content is change by lsp-bridge-ref.
Anyway, you can set this option with nil if you don't like lsp-bridge-ref kill any buffer."
  :type 'boolean
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-show-function-name-p t
  "View the function name when navigate in match line.
Default is enable, set this variable to nil if you don't like this feature."
  :type 'boolean
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-delete-other-windows t
  "Delete other windows while finding references.
Default is enable, set this variable to nil if you don't like this feature."
  :type 'boolean
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-open-file-in-request-window nil
  "Open file in the window which requested to find references.
Default is nil, set this variable to t if you like this feature."
  :type 'boolean
  :group 'lsp-bridge-ref)

(defcustom lsp-bridge-ref-popup-window-handler #'lsp-bridge-ref-popup-window-default-handler
  "The handler for creating and opening a new lsp-bridge-ref window.
Default is `lsp-bridge-ref-popup-window-default-handler'."
  :type 'function
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-header-line-text
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for the text of the header line."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-header-line-edit-mode
  '((t :inherit warning :bold t))
  "Face for the header line while the results are being edited."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-command
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for a command named in the results."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-file
  '((t :inherit link :bold t))
  "Face for the path of a file in the results."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-line-number
  '((t :inherit line-number))
  "Face for the line number of a result."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-column-number
  '((t :inherit line-number))
  "Face for the column number of a result."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-position-splitter
  '((t :inherit shadow))
  "Face for the character that separates a line from a column."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-match
  '((t :inherit match :bold t))
  "Face for the keyword a result matched."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-diagnostic
  '((t :inherit error :bold t))
  "Face for a diagnostic the results carry."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-mark-changed
  '((t :inherit match :bold t))
  "Face for keyword match.
It inherits `match', the face for what a search found, so a changed line is
shown in the colour of the theme in use."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-mark-deleted
  '((t :inherit error :bold t))
  "Face for a line that the results say is gone.
It inherits `error', the theme's face for something removed, rather than
naming a red of its own."
  :group 'lsp-bridge-ref)

(defface lsp-bridge-ref-font-lock-function-location
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for the location of the function a result is in."
  :group 'lsp-bridge-ref)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These three belong to the remote module and are set or read here; declaring them
;; without a value is what tells the compiler they are not to be looked for in this
;; file, which is how `lsp-bridge-jump.el' declares the first of them as well.
(defvar lsp-bridge-enable-with-tramp)
(defvar lsp-bridge-remote-file-pattern)
(defvar lsp-bridge-remote-file-window)

(defvar lsp-bridge-ref-temp-visit-buffers nil
  "The temp visit buffers use to kill temp buffer after quit lsp-bridge-ref.")

(defvar lsp-bridge-ref-window-configuration-before-search nil
  "Window configuration saved before the search.
It is restored once the search finishes.")

(defvar lsp-bridge-ref-buffer-point-before-search nil
  "Buffer point saved before the search.
It is restored once the search finishes.")

(defvar lsp-bridge-ref-request-search-window nil
  "The window that requested the reference search.")

(defvar lsp-bridge-ref-window-configuration-before-apply nil
  "Window configuration saved before the changes are applied.
It is restored once the changes have been applied.")

(defvar lsp-bridge-ref-hit-count 0
  "Search keyword hit counter.")

(defvar lsp-bridge-ref-regexp-file "^[/\\~].*\\|^[a-z]:.*"
  "Regexp to match filename.")

(defvar lsp-bridge-ref-regexp-split-line "\n\n"
  "Regexp to match empty line between two files.")

(defvar lsp-bridge-ref-regexp-position "^\\([0-9][0-9]*\\):\\([0-9][0-9]*\\):"
  "Regexp to match line/column string.")

(defvar lsp-bridge-ref-changed-lines nil
  "The list that record the changed lines.")

(defvar lsp-bridge-ref-read-input-history nil
  "Minibuffer history for the input of lsp-bridge-ref searches.")

(defvar lsp-bridge-ref-files-history nil "History for files args.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp-bridge-ref mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar lsp-bridge-ref-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'lsp-bridge-ref-beginning-of-line)
    (define-key map (kbd "<tab>") 'lsp-bridge-ref-jump-next-keyword)
    (define-key map (kbd "<backtab>") 'lsp-bridge-ref-jump-prev-keyword)

    (define-key map (kbd "j") 'lsp-bridge-ref-jump-next-keyword)
    (define-key map (kbd "k") 'lsp-bridge-ref-jump-prev-keyword)
    (define-key map (kbd "h") 'lsp-bridge-ref-jump-next-file)
    (define-key map (kbd "l") 'lsp-bridge-ref-jump-prev-file)
    (define-key map (kbd "i") 'lsp-bridge-ref-insert-current-line)

    (define-key map (kbd "SPC") 'lsp-bridge-ref-open-file)
    (define-key map (kbd "RET") 'lsp-bridge-ref-open-file-and-stay)
    (define-key map (kbd "C-m") 'lsp-bridge-ref-open-file-and-stay)

    (define-key map (kbd "e") 'lsp-bridge-ref-switch-to-edit-mode)

    (define-key map (kbd "r") 'lsp-bridge-ref-replace-all-matches)

    (define-key map (kbd "f") 'lsp-bridge-ref-filter-match-results)
    (define-key map (kbd "F") 'lsp-bridge-ref-filter-mismatch-results)

    (define-key map (kbd "x") 'lsp-bridge-ref-filter-match-files)
    (define-key map (kbd "X") 'lsp-bridge-ref-filter-mismatch-files)
    (define-key map (kbd "u") 'lsp-bridge-ref-unfilter)

    (define-key map (kbd "D") 'lsp-bridge-ref-remove-line-from-results)

    (define-key map (kbd "q") 'lsp-bridge-ref-quit)
    map)
  "Keymap used by `lsp-bridge-ref-mode'.")

(defvar lsp-bridge-ref-mode-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'lsp-bridge-ref-beginning-of-line)

    (define-key map (kbd "C-c C-j") 'lsp-bridge-ref-jump-next-keyword)
    (define-key map (kbd "C-c C-k") 'lsp-bridge-ref-jump-prev-keyword)
    (define-key map (kbd "C-c C-h") 'lsp-bridge-ref-jump-next-file)
    (define-key map (kbd "C-c C-l") 'lsp-bridge-ref-jump-prev-file)
    (define-key map (kbd "C-c <C-return>") 'lsp-bridge-ref-open-file)
    (define-key map (kbd "C-c C-v") 'lsp-bridge-ref-switch-to-view-mode)

    (define-key map (kbd "C-c C-d") 'lsp-bridge-ref-delete-line)
    (define-key map (kbd "C-c C-D") 'lsp-bridge-ref-delete-all-lines)
    (define-key map (kbd "C-c C-r") 'lsp-bridge-ref-recover-line)
    (define-key map (kbd "C-c C-R") 'lsp-bridge-ref-recover-buffer)
    (define-key map (kbd "C-c C-q") 'lsp-bridge-ref-quit)
    (define-key map (kbd "C-c C-c") 'lsp-bridge-ref-apply-changed)
    map)
  "Edit keymap used by `lsp-bridge-ref-mode'.")

(define-derived-mode lsp-bridge-ref-mode text-mode "lsp-bridge-ref"
  "Major mode for viewing lsp-bridge search results.

The buffer is read-only; keys of `lsp-bridge-ref-mode-map' navigate
and open the listed matches."
  (kill-all-local-variables)
  (setq major-mode 'lsp-bridge-ref-mode)
  (setq mode-name "lsp-bridge-ref")
  (read-only-mode 1)
  (lsp-bridge-ref-highlight-keywords)
  (use-local-map lsp-bridge-ref-mode-map)
  (run-hooks 'lsp-bridge-ref-mode-hook))

(defun lsp-bridge-ref-highlight-keywords ()
  "Highlight keywords."
  (font-lock-add-keywords
   nil
   '(
     ("^rg\\s-.*" . 'lsp-bridge-ref-font-lock-command)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 2 'lsp-bridge-ref-font-lock-position-splitter)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 1 'lsp-bridge-ref-font-lock-line-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 3 'lsp-bridge-ref-font-lock-column-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([0-9][0-9]*\\)\\(:\\)" 4 'lsp-bridge-ref-font-lock-position-splitter)
     ("^[/\\~].*\\|^[a-z]:.*" . 'lsp-bridge-ref-font-lock-file)))
  ;; NOTE:
  ;; Because search line maybe just contains *half* of string/comment that make rest content of buffer mark as string.
  ;; So we need turn off comment/string font-lock through set `font-lock-keywords-only'.
  (set (make-local-variable 'font-lock-keywords-only) t)
  (font-lock-mode 1))

(defun lsp-bridge-ref-update-header-line ()
  "Update `header-line-format' from the current search state.
Show the search mode, `lsp-bridge-ref-hit-count' and the key
bindings of `lsp-bridge-ref-mode-map' in the header line."
  (setq header-line-format (concat
                            (propertize (format "%s mode" (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search)) 'font-lock-face 'lsp-bridge-ref-font-lock-match)
                            (propertize (format " %s matches" lsp-bridge-ref-hit-count) 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize " [ " 'font-lock-face 'lsp-bridge-ref-font-lock-line-number)
                            (propertize "Nav " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys
                              "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-jump-next-keyword] / \\[lsp-bridge-ref-jump-prev-keyword] / \\[lsp-bridge-ref-jump-next-file] / \\[lsp-bridge-ref-jump-prev-file]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Replace " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys
                              "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-replace-all-matches]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Edit " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-switch-to-edit-mode]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Filter files: " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-filter-match-files] / \\[lsp-bridge-ref-filter-mismatch-files] / \\[lsp-bridge-ref-unfilter]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Filter regex: " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-filter-match-results] / \\[lsp-bridge-ref-filter-mismatch-results]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize "  Remove " 'font-lock-face 'lsp-bridge-ref-font-lock-header-line-text)
                            (propertize
                             (substitute-command-keys "\\<lsp-bridge-ref-mode-map> \\[lsp-bridge-ref-remove-line-from-results]" t)
                             'font-lock-face 'lsp-bridge-ref-font-lock-header-line-edit-mode)
                            (propertize " ]" 'font-lock-face 'lsp-bridge-ref-font-lock-line-number))))

(cl-defstruct (lsp-bridge-ref-search (:constructor lsp-bridge-ref-search-create)
                                     (:constructor lsp-bridge-ref-search-new (pattern dir))
                                     (:copier nil))
  keyword           ; search keyword
  dir               ; base directory
  globs             ; filename only match these globs will be searched
  file-exclude ; toggle exclude files, t means filename NOT match the globs will be searched
  literal      ; literal patterh (t or nil)
  case-sensitive                        ; case-sensitive (t or nil)
  no-ignore                             ; toggle no-ignore (t or nil)
  no-node                               ; toggle no-node (t or nil)
  mode                                  ; view or edit mode
  )

(defvar lsp-bridge-ref-cur-search (lsp-bridge-ref-search-create)
  "Stores parameters of last search.
Becomes buffer local in `lsp-bridge-ref-mode' buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-bridge-ref-popup (references-content references-counter)
  "Show REFERENCES-CONTENT (REFERENCES-COUNTER matches) in the ref buffer.
Reruns rg with the customized arguments, which gives the user more freedom
to run rg with special arguments."
  ;; Save window configuration before do search.
  ;; Just save when `lsp-bridge-ref-window-configuration-before-search' is nil
  ;; Or current buffer is not `lsp-bridge-ref-buffer' (that mean user not quit lsp-bridge-ref and search again in other place).
  (when (or (not lsp-bridge-ref-window-configuration-before-search)
            (not (string-equal (buffer-name) lsp-bridge-ref-buffer)))
    (setq lsp-bridge-ref-window-configuration-before-search (current-window-configuration))
    (setq lsp-bridge-ref-buffer-point-before-search (point)))
  (setq lsp-bridge-ref-request-search-window (selected-window))

  (setf (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search) "View")

  (setq lsp-bridge-ref-temp-visit-buffers nil)
  (setq lsp-bridge-ref-hit-count references-counter)
  ;; Erase or create search result.
  (get-buffer-create lsp-bridge-ref-buffer)
  (with-current-buffer lsp-bridge-ref-buffer
    (let ((inhibit-read-only t))
      ;; Switch to `lsp-bridge-ref-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
      (lsp-bridge-ref-mode)

      ;; Erase buffer content.
      (read-only-mode -1)
      (erase-buffer)))
  (setq lsp-bridge-ref-changed-lines nil)

  ;; Run search command.
  (with-current-buffer lsp-bridge-ref-buffer
    (insert references-content)

    ;; Highlight file path.
    (goto-char (point-min))
    (while (re-search-forward "^\033\\[95m\\([^\033]+\\)\033\\[0m$" nil t)
      (replace-match (concat (propertize (match-string 1)
                                         'face nil 'font-lock-face 'lsp-bridge-ref-font-lock-file))
                     t t))

    ;; Highlight references.
    (goto-char (point-min))
    (while (re-search-forward "\033\\[94m\\([^\033]*\\)\033\\[0m" nil t)
      ;; We need set keyword when popup references
      (setf (lsp-bridge-ref-search-keyword lsp-bridge-ref-cur-search) (match-string 1))
      (replace-match (concat (propertize (match-string 1)
                                         'face nil 'font-lock-face 'lsp-bridge-ref-font-lock-match))
                     t t))

    ;; Highlight diagnostics.
    (goto-char (point-min))
    (while (re-search-forward "\033\\[93m\\([^\033]+\\)\033\\[0m" nil t)
      (replace-match (concat (propertize (match-string 1)
                                         'face nil 'font-lock-face 'lsp-bridge-ref-font-lock-diagnostic))
                     t t))

    (lsp-bridge-ref-update-header-line)

    (read-only-mode 1))

  ;; Create window
  (funcall lsp-bridge-ref-popup-window-handler))

(defun lsp-bridge-ref-find-next-position (regexp)
  "Search forward from end of line for REGEXP.
Return the position after the first match, or nil if the rest of
the buffer does not match."
  (save-excursion
    (end-of-line)
    (search-forward-regexp regexp nil t)))

(defun lsp-bridge-ref-get-match-file ()
  "Return the file name of the match around point.
Search backward for `lsp-bridge-ref-regexp-file' and return the
matching line without its text properties or trailing newline."
  (save-excursion
    (search-backward-regexp lsp-bridge-ref-regexp-file nil t)
    (substring-no-properties (string-remove-suffix "\n" (thing-at-point 'line)))))

(defun lsp-bridge-ref-get-match-line ()
  "Return the line number of the match at point as a number.
Move point to the beginning of the line first."
  (beginning-of-line)
  (string-to-number (thing-at-point 'symbol)))

(defun lsp-bridge-ref-get-match-column ()
  "Return the column number of the match at point as a number.
Move point forward to the colon that follows the line number."
  (search-forward ":")
  (string-to-number (thing-at-point 'symbol)))

(defun lsp-bridge-ref-get-match-buffer (filepath)
  "Return the visiting buffer of FILEPATH, or nil if it is not open.
FILEPATH is compared with the buffer's file name."
  (cl-dolist (buffer (buffer-list))
    (let* ((buf-file-name (buffer-file-name buffer))
           (file-name (if (stringp buf-file-name) (substring-no-properties buf-file-name) buf-file-name)))
      (when (string-equal file-name filepath)
        (cl-return buffer)))))

(defun lsp-bridge-ref-current-line-empty-p ()
  "Return non-nil if the current line contains only whitespace."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun lsp-bridge-ref-get-line-content (buffer line)
  "Return the code text of LINE from BUFFER.
The result starts after the line:column prefix matched by
`lsp-bridge-ref-regexp-position' and runs to the end of the line."
  (with-current-buffer buffer
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (search-forward-regexp lsp-bridge-ref-regexp-position nil t)
      (let ((start (point)) end)
        (end-of-line)
        (setq end (point))
        (buffer-substring start end)))))

(defun lsp-bridge-ref-get-row-column-position ()
  "Return the position where the line:column prefix of the current line starts.
Return nil when the line has no such prefix."
  (let* ((search-bound
          (save-excursion
            (end-of-line)
            (point)))
         (row-column-position
          (save-excursion
            (beginning-of-line)
            (search-forward-regexp lsp-bridge-ref-regexp-position search-bound t))))
    row-column-position))

(defun lsp-bridge-ref-after-change-function (beg end leng-before)
  "Track a buffer change made in edit mode.
BEG and END delimit the change and LENG-BEFORE is its old length.
Compare the changed line with the same line in the temp buffer,
record it in `lsp-bridge-ref-changed-lines' and mark it with an
overlay as changed or, when the line is now empty, as deleted."
  ;; NOTE:
  ;; We should use `save-match-data' wrap function that hook in `after-change-functions'.
  ;; Otherwise will got error: "replace-match-maybe-edit: Match data clobbered by buffer modification hooks"
  (save-match-data
    (let* ((change-line (save-excursion
                          (goto-char beg)
                          (line-number-at-pos)))
           changed-line-content
           original-line-content)
      (setq changed-line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-buffer change-line))
      (setq original-line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-temp-buffer change-line))
      (if (string-equal changed-line-content original-line-content)
          (progn
            (setq lsp-bridge-ref-changed-lines (remove change-line lsp-bridge-ref-changed-lines))
            (lsp-bridge-ref-mark-position-clear change-line))
        (add-to-list 'lsp-bridge-ref-changed-lines change-line)
        (if (string-equal changed-line-content "")
            (lsp-bridge-ref-mark-position-deleted change-line)
          (lsp-bridge-ref-mark-position-changed change-line))))))

(defun lsp-bridge-ref-mark-position-clear (line)
  "Delete the change overlays on LINE.
Only overlays of type \"changed\" or \"deleted\" are removed."
  (save-excursion
    (goto-line line)
    (beginning-of-line)
    (forward-char)
    (dolist (overlay (overlays-at (point)))
      (when (or (string-equal (overlay-get overlay 'overlay-type) "changed")
                (string-equal (overlay-get overlay 'overlay-type) "deleted"))
        (delete-overlay overlay)))))

(defun lsp-bridge-ref-mark-position (line type face)
  "Mark LINE with an overlay of TYPE and FACE.
Any previous change overlay on LINE is removed first."
  (save-excursion
    (lsp-bridge-ref-mark-position-clear line)
    ;; Create mark changed overlay if not exists.
    (let (start end changed-overlay)
      (save-excursion
        (beginning-of-line)
        (setq start (point))
        (end-of-line)
        (setq end (point))
        (setq changed-overlay (make-overlay start end))
        (overlay-put changed-overlay 'overlay-type type)
        (overlay-put changed-overlay 'face face)))))

(defun lsp-bridge-ref-mark-position-changed (line)
  "Mark LINE as changed by the user, using the changed face."
  (lsp-bridge-ref-mark-position line "changed" 'lsp-bridge-ref-font-lock-mark-changed))

(defun lsp-bridge-ref-mark-position-deleted (line)
  "Mark LINE as deleted by the user, using the deleted face."
  (lsp-bridge-ref-mark-position line "deleted" 'lsp-bridge-ref-font-lock-mark-deleted))

(defun lsp-bridge-ref-kill-temp-buffer ()
  "Kill the reference temp buffer and forget the changed lines."
  (when (get-buffer lsp-bridge-ref-temp-buffer)
    (kill-buffer lsp-bridge-ref-temp-buffer)
    (setq lsp-bridge-ref-changed-lines nil)))

(defun lsp-bridge-ref-clone-to-temp-buffer ()
  "Clone the reference buffer content into the reference temp buffer.
The temp buffer keeps the original results so changes can be
compared and recovered.  It is killed when the reference buffer
is killed."
  (lsp-bridge-ref-kill-temp-buffer)
  (with-current-buffer lsp-bridge-ref-buffer
    (add-hook 'kill-buffer-hook 'lsp-bridge-ref-kill-temp-buffer nil t)
    (generate-new-buffer lsp-bridge-ref-temp-buffer)
    (append-to-buffer lsp-bridge-ref-temp-buffer (point-min) (point-max))))

;;;###autoload
(defun lsp-bridge-ref-switch-to-view-mode ()
  "Switch the reference buffer back to view mode.
Discard the change markers and the temp buffer, remove the
change hook and make the buffer read-only again."
  (interactive)
  (with-current-buffer lsp-bridge-ref-buffer
    ;; Do clean work.
    (dolist (line lsp-bridge-ref-changed-lines)
      (lsp-bridge-ref-mark-position-clear line))
    (setq lsp-bridge-ref-changed-lines nil)
    (lsp-bridge-ref-kill-temp-buffer)
    (remove-hook 'after-change-functions 'lsp-bridge-ref-after-change-function t)
    ;; Switch to view mode.
    (read-only-mode 1)
    (use-local-map lsp-bridge-ref-mode-map)
    (kill-local-variable 'query-replace-skip-read-only)
    (setf (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search) "View")
    (lsp-bridge-ref-update-header-line)))

(defun lsp-bridge-ref-filter-results (match-regexp)
  "Remove result lines selected by a regexp read from the minibuffer.
With MATCH-REGEXP non-nil keep the lines that match the regexp and
remove the others; otherwise remove the lines that match.  Report
how many lines were removed and refresh the header line."
  (let ((line-content nil) (remove-counter 0)
          (filter-regexp (read-string
                        (format (if match-regexp
                                    "Filter result match regexp: "
                                  "Filter result not match regexp: ")))))
    (save-excursion
      (with-current-buffer lsp-bridge-ref-buffer
        (setq remove-counter 0)
        (goto-char (point-min))
        (while (search-forward-regexp lsp-bridge-ref-regexp-position nil t)
          (setq line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-buffer (line-number-at-pos)))
          (if match-regexp
              (unless (string-match filter-regexp line-content)
                (lsp-bridge-ref-remove-line-from-results)
                (setq remove-counter (+ 1 remove-counter)))
            (when (string-match filter-regexp line-content)
              (lsp-bridge-ref-remove-line-from-results)
              (setq remove-counter (+ 1 remove-counter)))))
        (if match-regexp
            (message (format "[LSP-Bridge] Remove %s lines not match regexp '%s'." remove-counter filter-regexp))
          (message (format "[LSP-Bridge] Remove %s lines match regexp '%s'." remove-counter filter-regexp))))))
  ;; Update hit number in header line.
  (lsp-bridge-ref-update-header-line-hits))

(defun lsp-bridge-ref-file-extension (file)
  "Return the extension of FILE as a string.
The extension is everything after the last dot of the file name."
  (string-join (cdr (split-string (file-name-nondirectory file) "\\.")) "."))

(defun lsp-bridge-ref-filter-files (match-files)
  "Remove result files selected by a file extension.
Read the extension with `ido-completing-read' among the extensions
present in the results.  With MATCH-FILES non-nil keep the files
that have it and remove the others; otherwise remove the files
that have it.  Do nothing when the results hold only one
extension, then refresh the header line."
  (let (file-extensions filename filter-extension file-extension start end)
    (save-excursion
      (goto-char (point-min))
      (while (setq end (search-forward-regexp lsp-bridge-ref-regexp-file nil t))
        (beginning-of-line)
        (setq start (point))
        (setq filename (buffer-substring-no-properties start end))
        (end-of-line)
        (cl-pushnew (lsp-bridge-ref-file-extension filename) file-extensions :test 'string-equal)))
    (if (< (length file-extensions) 2)
        (message (format "[LSP-Bridge] Has one type files now."))
      (setq filter-extension (ido-completing-read (if match-files
                                                      "Only display file suffix with: "
                                                    "Remove file suffix with: ")
                                                  file-extensions))
      (save-excursion
        (with-current-buffer lsp-bridge-ref-buffer
          (goto-char (point-min))
          (while (setq end (search-forward-regexp lsp-bridge-ref-regexp-file nil t))
            (beginning-of-line)
            (setq start (point))
            (setq file-extension (lsp-bridge-ref-file-extension (buffer-substring-no-properties start end)))
            (if match-files
                (if (string-equal file-extension filter-extension)
                    (end-of-line)
                  (lsp-bridge-ref-remove-lines-under-file))
              (if (string-equal file-extension filter-extension)
                  (lsp-bridge-ref-remove-lines-under-file)
                (end-of-line))))))))
  ;; Update hit number in header line.
  (lsp-bridge-ref-update-header-line-hits))

(defun lsp-bridge-ref-remove-lines-under-file ()
  "Remove the file line at point and all result lines that follow it.
The lines are removed up to the next empty line, which separates
two files in the results buffer."
  (let (start end)
    (save-excursion
      (with-current-buffer lsp-bridge-ref-buffer
        (read-only-mode -1)
        (beginning-of-line)
        (setq start (point))
        (when (search-forward-regexp lsp-bridge-ref-regexp-split-line nil t)
          (setq end (point))
          (kill-region start end))
        (read-only-mode 1)))))

(defun lsp-bridge-ref-update-header-line-hits ()
  "Recount the matches in the reference buffer and update the header line."
  (setq lsp-bridge-ref-hit-count (lsp-bridge-ref-stat-hits))
  (lsp-bridge-ref-update-header-line))

(defun lsp-bridge-ref-stat-hits ()
  "Return the number of match highlights in the reference buffer.
Count the text regions whose properties include the
`lsp-bridge-ref-font-lock-match' face."
  (let ((hit-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((plist (text-properties-at (point)))
              (next-change
               (or (next-property-change (point) (current-buffer))
                   (point-max))))
          (dolist (property plist)
            (when (string-equal (format "%s" property) "lsp-bridge-ref-font-lock-match")
              (setq hit-count (+ hit-count 1))))
          (goto-char next-change)))
      hit-count)))

(defun lsp-bridge-ref-popup-window-default-handler ()
  "Default function for `lsp-bridge-ref-popup-window-handler'."
  ;; Pop search buffer.
  (if lsp-bridge-ref-delete-other-windows
      (delete-other-windows))

  ;; Set `window-resize-pixelwise' with non-nil will cause `split-window' failed.
  (let (window-resize-pixelwise)
    (select-window
     (split-window nil (* 0.618 (window-pixel-height)) nil t)))
  (switch-to-buffer lsp-bridge-ref-buffer)
  (goto-char (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'lsp-bridge-ref-search-input-in-project 'lsp-bridge-ref-search-project)

;;;###autoload
(defun lsp-bridge-ref-replace-all-matches ()
  "Replace all matches in the reference buffer with user input.
Read the replacement text from the minibuffer, replace every
occurrence of the search keyword, then apply the changes to the
source files.  The keyword of `lsp-bridge-ref-cur-search' becomes
the replacement text, and the number of edited lines is reported."
  (interactive)
  (save-excursion
    (let (changed-line-number)
      (let ((inhibit-message t)) ; don't flush to echo area when apply changed, optimise for lsp-bridge-ref
        (with-current-buffer lsp-bridge-ref-buffer
          (let* ((search-keyword (lsp-bridge-ref-search-keyword lsp-bridge-ref-cur-search))
                 (replace-text (read-string (format "Replace '%s' all matches with: " search-keyword) search-keyword)))
            (lsp-bridge-ref-switch-to-edit-mode)
            (if (lsp-bridge-ref-search-literal lsp-bridge-ref-cur-search)
                (query-replace search-keyword replace-text nil (point-min) (point-max))
              (query-replace-regexp search-keyword replace-text nil (point-min) (point-max)))
            (setq changed-line-number (length lsp-bridge-ref-changed-lines))
            (lsp-bridge-ref-apply-changed)
            (lsp-bridge-ref-switch-to-view-mode)
            (setf (lsp-bridge-ref-search-keyword lsp-bridge-ref-cur-search) replace-text))))
      (message "[LSP-Bridge] Replace %s lines" changed-line-number))))

;;;###autoload
(defun lsp-bridge-ref-filter-match-results ()
  "Keep only the result lines that match a regexp read from the minibuffer."
  (interactive)
  (lsp-bridge-ref-filter-results t))

;;;###autoload
(defun lsp-bridge-ref-filter-mismatch-results ()
  "Remove the result lines that match a regexp read from the minibuffer."
  (interactive)
  (lsp-bridge-ref-filter-results nil))

;;;###autoload
(defun lsp-bridge-ref-filter-match-files ()
  "Keep only the result files whose extension is read from the minibuffer."
  (interactive)
  (lsp-bridge-ref-filter-files t))

;;;###autoload
(defun lsp-bridge-ref-filter-mismatch-files ()
  "Remove the result files whose extension is read from the minibuffer."
  (interactive)
  (lsp-bridge-ref-filter-files nil))

;;;###autoload
(defun lsp-bridge-ref-unfilter ()
  "Restore the unfiltered results from the reference temp buffer.
Erase the reference buffer, insert the original content of the
temp buffer and refresh the header line."
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (let ((inhibit-read-only t))
        (lsp-bridge-ref-mode) ; switch to `lsp-bridge-ref-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
        (read-only-mode -1)
        (erase-buffer)
        (insert (with-current-buffer lsp-bridge-ref-temp-buffer
                  (buffer-substring (point-min) (point-max))))
        (read-only-mode 1))
      ;; Update hit number in header line.
      (lsp-bridge-ref-update-header-line-hits))))

;;;###autoload
(defun lsp-bridge-ref-remove-line-from-results ()
  "Remove the current result line from the results buffer.
Do nothing when the current line has no line:column prefix."
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (when (lsp-bridge-ref-get-row-column-position)
        (read-only-mode -1)
        (beginning-of-line)
        (kill-line)
        (kill-line)
        (read-only-mode 1)))))

;;;###autoload
(defun lsp-bridge-ref-jump-next-keyword ()
  "Move to the next match line and open its source location."
  (interactive)
  (let* ((next-position (lsp-bridge-ref-find-next-position lsp-bridge-ref-regexp-position)))
    (if next-position
        (progn
          (goto-char next-position)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to last line."))))

;;;###autoload
(defun lsp-bridge-ref-jump-prev-keyword ()
  "Move to the previous match line and open its source location."
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp lsp-bridge-ref-regexp-position nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp lsp-bridge-ref-regexp-position nil t)
                       (line-number-at-pos))))
               (if (equal first-search-line (line-number-at-pos))
                   ;; Search previous again if first search is same line of point.
                   (save-excursion
                     (beginning-of-line)
                     (search-backward-regexp lsp-bridge-ref-regexp-position nil t))
                 (save-excursion (search-backward-regexp lsp-bridge-ref-regexp-position nil t))))
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to first line."))))

;;;###autoload
(defun lsp-bridge-ref-jump-next-file ()
  "Move to the next file line in the results and open its first match."
  (interactive)
  (let*  ((next-position (lsp-bridge-ref-find-next-position lsp-bridge-ref-regexp-file)))
    (if next-position
        (progn
          (goto-char next-position)
          (forward-line)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to last file."))))

;;;###autoload
(defun lsp-bridge-ref-jump-prev-file ()
  "Move to the previous file line in the results and open its first match."
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion
               (and (search-backward-regexp lsp-bridge-ref-regexp-file nil t)
                    (search-backward-regexp lsp-bridge-ref-regexp-split-line nil t)))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp lsp-bridge-ref-regexp-file nil t)
                       (line-number-at-pos)))
                    (prev-empty-line
                     (save-excursion
                       (search-backward-regexp lsp-bridge-ref-regexp-split-line)
                       (line-number-at-pos))))
               (if (and (> first-search-line prev-empty-line)
                        (not (lsp-bridge-ref-current-line-empty-p)))
                   ;; Search filename previous again if first search is current file result area.
                   (save-excursion
                     (search-backward-regexp lsp-bridge-ref-regexp-split-line)
                     (search-backward-regexp lsp-bridge-ref-regexp-file nil t))
                 (save-excursion (search-backward-regexp lsp-bridge-ref-regexp-file nil t))))
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (forward-line)
          (lsp-bridge-ref-open-file))
      (message "[LSP-Bridge] Reach to first file."))))

;;;###autoload
(defun lsp-bridge-ref-insert-current-line ()
  "Insert the code of the current result line at the original point.
Quit the reference buffer and restore its window configuration
first, so the code goes back where the search started."
  (interactive)
  ;; The two positions and the line are bound first and set afterwards: a plain `let'
  ;; evaluates all of its value forms before any of its bindings exists, so a `setq'
  ;; written inside one of them was setting the variables of the session - which is what
  ;; the compiler had been saying about them.
  (let (start end current-line)
    (save-excursion
      (beginning-of-line)
      (search-forward-regexp lsp-bridge-ref-regexp-position nil t)
      (setq start (point))
      (end-of-line)
      (setq end (point))
      (setq current-line (buffer-substring start end)))
    ;; We need to restore windows if we want to insert current line.
    (lsp-bridge-ref-quit t)
    (insert current-line)))

(defvar lsp-bridge-ref-open-remote-file-go-back-to-ref-window nil
  "Whether to go back to the reference window after opening a remote file.
Set by `lsp-bridge-ref-open-file' and cleared by lsp-bridge when
the remote file has been opened.")
(defun lsp-bridge-switch-to-ref-window ()
  "Select the window that shows the reference buffer.
Create a new reference window with
`lsp-bridge-ref-popup-window-handler' when the buffer is not
displayed in any window."
  ;; Keep cursor in search buffer's window.
  (let ((ref-buffer-window (get-buffer-window lsp-bridge-ref-buffer)))
    (if ref-buffer-window
        (select-window ref-buffer-window)
      ;; Create window if `lsp-bridge-ref-buffer' is not exist in windows.
      (funcall lsp-bridge-ref-popup-window-handler))))

;;;###autoload
(defun lsp-bridge-ref-open-file (&optional stay)
  "Open the file of the result line at point and jump to its match.
Move point to the matched line and column, reveal the match in an
org file when needed, and flash the target line.  Unless STAY is
non-nil, select the reference window again afterwards."
  (interactive)
  (let* ((match-file (lsp-bridge-ref-get-match-file))
         (match-line (lsp-bridge-ref-get-match-line))
         (match-column (lsp-bridge-ref-get-match-column))
         (match-buffer (lsp-bridge-ref-get-match-buffer match-file))
         in-org-link-content-p)
    (save-excursion
      (let ((inhibit-message t))
        ;; Try fill variables when in org file.
        (lsp-bridge-ref-move-to-column match-column)
        (setq in-org-link-content-p
              (and (lsp-bridge-ref-is-org-file match-file)
                   (lsp-bridge-ref-in-org-link-content-p)))
        ;; Note, don't use `find-file-other-window', it will failed if path is tramp path that start with /sudo:root
        (if (and lsp-bridge-ref-open-file-in-request-window
                 ;; check if the window which requested search has beed deleted.
                 (window-valid-p lsp-bridge-ref-request-search-window))
            (select-window lsp-bridge-ref-request-search-window)
          (other-window 1))
        (if (and (not lsp-bridge-enable-with-tramp)
                 (string-match-p lsp-bridge-remote-file-pattern match-file))
            (progn
              (setq lsp-bridge-remote-file-window (selected-window))
              (setq lsp-bridge-ref-open-remote-file-go-back-to-ref-window (not stay))
              (lsp-bridge-call-async "open_remote_file" match-file
                                     (list :line (1- match-line) :character match-column))
              (cl-return))
          (find-file match-file)
          ;; Add to temp list if file's buffer is not exist.
          (unless match-buffer
            (add-to-list 'lsp-bridge-ref-temp-visit-buffers (current-buffer)))
          ;; Jump to match point.
          ;; We use `ignore-errors' to make sure cursor will back to lsp-bridge-ref buffer
          ;; even target line is not exists in search file (such as delete by user).
          (ignore-errors
            (lsp-bridge-ref-move-to-point match-line match-column)
            (when (lsp-bridge-ref-is-org-file match-file)
              ;; Expand org block if current file is *.org file.
              (org-reveal)
              ;; Jump to link beginning if keyword in content area.
              (when in-org-link-content-p
                (search-backward-regexp "\\[\\[" (line-beginning-position) t))))))
      ;; Flash match line.
      (lsp-bridge-ref-flash-line))
    (unless stay
      (lsp-bridge-switch-to-ref-window))
    ;; Ajust column position.
    (lsp-bridge-ref-move-to-column match-column)))

;;;###autoload
(defun lsp-bridge-ref-open-file-and-stay ()
  "Open the file of the result line at point and stay in the results.
Like `lsp-bridge-ref-open-file' with STAY non-nil."
  (interactive)
  (lsp-bridge-ref-open-file t))

(defun lsp-bridge-ref-flash-line ()
  "Flash the current match line.
Also show the name of the enclosing function in the echo area when
`lsp-bridge-ref-show-function-name-p' is non-nil."
  ;; Flash match line.
  (lsp-bridge-flash-line)
  ;; View the function name when navigate in match line.
  (when lsp-bridge-ref-show-function-name-p
    (require 'which-func)
    (when-let* ((function-name (which-function)))
      (message "[LSP-Bridge] Located in function: %s"
               (propertize
                function-name
                'face 'lsp-bridge-ref-font-lock-function-location)))))

(defun lsp-bridge-ref-in-org-link-content-p ()
  "Return non-nil if point is inside the content of an org link.
That is the text between the link target and the closing
brackets."
  (and (looking-back "\\[\\[.*" (line-beginning-position))
       (looking-at ".*\\]\\[")
       (looking-at ".*\\]\\]")))

(defun lsp-bridge-ref-is-org-file (file)
  "Return non-nil if FILE has the org file extension."
  (string-equal (lsp-bridge-ref-file-extension file) "org"))

(defun lsp-bridge-ref-move-to-point (line column)
  "Move point to COLUMN on LINE in the current buffer."
  ;; Jump to match position.
  (goto-line line)
  (beginning-of-line)

  (lsp-bridge-ref-jump-to-column column))

(defun lsp-bridge-ref-move-to-column (column)
  "Move point to COLUMN inside the code of the current result line.
Do nothing when the current line is empty."
  (beginning-of-line)
  (search-forward-regexp lsp-bridge-ref-regexp-position)

  ;; Forward to column if current line is not empty line (delete by `lsp-bridge-ref-delete-line').
  (unless (looking-at "[[:space:]]*$")
    (lsp-bridge-ref-jump-to-column column)))

(defun lsp-bridge-ref-jump-to-column (column)
  "Move forward COLUMN bytes in a line of mixed-width characters.
`move-to-column' counts characters, but the reference buffer stores the
column reported by rg as a byte offset, so the position is found by adding
up `string-bytes' one character at a time."
  (let ((scan-column 0)
        (first-char-point (point)))

    (while (> column scan-column)
      (forward-char 1)
      (setq scan-column (string-bytes (buffer-substring first-char-point (point)))))))

;;;###autoload
(defun lsp-bridge-ref-switch-to-edit-mode ()
  "Switch the reference buffer to edit mode.
Clone the results into the reference temp buffer, keep the file
and position lines read-only, make the code lines editable, and
install `lsp-bridge-ref-after-change-function' to track changes."
  (interactive)
  ;; Clone content to temp buffer.
  (lsp-bridge-ref-clone-to-temp-buffer)
  (setf (lsp-bridge-ref-search-mode lsp-bridge-ref-cur-search) "Edit")

  ;; Set `query-replace-skip-read-only' to avoid read-only error when do `query-replace'.
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (lsp-bridge-ref-update-header-line)
  ;; Turn off readonly mode.
  (read-only-mode -1)
  (use-local-map lsp-bridge-ref-mode-edit-map)
  (let (start end)
    ;; Make all buffer with readonly text property.
    (let ((inhibit-read-only t))
      (save-excursion
        (put-text-property 1 2 'front-sticky '(read-only))
        (put-text-property (point-min) (point-max) 'read-only t)))
    ;; Make all code with edit property.
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (setq start (search-forward-regexp lsp-bridge-ref-regexp-position nil t))
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (put-text-property (1- start) end 'read-only nil)))))
  (add-hook 'after-change-functions 'lsp-bridge-ref-after-change-function nil t)
  ;; Message to user.
  (message "[LSP-Bridge] Switch to edit mode: press C-c C-c to apply change, press C-c C-q cancel edit"))

;;;###autoload
(defun lsp-bridge-ref-quit (&optional restore)
  "Quit the reference buffer and restore the previous window layout.
Kill the buffers that were opened by lsp-bridge-ref when
`lsp-bridge-ref-kill-temp-buffer-p' is non-nil.  Restore the saved
window configuration when `lsp-bridge-ref-delete-other-windows'
or RESTORE is non-nil, otherwise only delete the reference window
and select the window that requested the search."
  (interactive)
  ;; Kill temp buffer open by lsp-bridge-ref.
  (when lsp-bridge-ref-kill-temp-buffer-p
    (dolist (temp-buffer lsp-bridge-ref-temp-visit-buffers)
      (kill-buffer temp-buffer)))
  (setq lsp-bridge-ref-temp-visit-buffers nil)
  ;; Kill search buffer.
  (kill-buffer lsp-bridge-ref-buffer)
  (when lsp-bridge-ref-window-configuration-before-search
    ;; Restore window configuration before search if we delete-other-window before search.
    ;; Or if we need to restore. e.g. we are inserting current line.
    (if (or lsp-bridge-ref-delete-other-windows
            restore)
        (progn
          (set-window-configuration lsp-bridge-ref-window-configuration-before-search)
          (goto-char lsp-bridge-ref-buffer-point-before-search))
      ;; Otherwise we just delete the lsp-bridge-ref window, and select the request window if still exists.
      (delete-window)
      (if (window-valid-p lsp-bridge-ref-request-search-window)
          (select-window lsp-bridge-ref-request-search-window)))
    (setq lsp-bridge-ref-window-configuration-before-search nil)
    (setq lsp-bridge-ref-buffer-point-before-search nil))
  ;; clear saved window which requested search.
  (setq lsp-bridge-ref-request-search-window nil))

;;;###autoload
(defun lsp-bridge-ref-beginning-of-line ()
  "Move to the start of the line:column prefix, or to the line start.
When the current line has no such prefix, move to the beginning of
the line instead."
  (interactive)
  (let* ((row-column-position (lsp-bridge-ref-get-row-column-position)))
    (if row-column-position
        (goto-char row-column-position)
      (move-beginning-of-line 1))))

;;;###autoload
(defun lsp-bridge-ref-delete-line ()
  "Kill the code of the current result line.
Do nothing when the current line has no line:column prefix."
  (interactive)
  ;; `start' and `end' are bound here rather than set: as plain `setq' they became
  ;; variables of the whole session, which the compiler reported as an assignment to
  ;; a free variable and which the next package could read.
  (let ((start nil) (end nil)
        (row-column-position (lsp-bridge-ref-get-row-column-position)))
    (when row-column-position
      (setq start row-column-position)
      (end-of-line)
      (setq end (point))
      (kill-region start end))))

;;;###autoload
(defun lsp-bridge-ref-delete-all-lines ()
  "Kill the code of every result line in the reference buffer."
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (goto-char (point-min))
      (while (search-forward-regexp lsp-bridge-ref-regexp-position nil t)
        (lsp-bridge-ref-delete-line)))))

;;;###autoload
(defun lsp-bridge-ref-recover-line ()
  "Restore the original code of the current result line.
The original is taken from the reference temp buffer."
  (interactive)
  (lsp-bridge-ref-delete-line)
  (insert (lsp-bridge-ref-get-line-content lsp-bridge-ref-temp-buffer (line-number-at-pos))))

;;;###autoload
(defun lsp-bridge-ref-recover-buffer ()
  "Restore the whole reference buffer from the reference temp buffer.
Discard every pending change and switch back to edit mode."
  (interactive)
  (save-excursion
    (with-current-buffer lsp-bridge-ref-buffer
      (let ((inhibit-read-only t))
        ;; Recover buffer content from temp buffer.
        (lsp-bridge-ref-mode) ; switch to `lsp-bridge-ref-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
        (read-only-mode -1)
        (erase-buffer)
        (insert (with-current-buffer lsp-bridge-ref-temp-buffer
                  (buffer-substring (point-min) (point-max))))
        ;; Switch to edit mode.
        (lsp-bridge-ref-switch-to-edit-mode)))))

;;;###autoload
(defun lsp-bridge-ref-apply-changed ()
  "Write the edited result lines back to their source files.
Each changed line replaces the matched line in its file.  Report
that nothing needs to be changed when no line was edited, then
switch the reference buffer back to view mode."
  (interactive)
  (if (equal (length lsp-bridge-ref-changed-lines) 0)
      (message "Nothing need change.")
    ;; Save window configuration before do apply.
    (setq lsp-bridge-ref-window-configuration-before-apply (current-window-configuration))
    ;; Apply changed.
    (let ((inhibit-message t)) ; don't flush to echo area when apply changed, optimise for lsp-bridge-ref
      (save-excursion
        (dolist (line lsp-bridge-ref-changed-lines)
          (let (match-file match-line changed-line-content)
            (setq changed-line-content (lsp-bridge-ref-get-line-content lsp-bridge-ref-buffer line))
            (with-current-buffer lsp-bridge-ref-buffer
              (goto-line line)
              (setq match-file (lsp-bridge-ref-get-match-file))
              (setq match-line (lsp-bridge-ref-get-match-line)))
            (find-file match-file)
            ;; Remove from temp list if file's buffer is exist.
            (setq lsp-bridge-ref-temp-visit-buffers (remove (current-buffer) lsp-bridge-ref-temp-visit-buffers))
            ;; Kill target line.
            (goto-line match-line)
            (kill-line)
            ;; Insert change line.
            (if (string-equal changed-line-content "")
                ;; Kill empty line if line mark as deleted.
                (kill-line)
              ;; Otherwise insert new line into file.
              (insert changed-line-content))))))
    ;; Restore window configuration before apply changed.
    (when lsp-bridge-ref-window-configuration-before-apply
      (set-window-configuration lsp-bridge-ref-window-configuration-before-apply)
      (setq lsp-bridge-ref-window-configuration-before-apply nil))
    ;; Message to user.
    (message (format "[LSP-Bridge] Apply %s lines" (length lsp-bridge-ref-changed-lines))))
  (lsp-bridge-ref-switch-to-view-mode))

(provide 'lsp-bridge-ref)
;;; lsp-bridge-ref.el ends here
