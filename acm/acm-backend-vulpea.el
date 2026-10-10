;;; acm-backend-vulpea.el --- acm vulpea support -*- lexical-binding: t -*-

;;; Commentary:

;; Vulpea is the note database built on top of org-roam that this tree manages
;; notes with: `vulpea-db-query' returns `vulpea-note' structures and
;; `vulpea-utils-link-make-string' builds the id: link of a note.
;;
;; This backend offers the titles of those notes while point is inside `[['
;; brackets and inserts the id: link of the selected note, so a note can be
;; linked from any buffer without leaving it.  It is enabled with
;; `acm-enable-vulpea'.

;;; Code:

(require 'cl-lib)
(require 'org)

(declare-function acm-candidate-fuzzy-search "acm" (keyword candidate))
(declare-function acm-candidate-sort-by-prefix "acm" (keyword candidates))
(declare-function vulpea-db-query "vulpea-db" (&optional filter-fn))
(declare-function vulpea-note-title "vulpea-note" (note))
(declare-function vulpea-utils-link-make-string "vulpea-utils" (note &optional description))

(defgroup acm-backend-vulpea nil
  "Vulpea backend for acm."
  :group 'acm)

(defcustom acm-enable-vulpea nil
  "Popup Vulpea note completions when this option is turn on.
The backend reads the note titles from the Vulpea database with
`vulpea-db-query', so vulpea has to be loaded and its database built."
  :type 'boolean
  :group 'acm-backend-vulpea)

(defcustom acm-backend-vulpea-candidates-number 10
  "Maximal number of Vulpea candidates in the menu."
  :type 'integer
  :group 'acm-backend-vulpea)

(defvar acm-backend-vulpea-bracket-re
  (rx "[[" (group-n 1 (optional "id:")) (group-n 2 (zero-or-more (not (any "]")))) (optional "]]"))
  "Regexp matching the `[[' link brackets of a Vulpea note link.
Group 1 is the optional \"id:\" prefix and group 2 is the text typed
inside the brackets.  The closing brackets are optional, so the menu also
pops up while they are still being typed; when they are already there the
expansion replaces them together with the typed text.")

(defun acm-backend-vulpea--candidate (note)
  "Return the completion candidate of the Vulpea NOTE.
The note itself is kept in the candidate, so expanding it does not have
to look it up in the database again."
  (let ((title (vulpea-note-title note)))
    (list :key title
          :icon "note"
          :label title
          :displayLabel title
          :annotation "Vulpea"
          :backend "vulpea"
          :note note)))

(defun acm-backend-vulpea-candidates (keyword)
  "Return Vulpea note candidates whose title matches KEYWORD.
Return nil unless `acm-enable-vulpea' is non-nil and vulpea is loaded.
The titles are fuzzy-matched against KEYWORD, the ones starting with it
come first and at most `acm-backend-vulpea-candidates-number' of them are
returned."
  (when (and acm-enable-vulpea
             (featurep 'vulpea))
    (let* ((notes (vulpea-db-query
                   (lambda (note)
                     (acm-candidate-fuzzy-search keyword (vulpea-note-title note)))))
           (candidates (mapcar #'acm-backend-vulpea--candidate notes)))
      (cl-subseq (acm-candidate-sort-by-prefix keyword candidates)
                 0 (min (length candidates) acm-backend-vulpea-candidates-number)))))

(defun acm-backend-vulpea--link-region (bound-start)
  "Return the buffer region the inserted link should replace.
BOUND-START is the beginning of the typed prefix.  The region is the
whole `[[' bracket construct around point when point is inside one, and
the text from BOUND-START to point otherwise."
  (or (org-in-regexp acm-backend-vulpea-bracket-re 1)
      (cons bound-start (point))))

(defun acm-backend-vulpea-candidate-expand (candidate-info bound-start)
  "Replace the brackets at point with the Vulpea link of CANDIDATE-INFO.
The link is built by `vulpea-utils-link-make-string', so it is an id: link
to the note; the label alone is inserted when the candidate carries no
note.  BOUND-START is the beginning of the typed prefix and is only used
when point is not inside brackets.  Point ends up after the link."
  (let* ((note (plist-get candidate-info :note))
         (region (acm-backend-vulpea--link-region bound-start))
         (link (if note
                   (vulpea-utils-link-make-string note)
                 (plist-get candidate-info :label))))
    (delete-region (car region) (cdr region))
    (insert link)))

(provide 'acm-backend-vulpea)

;;; acm-backend-vulpea.el ends here
