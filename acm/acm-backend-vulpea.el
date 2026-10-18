;;; acm-backend-vulpea.el --- acm vulpea support -*- lexical-binding: t -*-

;;; Commentary:

;; Vulpea is the note database this tree manages notes with: `vulpea-db-query'
;; returns `vulpea-note' structures and `vulpea-utils-link-make-string' builds
;; the id: link of a note.  This backend offers the notes while point is inside
;; `[[' brackets and inserts the link of the selected one, so a note can be
;; linked from any buffer without leaving it.
;;
;; What is offered, and how, follows Vulpea itself:
;;
;; - a note is offered under its title *and* under each of its aliases, because
;;   both are how the note is referred to (`vulpea-note-expand-aliases' is what
;;   `vulpea-select' and the `vulpea-capf' package use for the same reason);
;; - headings that are not notes (no id) are skipped, since there is nothing to
;;   link to;
;; - the row is annotated by `vulpea-select-annotate', which is the annotation
;;   of Vulpea's own selection UI: the tags, and the primary title of a note
;;   that is being picked by one of its aliases;
;; - the inserted link is `[[id:UUID][name]]', where name is the title or the
;;   alias that was picked, not the primary title of the note.
;;
;; Difference to the `vulpea-capf' package: that one is a set of
;; `completion-at-point' functions, so it needs a completion UI to surface
;; them, and its bracket capf only fires after the closing `]]' has been typed.
;; This backend is an acm backend, so the menu pops up as soon as `[[' is typed,
;; and the expansion replaces the whole bracket construct.  The bare-word
;; completion that `vulpea-capf' offers ("AWS" anywhere in an Org buffer) is not
;; duplicated here: install that package if you want it, and let the acm capf
;; backend surface it by adding `org-mode' to `acm-backend-capf-mode-list'.
;;
;; Enabled with `acm-enable-vulpea'.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)

(declare-function acm-candidate-fuzzy-search "acm" (keyword candidate))
(declare-function acm-candidate-sort-by-prefix "acm" (keyword candidates))
(declare-function vulpea-db-query "vulpea-db-query" (&optional predicate))
(declare-function vulpea-note-aliases "vulpea-note" (note))
(declare-function vulpea-note-expand-aliases "vulpea-note" (note))
(declare-function vulpea-note-id "vulpea-note" (note))
(declare-function vulpea-note-title "vulpea-note" (note))
(declare-function vulpea-select-annotate "vulpea-select" (note))
(declare-function vulpea-utils-link-make-string "vulpea-utils" (note &optional description))

(defgroup acm-backend-vulpea nil
  "Vulpea backend for acm."
  :group 'acm)

(defcustom acm-enable-vulpea nil
  "Popup Vulpea note completions when this option is turn on.
The backend reads the notes from the Vulpea database with
`vulpea-db-query', so vulpea has to be loaded and its database built."
  :type 'boolean
  :group 'acm-backend-vulpea)

(defcustom acm-backend-vulpea-candidates-number 10
  "Maximal number of Vulpea candidates in the menu."
  :type 'integer
  :group 'acm-backend-vulpea)

(defvar acm-backend-vulpea-bracket-re
  (rx "[[" (group-n 1 (optional "id:"))
      (group-n 2 (zero-or-more
                  (or (not (any "[]\\"))
                      (and "\\" (zero-or-more "\\\\") (any "[]"))
                      (and (one-or-more "\\") (not (any "[]"))))))
      (optional "]]"))
  "Regexp matching the `[[' link brackets of a Vulpea note link.
Group 1 is the optional \"id:\" prefix and group 2 is the text typed inside
the brackets, in which escaped brackets are allowed - the same shape the
`vulpea-capf' package matches.  The closing brackets are optional, so the
menu also pops up while they are still being typed; when they are already
there, the expansion replaces them together with the typed text.")

(defun acm-backend-vulpea--annotation (note)
  "Return the annotation Vulpea itself shows for NOTE.
`vulpea-select-annotate' returns the tags of the note and, for a note that
is picked by one of its aliases, the primary title in parentheses.  When
there is nothing to show, the name of the backend is used instead, so that
a candidate always says where it comes from."
  (let ((annotation (vulpea-select-annotate note)))
    (if (or (null annotation) (string-empty-p annotation))
        "Vulpea"
      annotation)))

(defun acm-backend-vulpea--candidate (note)
  "Return the completion candidate of the (possibly alias) NOTE.
`:label' is the title or the alias the note is picked by, which is also the
description of the link that gets inserted."
  (let ((title (vulpea-note-title note)))
    (list :key title
          :icon "note"
          :label title
          :displayLabel title
          :annotation (acm-backend-vulpea--annotation note)
          :backend "vulpea"
          :note note)))

(defun acm-backend-vulpea--candidates-of-note (note)
  "Return the candidates of NOTE: one for its title, one per alias.
Return nil for a heading that is not a note - it has no id and therefore
cannot be linked to.  Aliases are expanded with
`vulpea-note-expand-aliases', so an alias candidate carries the note with
its `vulpea-note-primary-title' set."
  (when (vulpea-note-id note)
    (mapcar #'acm-backend-vulpea--candidate (vulpea-note-expand-aliases note))))

(defun acm-backend-vulpea--match-p (keyword note)
  "Return non-nil when KEYWORD matches the title or an alias of NOTE."
  (or (acm-candidate-fuzzy-search keyword (vulpea-note-title note))
      (seq-some (lambda (alias)
                  (acm-candidate-fuzzy-search keyword alias))
                (vulpea-note-aliases note))))

(defun acm-backend-vulpea-candidates (keyword)
  "Return Vulpea note candidates whose title or alias matches KEYWORD.
Return nil unless `acm-enable-vulpea' is non-nil and vulpea is loaded.
Titles and aliases are fuzzy-matched against KEYWORD, the candidates
starting with it come first, and at most
`acm-backend-vulpea-candidates-number' of them are returned."
  (when (and acm-enable-vulpea
             (featurep 'vulpea))
    (let* ((notes (vulpea-db-query
                   (lambda (note)
                     (acm-backend-vulpea--match-p keyword note))))
           (candidates (seq-mapcat #'acm-backend-vulpea--candidates-of-note notes))
           ;; A note is offered under its title and under each alias, but only
           ;; the names matching the input are kept - the same filtering a
           ;; completion UI does in front of the `vulpea-capf' candidate table.
           ;; Without it, typing an alias would drag the title of its note in.
           (matching (seq-filter (lambda (candidate)
                                   (acm-candidate-fuzzy-search
                                    keyword (plist-get candidate :label)))
                                 candidates)))
      (cl-subseq (acm-candidate-sort-by-prefix keyword matching)
                 0 (min (length matching) acm-backend-vulpea-candidates-number)))))

(defun acm-backend-vulpea--link-region (bound-start)
  "Return the buffer region the inserted link should replace.
BOUND-START is the beginning of the typed prefix.  The region is the
whole `[[' bracket construct around point when point is inside one, and
the text from BOUND-START to point otherwise."
  (or (org-in-regexp acm-backend-vulpea-bracket-re 1)
      (cons bound-start (point))))

(defun acm-backend-vulpea-candidate-expand (candidate-info bound-start)
  "Replace the brackets at point with the Vulpea link of CANDIDATE-INFO.
The link is built by `vulpea-utils-link-make-string' with the label of the
candidate - its title, or the alias it was picked by - as the description,
so it is an id: link.  The label alone is inserted when the candidate
carries no note.  BOUND-START is the beginning of the typed prefix and is
only used when point is not inside brackets.  Point ends up after the
link."
  (let* ((note (plist-get candidate-info :note))
         (label (plist-get candidate-info :label))
         (region (acm-backend-vulpea--link-region bound-start))
         (link (if note
                   (vulpea-utils-link-make-string note label)
                 label)))
    (delete-region (car region) (cdr region))
    (insert link)))

(provide 'acm-backend-vulpea)
;;; acm-backend-vulpea.el ends here
