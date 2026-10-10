;;; acm-backend-vulpea-test.el --- Tests for the Vulpea backend  -*- lexical-binding: t; -*-

;;; Commentary:

;; The backend reads the note database through `vulpea-db-query', expands the
;; aliases of a note with `vulpea-note-expand-aliases' and builds its links with
;; `vulpea-utils-link-make-string'.  Vulpea is a package of its own and its
;; database is a file, so these tests provide the feature and stand in for those
;; functions with the note plists below: what is under test is the candidate
;; building and the bracket expansion, not the database.
;;
;; The stubs follow the semantics of the functions they replace (vulpea 2.7):
;; `vulpea-note-expand-aliases' returns the note itself followed by one copy per
;; alias with `primary-title' set, and `vulpea-select-annotate' returns the
;; primary title of an alias candidate in parentheses plus the tags.
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/acm-backend-vulpea-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'acm)
(require 'acm-backend-vulpea)
(require 'org)

;;; A stand-in for the vulpea API

(defvar acm-backend-vulpea-test-notes nil
  "Notes the stubbed `vulpea-db-query' answers with.")

;; The backend only runs when vulpea is loaded, which is what this provides.
(provide 'vulpea)

(defun acm-backend-vulpea-test--note (title id &optional aliases tags)
  "Return a note with TITLE, ID, ALIASES and TAGS, like a `vulpea-note'."
  (list :title title :id id :aliases aliases :tags tags :primary-title nil))

(defun vulpea-db-query (&optional predicate)
  "Return the stub notes of the current test that satisfy PREDICATE."
  (seq-filter (or predicate #'identity) acm-backend-vulpea-test-notes))

(defun vulpea-note-title (note)
  "Return the title of NOTE."
  (plist-get note :title))

(defun vulpea-note-id (note)
  "Return the id of NOTE."
  (plist-get note :id))

(defun vulpea-note-aliases (note)
  "Return the aliases of NOTE."
  (plist-get note :aliases))

(defun vulpea-note-primary-title (note)
  "Return the primary title of NOTE, set for an alias candidate."
  (plist-get note :primary-title))

(defun vulpea-note-tags (note)
  "Return the tags of NOTE."
  (plist-get note :tags))

(defun vulpea-note-expand-aliases (note)
  "Return NOTE followed by one copy per alias, as vulpea does.
The copies carry the alias as title and the original title as
`primary-title'."
  (cons note
        (mapcar (lambda (alias)
                  (list :title alias
                        :id (vulpea-note-id note)
                        :aliases nil
                        :tags (vulpea-note-tags note)
                        :primary-title (vulpea-note-title note)))
                (vulpea-note-aliases note))))

(defun vulpea-select-annotate (note)
  "Annotate NOTE the way vulpea's selection UI does.
The primary title is shown in parentheses for an alias candidate, followed
by the tags."
  (let ((sections (seq-remove #'string-empty-p
                              (list (if (vulpea-note-primary-title note)
                                        (format "(%s)" (vulpea-note-primary-title note))
                                      "")
                                    (mapconcat (lambda (tag) (concat "#" tag))
                                               (vulpea-note-tags note)
                                               " ")))))
    (if sections (concat " " (string-join sections " ")) "")))

(defun vulpea-utils-link-make-string (note &optional description)
  "Return the id: link of NOTE, the way vulpea builds it.
DESCRIPTION, when given, replaces the title of the note."
  (format "[[id:%s][%s]]"
          (vulpea-note-id note)
          (or description (vulpea-note-title note))))

(defun acm-backend-vulpea-test--labels (candidates)
  "Return the labels of CANDIDATES."
  (mapcar (lambda (candidate) (plist-get candidate :label)) candidates))

(defmacro acm-backend-vulpea-test--with-org (text &rest body)
  "Run BODY in a temporary Org buffer that holds TEXT and enables the backend.
Point starts at the end of TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (let ((acm-enable-vulpea t))
       (insert ,text)
       ,@body)))

;;; Candidates

(ert-deftest acm-backend-vulpea-candidates-need-the-option ()
  "Nothing is offered while the backend is turned off."
  (let ((acm-enable-vulpea nil)
        (acm-backend-vulpea-test-notes
         (list (acm-backend-vulpea-test--note "Alpha" "a"))))
    (should-not (acm-backend-vulpea-candidates "Al"))))

(ert-deftest acm-backend-vulpea-candidates-filter-sort-and-limit ()
  "Titles are matched, prefix matches come first, and the list is limited."
  (let ((acm-enable-vulpea t)
        (acm-candidate-match-function 'regexp-quote)
        (acm-backend-vulpea-test-notes
         (list (acm-backend-vulpea-test--note "Zettel" "z")
               (acm-backend-vulpea-test--note "Alpha" "a")
               (acm-backend-vulpea-test--note "Alphabet" "b"))))
    (let ((candidates (acm-backend-vulpea-candidates "Al")))
      (should (equal (acm-backend-vulpea-test--labels candidates)
                     '("Alpha" "Alphabet")))
      (should (equal (plist-get (car candidates) :backend) "vulpea"))
      (should (equal (plist-get (car candidates) :key) "Alpha"))
      ;; Without tags or an alias there is nothing to annotate, so the
      ;; candidate only names its backend.
      (should (equal (plist-get (car candidates) :annotation) "Vulpea"))
      ;; The note travels with the candidate, so expanding it needs no query.
      (should (equal (vulpea-note-id (plist-get (car candidates) :note)) "a")))
    (let ((acm-backend-vulpea-candidates-number 1))
      (should (equal (acm-backend-vulpea-test--labels
                      (acm-backend-vulpea-candidates "Al"))
                     '("Alpha"))))
    ;; A keyword that matches nothing yields no candidate.
    (should-not (acm-backend-vulpea-candidates "nothing-matches-this"))))

(ert-deftest acm-backend-vulpea-candidates-expand-aliases ()
  "A note is offered under its title and under each of its aliases."
  (let ((acm-enable-vulpea t)
        (acm-candidate-match-function 'regexp-quote)
        (acm-backend-vulpea-test-notes
         (list (acm-backend-vulpea-test--note "Zettelkasten" "z" '("Slip box") '("notes")))))
    ;; The alias is a candidate of its own ...
    (let ((candidates (acm-backend-vulpea-candidates "Slip")))
      (should (equal (acm-backend-vulpea-test--labels candidates) '("Slip box")))
      ;; ... annotated with the primary title and the tags, like vulpea's UI.
      (should (equal (plist-get (car candidates) :annotation)
                     " (Zettelkasten) #notes"))
      (should (equal (vulpea-note-id (plist-get (car candidates) :note)) "z")))
    ;; Only the name that matches is offered: typing an alias does not drag
    ;; the title of its note into the menu (and the other way round).
    (should (equal (acm-backend-vulpea-test--labels
                    (acm-backend-vulpea-candidates "Zettel"))
                   '("Zettelkasten")))
    (should (equal (acm-backend-vulpea-test--labels
                    (acm-backend-vulpea-candidates "slip"))
                   '("Slip box")))
    (should (equal (acm-backend-vulpea-test--labels
                    (acm-backend-vulpea-candidates "z"))
                   '("Zettelkasten")))))

(ert-deftest acm-backend-vulpea-candidates-skip-non-notes ()
  "A heading without an id is not a note and cannot be linked to."
  (let ((acm-enable-vulpea t)
        (acm-candidate-match-function 'regexp-quote)
        (acm-backend-vulpea-test-notes
         (list (acm-backend-vulpea-test--note "Section" nil)
               (acm-backend-vulpea-test--note "Note" "n"))))
    (should (equal (acm-backend-vulpea-test--labels
                    (acm-backend-vulpea-candidates ""))
                   '("Note")))))

;;; The brackets

(ert-deftest acm-vulpea-bracket-detection ()
  "Point inside `[[' brackets is detected, point before them is not."
  (acm-backend-vulpea-test--with-org "[[Al"
    (should (acm-in-vulpea-bracket-p))
    (should (equal (acm-get-input-prefix) "Al")))
  (acm-backend-vulpea-test--with-org "[[Al]]"
    (goto-char 4)
    (should (acm-in-vulpea-bracket-p)))
  (acm-backend-vulpea-test--with-org "before [[Al"
    (goto-char (point-min))
    (forward-char 7)                    ; right before the brackets
    (should-not (acm-in-vulpea-bracket-p))))

(ert-deftest acm-vulpea-bracket-detection-with-escaped-bracket ()
  "An escaped bracket inside the link does not end the brackets.
This is the case the `vulpea-capf' bracket regexp handles as well."
  (acm-backend-vulpea-test--with-org "[[a \\] b"
    (should (acm-in-vulpea-bracket-p))
    (should (equal (acm-get-input-prefix) "a \\] b"))))

(ert-deftest acm-vulpea-input-prefix-bound ()
  "The \"vulpea\" input bound is the text typed inside the brackets."
  (acm-backend-vulpea-test--with-org "[[Al"
    (let* ((acm-input-bound-style "vulpea")
           (bound (acm-get-input-prefix-bound)))
      (should bound)
      (should (equal (buffer-substring-no-properties (car bound) (cdr bound))
                     "Al")))))

;;; Expansion

(ert-deftest acm-backend-vulpea-expand-inserts-id-link ()
  "Expanding replaces the typed brackets with the id: link of the note."
  (acm-backend-vulpea-test--with-org "[[Al"
    (let* ((note (acm-backend-vulpea-test--note "Alpha" "a"))
           (candidate (list :label "Alpha" :note note))
           (bound-start 3))             ; start of "Al"
      (acm-backend-vulpea-candidate-expand candidate bound-start)
      (should (equal (buffer-string) "[[id:a][Alpha]]"))
      (should (eobp)))))

(ert-deftest acm-backend-vulpea-expand-uses-the-alias-as-description ()
  "A note picked by an alias is linked with that alias as its description."
  (let ((acm-enable-vulpea t)
        (acm-candidate-match-function 'regexp-quote)
        (acm-backend-vulpea-test-notes
         (list (acm-backend-vulpea-test--note "Zettelkasten" "z" '("Slip box")))))
    (acm-backend-vulpea-test--with-org "[[Sl"
      (let ((candidate (car (acm-backend-vulpea-candidates "Slip"))))
        (should (equal (plist-get candidate :label) "Slip box"))
        ;; The candidate carries the expanded note, not the primary one.
        (should (equal (vulpea-note-title (plist-get candidate :note)) "Slip box"))
        (should (equal (vulpea-note-primary-title (plist-get candidate :note))
                       "Zettelkasten"))
        (acm-backend-vulpea-candidate-expand candidate 3)
        (should (equal (buffer-string) "[[id:z][Slip box]]"))))))

(ert-deftest acm-backend-vulpea-expand-replaces-closing-brackets ()
  "Closing brackets that are already typed are replaced as well."
  (acm-backend-vulpea-test--with-org "[[Al]]"
    (goto-char 5)                       ; after "Al", before "]]"
    (let* ((note (acm-backend-vulpea-test--note "Alpha" "a"))
           (candidate (list :label "Alpha" :note note)))
      (acm-backend-vulpea-candidate-expand candidate 3)
      (should (equal (buffer-string) "[[id:a][Alpha]]")))))

(ert-deftest acm-backend-vulpea-expand-without-note-inserts-label ()
  "A candidate without a note only inserts its label."
  (acm-backend-vulpea-test--with-org "[[Al"
    (acm-backend-vulpea-candidate-expand (list :label "Alpha") 3)
    (should (equal (buffer-string) "Alpha"))))

(provide 'acm-backend-vulpea-test)
;;; acm-backend-vulpea-test.el ends here
