;;; acm-backend-vulpea-test.el --- Tests for the Vulpea backend  -*- lexical-binding: t; -*-

;;; Commentary:

;; The backend reads the note database through `vulpea-db-query' and builds its
;; links with `vulpea-utils-link-make-string'.  Vulpea is a package of its own
;; (and its database is a file on disk), so these tests provide the feature and
;; replace those functions with the note plists below: what is under test is the
;; candidate building and the bracket expansion, not the database.
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

(defun acm-backend-vulpea-test--note (title id)
  "Return a note with TITLE and ID, shaped like a `vulpea-note'."
  (list :title title :id id))

(defun vulpea-db-query (&optional filter-fn)
  "Return the stub notes of the current test that satisfy FILTER-FN."
  (seq-filter (or filter-fn #'identity) acm-backend-vulpea-test-notes))

(defun vulpea-note-title (note)
  "Return the title of NOTE."
  (plist-get note :title))

(defun vulpea-note-id (note)
  "Return the id of NOTE."
  (plist-get note :id))

(defun vulpea-utils-link-make-string (note &optional description)
  "Return the id: link of NOTE, the way vulpea builds it.
DESCRIPTION, when given, replaces the title of the note."
  (format "[[id:%s][%s]]"
          (vulpea-note-id note)
          (or description (vulpea-note-title note))))

(defun acm-backend-vulpea-test--labels (candidates)
  "Return the labels of CANDIDATES."
  (mapcar (lambda (candidate) (plist-get candidate :label)) candidates))

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
      (should (equal (plist-get (car candidates) :annotation) "Vulpea"))
      (should (equal (plist-get (car candidates) :backend) "vulpea"))
      (should (equal (plist-get (car candidates) :key) "Alpha"))
      ;; The note travels with the candidate, so expanding it needs no query.
      (should (equal (vulpea-note-id (plist-get (car candidates) :note)) "a")))
    (let ((acm-backend-vulpea-candidates-number 1))
      (should (equal (acm-backend-vulpea-test--labels
                      (acm-backend-vulpea-candidates "Al"))
                     '("Alpha"))))
    ;; A keyword that matches nothing yields no candidate.
    (should-not (acm-backend-vulpea-candidates "nothing-matches-this"))))

;;; The brackets

(ert-deftest acm-vulpea-bracket-detection ()
  "Point inside `[[' brackets is detected, point before them is not."
  (with-temp-buffer
    (org-mode)
    (let ((acm-enable-vulpea t))
      (insert "[[Al")
      (should (acm-in-vulpea-bracket-p))
      (should (equal (acm-get-input-prefix) "Al")))
    (with-temp-buffer
      (org-mode)
      (let ((acm-enable-vulpea t))
        (insert "before [[Al")
        (goto-char (point-min))
        (forward-char 7)                ; right before the brackets
        (should-not (acm-in-vulpea-bracket-p))))))

(ert-deftest acm-vulpea-input-prefix-bound ()
  "The \"vulpea\" input bound is the text typed inside the brackets."
  (with-temp-buffer
    (org-mode)
    (insert "[[Al")
    (let* ((acm-input-bound-style "vulpea")
           (bound (acm-get-input-prefix-bound)))
      (should bound)
      (should (equal (buffer-substring-no-properties (car bound) (cdr bound))
                     "Al")))))

;;; Expansion

(ert-deftest acm-backend-vulpea-expand-inserts-id-link ()
  "Expanding replaces the typed brackets with the id: link of the note."
  (with-temp-buffer
    (org-mode)
    (insert "[[Al")
    (let* ((note (acm-backend-vulpea-test--note "Alpha" "a"))
           (candidate (list :label "Alpha" :note note))
           (bound-start 3))             ; start of "Al"
      (acm-backend-vulpea-candidate-expand candidate bound-start)
      (should (equal (buffer-string) "[[id:a][Alpha]]"))
      (should (eobp)))))

(ert-deftest acm-backend-vulpea-expand-replaces-closing-brackets ()
  "Closing brackets that are already typed are replaced as well."
  (with-temp-buffer
    (org-mode)
    (insert "[[Al]]")
    (goto-char 5)                       ; after "Al", before "]]"
    (let* ((note (acm-backend-vulpea-test--note "Alpha" "a"))
           (candidate (list :label "Alpha" :note note)))
      (acm-backend-vulpea-candidate-expand candidate 3)
      (should (equal (buffer-string) "[[id:a][Alpha]]")))))

(ert-deftest acm-backend-vulpea-expand-without-note-inserts-label ()
  "A candidate without a note only inserts its label."
  (with-temp-buffer
    (org-mode)
    (insert "[[Al")
    (acm-backend-vulpea-candidate-expand (list :label "Alpha") 3)
    (should (equal (buffer-string) "Alpha"))))

(provide 'acm-backend-vulpea-test)
;;; acm-backend-vulpea-test.el ends here
