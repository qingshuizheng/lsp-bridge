;;; check-no-byte-compile.el --- Variables used without being declared  -*- lexical-binding: t; -*-

;;; Commentary:
;; Eighteen files of this package carry `no-byte-compile: t' in their header, so
;; the compiler never looks at them and nothing tells their author about a variable
;; that is read or written without ever being declared.  This script copies them
;; with that marker taken out and compiles the copies, then prints the warnings
;; that name something of this package which is declared nowhere and is not read
;; behind a `boundp' guard either - which is the shape of a variable that is only
;; there because a `setq-local' happened to create it first.
;;
;; Run as: emacs -Q --batch -L . -L acm -l scripts/check-no-byte-compile.el [DIR]
;;
;; What it ignores, and why: names of functions and variables declared in other
;; files of the package, which a file compiled on its own cannot know; names of
;; other packages, which are not loaded here; and names tested with `boundp'
;; before they are read, which is a guard the compiler cannot follow and which is
;; how the candidate caches of the backends are written.

;;; Code:

(require 'cl-lib)

(defconst nbc--package-prefixes '("lsp-bridge-" "acm-")
  "Prefixes of the names that belong to this package.")

(defun nbc--files (directory)
  "Return the Emacs Lisp files of DIRECTORY, recursively."
  (directory-files-recursively directory "\\.el\\'"))

(defconst nbc--known
  '(;; The frame of the code action menu, and neither of the two things the file
    ;; that uses it takes it for.  Read as a variable in thirteen places - passing it
    ;; to `frame-live-p', `frame-position', `window-list' and the like, which want a
    ;; frame - and passed as a slot name to `acm-frame-delete-frame' and the other
    ;; macros of acm-frame, whose slots are those of an `acm-session': trying that
    ;; gives `cl-struct-unknown-slot acm-session lsp-bridge-call-hierarchy--frame',
    ;; seen while this was looked into.  Nothing declares the name either, so the
    ;; reads are of an unbound variable.  Which of the two it should become, a
    ;; declared variable holding a frame or a popup of acm's own, is a decision to
    ;; make with a display to try the menu on; until then this check keeps any *new*
    ;; name of this shape from being added unnoticed.
    "lsp-bridge-call-hierarchy--frame")
  "Names of this shape that are known and have not been dealt with yet.")

(defun nbc--where-used (files)
  "Return a hash from each name FILES mention to the files that mention it.
A name that is mentioned by more than one file is declared or used somewhere
else of the package, which a file compiled on its own cannot know about.  What
is left is the shape of a variable that exists only because a `setq-local'
created it."
  (let ((used (make-hash-table :test 'equal)))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "[a-zA-Z][a-zA-Z0-9-]*" nil t)
          (let ((name (match-string 0)))
            (when (cl-some (lambda (prefix) (string-prefix-p prefix name)) nbc--package-prefixes)
              (puthash name (cons file (gethash name used)) used))))))
    used))

(defun nbc--guarded (files)
  "Return a hash of the names that FILES read only behind a guard.
That is a name tested with `boundp', and a name passed to
`acm-with-cache-candidates', which is the macro that tests its argument with
`boundp' before reading it."
  (let ((guarded (make-hash-table :test 'equal)))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(\\s-*bound\\(?:p\\|-and-true-p\\)\\s-+'\\?\\s-*\\([^ \t\n()]+\\)" nil t)
          (puthash (match-string 1) t guarded))
        (goto-char (point-min))
        (while (re-search-forward "(\\s-*acm-with-cache-candidates\\s-+\\([^ \t\n()]+\\)" nil t)
          (puthash (match-string 1) t guarded))))
    guarded))

(defun nbc--original (copy scratch root)
  "Return the file of ROOT that COPY, which lives in SCRATCH, was copied from."
  (expand-file-name (file-relative-name copy scratch) root))

(defun nbc--compile (file)
  "Compile FILE and return its warnings as a list of (LINE . MESSAGE).
`byte-compile-warnings' is left alone: it is one of Emacs' own variables, it
reports everything by default, and reading it before the compiler is loaded is a
void variable rather than a value to save."
  (with-current-buffer (get-buffer-create "*Compile-Log*") (erase-buffer))
  (byte-compile-file file)
  (let ((warnings nil))
    (with-current-buffer "*Compile-Log*"
      (goto-char (point-min))
      (while (re-search-forward "^.+:\\([0-9]+\\):[0-9]+: Warning: \\(.+\\)$" nil t)
        (push (cons (string-to-number (match-string 1)) (match-string 2)) warnings)))
    (nreverse warnings)))

(defun nbc--name (message)
  "Return the name MESSAGE is about, when MESSAGE is about a free variable.
Only the two warnings about one are of interest: a warning about a docstring or
a custom type names something that its own declaration declares."
  (when (string-match "\\(?:reference to free variable\\|assignment to free variable\\) ‘\\([^’]+\\)’" message)
    (let ((name (match-string 1 message)))
      (when (cl-some (lambda (prefix) (string-prefix-p prefix name)) nbc--package-prefixes)
        name))))

(let* ((root (or (car command-line-args-left) "."))
       (files (nbc--files root))
       (used (nbc--where-used files))
       (guarded (nbc--guarded files))
       (marked (cl-remove-if-not (lambda (file)
                                   (with-temp-buffer
                                     (insert-file-contents file)
                                     (goto-char (point-min))
                                     (search-forward "no-byte-compile: t" nil t)))
                                 files))
       (scratch (make-temp-file "nbc-check" t))
       (found 0))
  (dolist (file marked)
    (let ((copy (expand-file-name (file-relative-name file root) scratch)))
      (make-directory (file-name-directory copy) t)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (search-forward "no-byte-compile: t" nil t)
          (replace-match "no-byte-compile: nil"))
        (write-region (point-min) (point-max) copy nil 'silent))))
  (dolist (copy (nbc--files scratch))
    (dolist (warning (nbc--compile copy))
      (let ((name (nbc--name (cdr warning))))
        (when (and name
                   (not (member name nbc--known))
                   ;; Only the file it was found in mentions it, and nothing
                   ;; tests it with `boundp' before reading it.
                   (equal (delete-dups (copy-sequence (gethash name used)))
                          (list (nbc--original copy scratch root)))
                   (not (gethash name guarded)))
          (setq found (1+ found))
          (princ (format "  %s:%d: %s\n"
                         (file-relative-name copy scratch) (car warning) (cdr warning)))))))
  (delete-directory scratch t)
  (if (zerop found)
      (princ (format "  ✓ %d files that ask not to be compiled: nothing used but undeclared\n"
                     (length marked)))
    (princ (format "  ✗ %d name(s) used but neither declared nor guarded\n" found)))
  (kill-emacs (if (zerop found) 0 1)))

;;; check-no-byte-compile.el ends here
