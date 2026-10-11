;;; acm-backend.el --- Registry and candidate protocol of the backends -*- lexical-binding: t -*-

;;; Commentary:

;; A backend is a named source of completion candidates.  This file is the
;; single place that knows which backends exist: `acm-backend-register' adds
;; one, and the menu asks them for candidates, links and documentation through
;; `acm-backends-fetch', `acm-backend-expand' and `acm-backend-doc' instead of
;; calling `acm-backend-<name>-...' by naming convention.
;;
;; A registration is an `acm-backend' structure; its docstring describes every
;; field.  The rules that used to sit inline in `acm-update-candidates' - which
;; option enables a backend, which backend must not run inside a comment, whose
;; candidates are dropped when the language server already returned the same
;; label, which backend replaces all the others when it has anything to offer -
;; are fields of that structure now, so a new backend is data, not a new branch
;; in the menu code.
;;
;; The candidate protocol
;; ----------------------
;; A candidate is a plist.  The menu itself reads these keys:
;;
;;   :label         required.  What the input is matched against, what the menu
;;                  shows and what is inserted when the backend has no expand
;;                  function.
;;   :backend       required.  The name of the backend that produced it; the
;;                  expand and doc functions are looked up with it.
;;   :key           stable identity of the candidate, used to keep the
;;                  selection while a backend returns a new list.  Defaults to
;;                  the label.
;;   :displayLabel  shown instead of the label when present.
;;   :icon          icon name looked up in `acm-icon-alist'.
;;   :annotation    extra text shown at the end of the candidate line.
;;
;; Anything else a candidate carries belongs to its backend: it is passed back
;; untouched to the expand and doc functions (the Vulpea backend keeps its
;; `vulpea-note' in :note, the LSP backend its `insertText' and text edits).
;; `acm-candidate-make' fills the defaults and `acm-candidates-normalize'
;; applies it to a whole list, dropping anything that has no label.

;;; Code:

(require 'cl-lib)
(require 'acm-backend-capf)
(require 'acm-backend-copilot)
(require 'acm-backend-ctags)
(require 'acm-backend-elisp)
(require 'acm-backend-jupyter)
(require 'acm-backend-lsp)
(require 'acm-backend-devonthink)
(require 'acm-backend-lsp-workspace-symbol)
(require 'acm-backend-path)
(require 'acm-backend-search-file-words)
(require 'acm-backend-tabby)
(require 'acm-backend-telega)
(require 'acm-backend-tempel)
(require 'acm-backend-vulpea)
(require 'acm-backend-yas)

(declare-function acm-in-comment-p "acm")
(declare-function acm-in-vulpea-bracket-p "acm")
(declare-function acm-remove-duplicate-candidates "acm" (candidates lsp-labels))

(cl-defstruct acm-backend
  "One registered completion backend.

Slots:
  name              The string candidates carry in their :backend key, and
                    that the menu displays as the source of a candidate.
  group             The candidate group the backend contributes to, such as
                    \"lsp-candidates\"; nil when its candidates are not merged
                    through `acm-completion-mode-candidates-merge-order'.
  fetch-fn          Symbol of a function taking the input prefix and returning
                    the candidate list.
  expand-fn         Symbol of a function taking a candidate, the start of the
                    prefix and, when `preview' is non-nil, a preview flag; it
                    inserts the candidate and returns an overlay for previews.
  doc-fn            Symbol of a function taking a candidate and returning its
                    documentation, or nil when the backend has none.
  clean-fn          Symbol of a function of no arguments that drops the cached
                    state of the backend when the menu is hidden.
  preview           Non-nil when `expand' understands the preview argument.
  enabled           Function of no arguments deciding whether the backend may
                    run at all, or nil for a backend that is always asked.
  skip-in-comment   Non-nil when the backend must not be asked while point is
                    inside a comment.
  dedupe-against-lsp  Non-nil when candidates whose label the LSP backend
                    already returned are dropped from its group.
  short-circuit     Non-nil when the backend stops the fetching as soon as it
                    returns candidates, because anything else would be noise
                    (the path backend: the user is typing a file name).
  template          Non-nil for the template backends, which the menu collects
                    with its own ticker logic instead of the group order."
  (name nil)
  (group nil)
  (fetch-fn nil)
  (expand-fn nil)
  (doc-fn nil)
  (clean-fn nil)
  (preview nil)
  (enabled nil)
  (skip-in-comment nil)
  (dedupe-against-lsp nil)
  (short-circuit nil)
  (template nil))

(defvar acm-backends nil
  "The registered backends, as a list of `acm-backend' structures.
The order is the order in which they are asked for candidates, so a backend
that uses `short-circuit' has to come before the ones it saves from being
asked.  The order in which the candidates are merged is a separate thing:
`acm-completion-mode-candidates-merge-order' and `acm-backend-order'.")

(defun acm-backend-register (backend)
  "Register BACKEND, replacing a registration with the same name.
BACKEND is an `acm-backend' structure; a third-party package can build one
with `make-acm-backend' and add itself to the menu this way."
  (let ((name (acm-backend-name backend))
        (replaced nil))
    (setq acm-backends
          (mapcar (lambda (registered)
                    (if (equal (acm-backend-name registered) name)
                        (progn (setq replaced t) backend)
                      registered))
                  acm-backends))
    (unless replaced
      (setq acm-backends (append acm-backends (list backend))))))

(defun acm-backend-find (name)
  "Return the registered backend called NAME, or nil."
  (cl-find name acm-backends :key #'acm-backend-name :test #'equal))

(defun acm-backend-names ()
  "Return the names of the registered backends, in fetch order."
  (mapcar #'acm-backend-name acm-backends))

(defun acm-candidate-make (&rest plist)
  "Return a candidate made of PLIST, with the protocol defaults filled in.
PLIST needs a `:label' and a `:backend'; `:key' and `:displayLabel' default
to the label.  Signalling an error on a candidate without a label is the
point: a candidate the user cannot see or match must not reach the menu."
  (let ((label (plist-get plist :label))
        (backend (plist-get plist :backend)))
    (unless (stringp label)
      (error "Candidate without a :label: %S" plist))
    (unless (stringp backend)
      (error "Candidate without a :backend: %S" plist))
    (unless (plist-get plist :key)
      (setq plist (plist-put plist :key label)))
    (unless (plist-get plist :displayLabel)
      (setq plist (plist-put plist :displayLabel label)))
    plist))

(defun acm-candidates-normalize (candidates backend)
  "Return CANDIDATES with the protocol defaults of BACKEND filled in.
CANDIDATES is what a backend returned; the entries that are not candidate
plists, or that have no label, are dropped instead of reaching the menu.
The `:backend' key of every candidate that lacks one is set to BACKEND, so a
backend that returns bare candidates still gets its expand function called."
  (delq nil
        (mapcar (lambda (candidate)
                  (when (and (consp candidate)
                             (stringp (plist-get candidate :label)))
                    (apply #'acm-candidate-make
                           (if (plist-get candidate :backend)
                               candidate
                             (plist-put candidate :backend backend)))))
                candidates)))

(defun acm-backend-fetch (backend keyword)
  "Return the candidates BACKEND offers for the input KEYWORD.
Return nil when the backend is disabled - `enabled' returns nil - or when it
must not run inside a comment and point is in one.  The result goes through
`acm-candidates-normalize'."
  (when (and (or (null (acm-backend-enabled backend))
                 (funcall (acm-backend-enabled backend)))
             (or (null (acm-backend-skip-in-comment backend))
                 (not (acm-in-comment-p))))
    (acm-candidates-normalize
     (funcall (acm-backend-fetch-fn backend) keyword)
     (acm-backend-name backend))))

(defun acm-backend-candidates (name keyword)
  "Return the candidates the backend NAME offers for KEYWORD, or nil.
Return nil when no backend is registered under NAME.  The menu uses this for
the template backends, whose candidates it collects with its own ticker logic
instead of the group order."
  (when-let* ((backend (acm-backend-find name)))
    (acm-backend-fetch backend keyword)))

(defun acm-backends-group (queries group)
  "Return the candidates of GROUP in QUERIES, or nil.
QUERIES is the alist `acm-backends-fetch' returned; the group names are
strings, which `alist-get' would compare with `eq' and never match."
  (cdr (assoc group queries)))

(defun acm-backends-fetch (keyword)
  "Ask every registered non-template backend for candidates for KEYWORD.
Return an alist of group name to candidate list, in registration order.  A
backend that `short-circuit's the fetching stops it as soon as it returns
candidates; that is what keeps the language server from being asked about a
prefix that is really a file path."
  (let ((queries nil)
        (stop nil))
    (dolist (backend acm-backends)
      (unless (or stop (acm-backend-template backend))
        (let ((candidates (acm-backend-fetch backend keyword)))
          (when (and (acm-backend-group backend)
                     (> (length candidates) 0))
            (push (cons (acm-backend-group backend) candidates) queries))
          (when (and (acm-backend-short-circuit backend)
                     (> (length candidates) 0))
            (setq stop t)))))
    (nreverse queries)))

(defun acm-backends-dedupe-against-lsp (queries)
  "Drop the candidates the LSP backend already returned from QUERIES.
Only the backends whose registration has `dedupe-against-lsp' are filtered,
which is what keeps the workspace symbols and the words of the project from
being offered twice next to the language server's own candidates.  QUERIES is
an alist of group name to candidates; it is modified and returned."
  (let ((lsp-labels (mapcar (lambda (candidate)
                              (plist-get candidate :label))
                            (acm-backends-group queries "lsp-candidates"))))
    (dolist (backend acm-backends)
      (when (acm-backend-dedupe-against-lsp backend)
        (let* ((group (acm-backend-group backend))
               (candidates (acm-backends-group queries group)))
          (when candidates
            (setcdr (assoc group queries)
                    (acm-remove-duplicate-candidates candidates lsp-labels))))))
    queries))

(defun acm-backends-group-candidates (queries order)
  "Concatenate the candidate groups of QUERIES in the given ORDER.
ORDER is a list of group names, such as
`acm-completion-mode-candidates-merge-order'; the groups it does not name -
and the ones no backend contributed to - are simply not part of the result."
  (apply #'append
         (mapcar (lambda (group) (acm-backends-group queries group)) order)))

(defun acm-backend-expand (candidate &optional bound-start preview)
  "Insert CANDIDATE at point, replacing the text from BOUND-START on.
Use the expand function of the backend that produced CANDIDATE and return
what it returns, which is an overlay for the backends that support previews.
Return nil when the backend has none, so the caller falls back to inserting
the label.  PREVIEW may only be non-nil for a backend whose `preview' field
is set, since the others do not accept the argument."
  (let ((backend (acm-backend-find (plist-get candidate :backend))))
    (when (and backend (acm-backend-expand-fn backend))
      (funcall (acm-backend-expand-fn backend) candidate bound-start preview))))

(defun acm-backend-expand-p (candidate)
  "Return non-nil when the backend of CANDIDATE can expand it."
  (let ((backend (acm-backend-find (plist-get candidate :backend))))
    (and backend (acm-backend-expand-fn backend) t)))

(defun acm-backend-preview-p (candidate)
  "Return non-nil when the backend of CANDIDATE can preview it."
  (let ((backend (acm-backend-find (plist-get candidate :backend))))
    (and backend (acm-backend-preview backend) t)))

(defun acm-backend-clean (name)
  "Drop the cached state of the backend NAME.
Does nothing when the backend has no clean function."
  (when-let* ((backend (acm-backend-find name))
              (clean (acm-backend-clean-fn backend)))
    (funcall clean)))

(defun acm-backend-doc (candidate)
  "Return the documentation the backend of CANDIDATE has for it, or nil."
  (let ((backend (acm-backend-find (plist-get candidate :backend))))
    (when (and backend (acm-backend-doc-fn backend))
      (funcall (acm-backend-doc-fn backend) candidate))))

(defun acm-backend-doc-p (candidate)
  "Return non-nil when the backend of CANDIDATE can document it."
  (let ((backend (acm-backend-find (plist-get candidate :backend))))
    (and backend (acm-backend-doc-fn backend) t)))

;;; The backends of this tree
;;
;; They are listed here - and only here - so that the menu code never has to
;; know about them again.  The order is the order in which they are asked: the
;; path backend comes before the language server because it short-circuits.

(defun acm-register-built-in-backends ()
  "Register the backends that ship with this tree.
Called at load time; `acm-backend-register' replaces an entry with the same
name, so a user can re-register one of them with different fields."
  (dolist (spec
           '((:name "vulpea" :group "vulpea-candidates"
                    :fetch-fn acm-backend-vulpea-candidates
                    :expand-fn acm-backend-vulpea-candidate-expand
                    :enabled (lambda () (acm-in-vulpea-bracket-p)))
             (:name "copilot" :group "copilot-candidates"
                    :fetch-fn acm-backend-copilot-candidates
                    :clean-fn acm-backend-copilot-clean
                    :expand-fn acm-backend-copilot-candidate-expand
                    :doc-fn acm-backend-copilot-candidate-doc
                    :preview t
                    :enabled (lambda () acm-enable-copilot))
             (:name "jupyter" :group "jupyter-candidates"
                    :fetch-fn acm-backend-jupyter-candidates
                    :clean-fn acm-backend-jupyter-clean
                    :enabled (lambda () acm-enable-jupyter))
             (:name "tabby" :group "tabby-candidates"
                    :fetch-fn acm-backend-tabby-candidates
                    :clean-fn acm-backend-tabby-clean
                    :enabled (lambda () acm-enable-tabby))
             (:name "capf" :group "capf-candidates"
                    :fetch-fn acm-backend-capf-candidates
                    :expand-fn acm-backend-capf-candidate-expand
                    :preview t
                    :enabled (lambda () acm-enable-capf))
             (:name "devonthink" :group "devonthink-candidates"
                    :fetch-fn acm-backend-devonthink-candidates
                    :expand-fn acm-backend-devonthink-candidate-expand
                    :preview t
                    :enabled (lambda () acm-enable-devonthink))
             (:name "path" :group "path-candidates"
                    :fetch-fn acm-backend-path-candidates
                    :clean-fn acm-backend-path-clean
                    :expand-fn acm-backend-path-candidate-expand
                    :preview t
                    :short-circuit t)
             (:name "ctags" :group "ctags-candidates"
                    :fetch-fn acm-backend-ctags-candidates
                    :clean-fn acm-backend-ctags-clean
                    :expand-fn acm-backend-ctags-candidate-expand
                    :preview t
                    :enabled (lambda () acm-enable-ctags)
                    :skip-in-comment t)
             (:name "lsp" :group "lsp-candidates"
                    :fetch-fn acm-backend-lsp-candidates
                    :clean-fn acm-backend-lsp-clean
                    :expand-fn acm-backend-lsp-candidate-expand
                    :doc-fn acm-backend-lsp-candidate-doc
                    :preview t
                    :skip-in-comment t)
             (:name "lsp-workspace-symbol" :group "lsp-workspace-symbol-candidates"
                    :fetch-fn acm-backend-lsp-workspace-symbol-candidates
                    :clean-fn acm-backend-lsp-workspace-symbol-clean
                    :enabled (lambda () acm-enable-lsp-workspace-symbol)
                    :skip-in-comment t
                    :dedupe-against-lsp t)
             (:name "search-file-words" :group "file-words-candidates"
                    :fetch-fn acm-backend-search-file-words-candidates
                    :clean-fn acm-backend-search-file-words-clean
                    :expand-fn acm-backend-search-file-words-candidate-expand
                    :preview t
                    :dedupe-against-lsp t)
             (:name "elisp" :group "elisp-candidates"
                    :fetch-fn acm-backend-elisp-candidates
                    :clean-fn acm-backend-elisp-clean
                    :doc-fn acm-backend-elisp-candidate-doc
                    :skip-in-comment t)
             (:name "telega" :group "telega-candidates"
                    :fetch-fn acm-backend-telega-candidates
                    :expand-fn acm-backend-telega-candidate-expand
                    :preview t)
             (:name "yas" :fetch-fn acm-backend-yas-candidates
                    :expand-fn acm-backend-yas-candidate-expand
                    :doc-fn acm-backend-yas-candidate-doc
                    :template t)
             (:name "tempel" :fetch-fn acm-backend-tempel-candidates
                    :expand-fn acm-backend-tempel-candidate-expand
                    :doc-fn acm-backend-tempel-candidate-doc
                    :template t)))
    (acm-backend-register (apply #'make-acm-backend spec))))

(acm-register-built-in-backends)

(provide 'acm-backend)

;;; acm-backend.el ends here
