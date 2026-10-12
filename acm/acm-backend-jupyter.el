;;; acm-backend-jupyter.el --- acm jupyter support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(defgroup acm-backend-jupyter nil
  "ACM jupyter support."
  :group 'acm)

(defcustom acm-enable-jupyter nil
  "Enable jupyter support."
  :type 'boolean
  :group 'acm-backend-jupyter)

(defvar-local acm-backend-jupyter-items nil
  "List of jupyter completion candidates recorded for this buffer.")

(defun acm-backend-jupyter-candidates (keyword)
  "Return jupyter completion candidates matching KEYWORD.
Candidates are read from `acm-backend-jupyter-items', sorted by
prefix for KEYWORD and memoized in
`acm-backend-jupyter-cache-candiates'."
  (acm-with-cache-candidates
   acm-backend-jupyter-cache-candiates
   (acm-candidate-sort-by-prefix
    keyword
    acm-backend-jupyter-items)))

;;;###autoload
(defun acm-backend-jupyter-record (current-symbol)
  "Record the jupyter completions for the current buffer.
Load the jupyter libraries when `jupyter-org-completion-at-point' is
not yet defined, clear `acm-backend-jupyter-items', then collect the
completions reported by `jupyter-org-completion-at-point' and keep
only those starting with CURRENT-SYMBOL when it is a string.  The
candidates are stored through
`lsp-bridge-search-backend--record-items'."
  (interactive)
  (unless (fboundp 'jupyter-org-completion-at-point)
    (require 'jupyter)
    (require 'jupyter-org-client)
    (require 'ob-jupyter))
  (setq-local acm-backend-jupyter-items nil)
  (let* ((candidates (mapcar #'substring-no-properties (all-completions "" (nth 2 (jupyter-org-completion-at-point)))))
         (candidates (if (stringp current-symbol)
                         (seq-filter (apply-partially #'string-prefix-p current-symbol) candidates)
                       candidates))
         (candidates-with-metadata (mapcar (lambda (candidate)
                                             (list :key candidate
                                                   :icon "jupyter"
                                                   :label candidate
                                                   :displayLabel candidate
                                                   :annotation "jupyter"
                                                   :backend "jupyter"))
                                           candidates)))
    (when candidates
      (lsp-bridge-search-backend--record-items "jupyter" candidates-with-metadata))))

(defun acm-backend-jupyter-clean ()
  "Clear the jupyter candidates and the candidate cache in this buffer."
  (setq-local acm-backend-jupyter-items nil)
  (setq-local acm-backend-jupyter-cache-candiates nil))

(provide 'acm-backend-jupyter)
;;; acm-backend-jupyter.el ends here
