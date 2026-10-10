;;; acm-backend-tempel.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defgroup acm-backend-tempel nil
  "Tempel backend for acm."
  :group 'acm)

(defcustom acm-enable-tempel nil
  "Popup tempel completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-tempel)

(defcustom acm-backend-tempel-candidates-number 2
  "Maximal number of tempel candidate of menu."
  :type 'integer
  :group 'acm-backend-tempel)

(defun acm-backend-tempel-candidates (keyword)
  "Return tempel snippet candidates matching KEYWORD.
Return nil unless `acm-enable-tempel' is non-nil and tempel is loaded.
When `tempel-trigger-prefix' is non-nil, KEYWORD must start with that
prefix and is matched without it."
  (when (and acm-enable-tempel
             (featurep 'tempel)
             (or (not tempel-trigger-prefix) (string-prefix-p tempel-trigger-prefix keyword)))
    (let* ((keyword (if tempel-trigger-prefix
                        (string-remove-prefix tempel-trigger-prefix keyword)
                      keyword))
           (snippets (cl-loop for template in (tempel--templates)
                              collect (format "%s" (car template))))
           (match-snippets (seq-filter (lambda (s) (acm-candidate-fuzzy-search keyword s)) snippets)))
      (acm-candidate-sort-by-prefix
       keyword
       (mapcar
        (lambda (snippet)
          (list :key snippet
                :icon "snippet"
                :label snippet
                :displayLabel snippet
                :annotation "Tempel"
                :backend "tempel"))
        (cl-subseq match-snippets 0 (min (length match-snippets) acm-backend-tempel-candidates-number)))))))

(defun acm-backend-tempel-candidate-expand (candidate-info bound-start)
  "Expand the tempel snippet held in CANDIDATE-INFO.
Delete the text between BOUND-START and point, then insert the
template named by the candidate label."
  (delete-region bound-start (point))
  (tempel-insert (intern-soft (plist-get candidate-info :label))))

(defun acm-backend-tempel-candidate-doc (candidate)
  "Return the printed template body of tempel CANDIDATE."
  (let ((snippet
         (alist-get (intern-soft (plist-get candidate :label))
                    (tempel--templates))))
    (mapconcat #'tempel--print-template snippet " ")))

(provide 'acm-backend-tempel)

;;;acm-backend-tempel.el ends here
