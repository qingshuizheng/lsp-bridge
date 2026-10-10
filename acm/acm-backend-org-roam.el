;;; acm-backend-org-roam.el -*- lexical-binding: t; no-byte-compile: t; -*-

(defgroup acm-backend-org-roam nil
  "Org roam backend for acm."
  :group 'acm)

(defcustom acm-enable-org-roam nil
  "Popup Org roam completions when this option is turn on."
  :type 'boolean
  :group 'acm-backend-org-roam)

(defcustom acm-backend-org-roam-candidates-number 10
  "Maximal number of Org roam candidate of menu."
  :type 'integer
  :group 'acm-backend-org-roam)

(defun acm-backend-org-roam-candidates (keyword)
  "Return Org roam title candidates matching KEYWORD.
Return nil unless `acm-enable-org-roam' is non-nil and org-roam is
loaded.  The titles are fuzzy-matched against KEYWORD, sorted by
prefix and truncated to
`acm-backend-org-roam-candidates-number' entries."
  (when (and acm-enable-org-roam
             (featurep 'org-roam))
    (let* ((titles (org-roam--get-titles))
           (match-titles (seq-filter (lambda (s) (acm-candidate-fuzzy-search keyword s)) titles)))
      (acm-candidate-sort-by-prefix
       keyword
       (mapcar
        (lambda (title)
          (list :key title
                :icon "note"
                :label title
                :displayLabel title
                :annotation "Org roam"
                :backend "org-roam"))
        (cl-subseq match-titles 0 (min (length match-titles) acm-backend-org-roam-candidates-number)))))))

(defun acm-backend-org-roam-candidate-expand (candidate-info bound-start)
  "Insert the Org roam link of CANDIDATE-INFO at point.
When point is inside `org-roam-bracket-completion-re', replace the
text from BOUND-START to point with the candidate label and move point
past the closing brackets.  The label is prefixed with \"roam:\"
unless point is outside a source block and the first match group is
non-blank.  Do nothing when the regexp does not match."
  (let (roam-p start end)
    (when (org-in-regexp org-roam-bracket-completion-re 1)
      (setq roam-p (not (or (org-in-src-block-p)
                            (string-blank-p (match-string 1))))
            start (match-beginning 2)
            end (match-end 2))
      (delete-region bound-start (point))
      (insert (if roam-p "" "roam:") (plist-get candidate-info :label))
      (forward-char 2))))

(provide 'acm-backend-org-roam)

;;;acm-backend-org-roam.el ends here
