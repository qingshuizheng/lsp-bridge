;;; acm-backend-ekg.el --- ekg backend for acm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup acm-backend-ekg nil
  "Ekg backend for ACM."
  :group 'acm)

(defcustom acm-enable-ekg t
  "If non-nil enable ekg completion."
  :type 'boolean
  :group 'acm-backend-ekg)



;;; TAGS completion

(defvar acm-backend-ekg-field-complete-funcs
  '(("Tags" . acm-backend-ekg-get-tags))
  "Functions to complete values in various metadata fields.")

(defun acm-backend-ekg-get-tags (keyword)
  (let ((tags (mapcar (lambda (tag)
                        (list :key tag
                              :icon "tag"
                              :label (if (ekg--in-metadata-p) tag (format "[[ekg-tags-any:(\"%s\")][🔖%s]]" tag tag))
                              :displayLabel tag
                              :annotation "Tag"
                              :backend "ekg"))
                      (ekg-tags))))
    (if (string-equal keyword "")
        tags
      (acm-candidate-sort-by-prefix
       keyword
       (seq-filter
        (lambda (tag)
          (string-match keyword (plist-get tag :label)))
        tags)))))



;;; FIELD NAME completion

(defun acm-backend-ekg-field-names-cand (keyword)
  (let ((field-name (mapcar (lambda (field)
                              (list :key field
                                    :icon "at"
                                    :label field
                                    :displayLabel field
                                    :annotation "Field Name"
                                    :backend "ekg"))
                            (seq-difference
                             (mapcar #'car ekg-metadata-parsers)
                             (mapcar #'car (ekg--metadata-fields nil))))))
    (if (string-equal keyword "")
        field-name
      (acm-candidate-sort-by-prefix
       keyword
       (seq-filter
        (lambda (field-name)
          (string-match keyword (plist-get field-name :label)))
        field-name)))))



;;; TITLE completion

(defvar acm-backend-ekg-titles-cand nil)

(defun acm-backend-ekg-ensure-titles-cand ()
  (unless acm-backend-ekg-titles-cand
    (message "updating TITLES, please wait...")
    (setq acm-backend-ekg-titles-cand
          (mapcar (lambda (title-cons)
                    (let* ((id (format "%s" (car title-cons)))
                           (title (cdr title-cons))
                           (completion (format "[[ekg-note:%s][✏️%s]]" id title)))
                      (list :key id
                            :icon "at"
                            :label completion    ; invisible, see it after completion
                            :displayLabel title ; what you see in the left
                            :annotation "Title"  ; what you see in the right
                            :backend "ekg")))
                  (ekg-document-titles)))
    (message "updating TITLES done...")))

(defun acm-backend-ekg-get-titles-cand (keyword)
  (let ((titles acm-backend-ekg-titles-cand))
    (unless titles (acm-backend-ekg-ensure-titles-cand))
    (if (string-equal keyword "")
        titles
      (acm-candidate-sort-by-prefix
       keyword
       (seq-filter
        (lambda (field-name)
          (string-match keyword (plist-get field-name :label)))
        titles)))))


;;; update ekg titles after loaded, since getting titles are really slow.

(with-eval-after-load 'ekg
  (run-with-idle-timer 2 nil #'acm-backend-ekg-ensure-titles-cand))



;;; completion target dispatch

(defun acm-backend-ekg-candidates (keyword)
  "Completion function for all metadata at `completion-at-point-functions'.
If no completion function is found for the field type, don't
attempt the completion."
  (when (and acm-enable-ekg (or ekg-capture-mode ekg-edit-mode))
    ;; 1. In metadata, has field name
    (if-let (field (ekg--metadata-current-field))
        (when-let (completion-func (assoc (car field) acm-backend-ekg-field-complete-funcs #'equal))
          (funcall (cdr completion-func) keyword))
      ;; 2. In metadata, has no filed name
      ;; There's no current field, but we're in the metadata, so let's complete
      ;; the possible fields.
      (if (ekg--in-metadata-p)
          (acm-backend-ekg-field-names-cand keyword)
        ;; 3. In text area
        (if (save-excursion
              (backward-char (length keyword))
              (or
               (char-equal (char-before) ?>) ;
               ))
            (acm-backend-ekg-get-titles-cand keyword)
          (if (save-excursion
                (backward-char (length keyword))
                (or
                 (char-equal (char-before) ?#) ; tag
                 (char-equal (char-before) ?n) ; note
                 (char-equal (char-before) ?t) ; tag
                 ))
              ;; (acm-backend-ekg-get-titles-cand keyword)
              (acm-backend-ekg-get-tags keyword)
            ))))))



(provide 'acm-backend-ekg)
;;; acm-backend-ekg.el ends here
