;;; acm-backend-yas-test.el --- How the yasnippet backend labels templates  -*- lexical-binding: t; -*-

;;; Commentary:
;; The yasnippet backend turns the templates of the current buffer into
;; candidates.  The part of that which is about the templates rather than about
;; yasnippet is how they are labelled - the name and the trigger of a template, in
;; whichever order the options ask for - and how they are ordered, which puts the
;; ones that start with the keyword being completed first.  Both helpers are asked
;; here with templates built through `yas--make-template', which is yasnippet's own
;; way of making one, so no snippet file has to be loaded to see the ordering.

;;; Code:

(require 'ert)
(require 'acm)
(require 'acm-backend-yas)

(ert-deftest acm-backend-yas-test-template-cons-puts-the-name-first ()
  "By default the car of the cons is the name and the cdr the trigger."
  (require 'yasnippet nil t)
  (skip-unless (fboundp 'yas--make-template))
  (let ((template (yas--make-template :name "the-name" :key "the-key")))
    (should (equal (acm-backend-yas--template-cons template) '("the-name" . "the-key")))
    (let ((acm-backend-yas-show-trigger-keyword " (%s)")
          (acm-backend-yas-match-by-trigger-keyword t))
      (should (equal (acm-backend-yas--template-cons template) '("the-key" . "the-name"))))))

(ert-deftest acm-backend-yas-test-template-sort-prefix-first ()
  "Templates starting with the keyword come first, then the shorter labels."
  (require 'yasnippet nil t)
  (skip-unless (fboundp 'yas--make-template))
  (let ((templates (mapcar (lambda (name) (yas--make-template :name name :key name))
                           '("bar" "foo-bar" "fo"))))
    (should (equal (mapcar (lambda (template) (car (acm-backend-yas--template-cons template)))
                           (acm-backend-yas--template-sort "fo" templates))
                   '("fo" "foo-bar" "bar")))))

(ert-deftest acm-backend-yas-test-template-sort-without-a-keyword ()
  "Without a keyword the templates are handed back as they were."
  (should (equal (acm-backend-yas--template-sort nil '(b a)) '(b a)))
  (should-not (acm-backend-yas--template-sort nil nil)))

(ert-deftest acm-backend-yas-test-cache-expire-follows-the-option ()
  "Snippets being loaded mark the cache stale, while the option is on."
  (let ((acm-backend-yas--cache-expire-p nil))
    (let ((acm-enable-yas nil))
      (acm-backend-yas--cache-expire)
      (should-not acm-backend-yas--cache-expire-p))
    (let ((acm-enable-yas t))
      (acm-backend-yas--cache-expire)
      (should acm-backend-yas--cache-expire-p))))

(defun acm-backend-yas-test--with-yas-templates (templates body)
  "Call BODY with TEMPLATES as the snippet tables of yasnippet.
The accessors are read from the yasnippet library, so they are stood in for."
  (cl-letf (((symbol-function 'yas--modes-to-activate) (lambda () '(prog-mode)))
            ((symbol-function 'yas--get-snippet-tables) (lambda () 'tables))
            ((symbol-function 'yas--all-templates) (lambda (_tables) templates)))
    (funcall body)))

(ert-deftest acm-backend-yas-test-candidates-need-yas-and-a-keyword ()
  "Nothing is offered while yasnippet is off, or without a keyword."
  (let ((acm-enable-yas nil)
        (acm-backend-yas-candidate-min-length 0))
    (should-not (acm-backend-yas-candidates "fo")))
  (let ((acm-enable-yas t)
        (acm-backend-yas-candidate-min-length 0))
    (setq yas-minor-mode nil)
    (should-not (acm-backend-yas-candidates "fo"))
    (should-not (acm-backend-yas-candidates ""))))

(ert-deftest acm-backend-yas-test-candidates-of-a-template ()
  "A matching template becomes a candidate with its snippet body."
  (let ((acm-enable-yas t)
        (acm-backend-yas-candidate-min-length 0)
        (acm-backend-yas-candidates-number 10)
        (acm-backend-yas-show-trigger-keyword t)
        (acm-backend-yas--cache nil)
        (acm-backend-yas--cache-expire-p t))
    (setq yas-minor-mode t)
    (acm-backend-yas-test--with-yas-templates
     (list (yas--make-template :name "foobar" :key "fb" :content "body-of-foobar"))
     (lambda ()
       (let ((candidate (car (acm-backend-yas-candidates "foo"))))
         (should (equal (plist-get candidate :label) "foobar"))
         (should (equal (plist-get candidate :icon) "snippet"))
         (should (equal (plist-get candidate :annotation) "Yas-Snippet"))
         (should (equal (plist-get candidate :content) "body-of-foobar"))
         ;; The trigger is shown beside the name, and the name is what is matched.
         (should (equal (substring-no-properties (plist-get candidate :displayLabel))
                        "foobar (fb)")))))))

(ert-deftest acm-backend-yas-test-candidates-show-the-trigger-as-configured ()
  "A string option is used as the format itself."
  (let ((acm-enable-yas t)
        (acm-backend-yas-candidate-min-length 0)
        (acm-backend-yas-candidates-number 10)
        (acm-backend-yas-show-trigger-keyword " [%s]")
        (acm-backend-yas--cache nil)
        (acm-backend-yas--cache-expire-p t))
    (setq yas-minor-mode t)
    (acm-backend-yas-test--with-yas-templates
     (list (yas--make-template :name "foobar" :key "fb" :content "body"))
     (lambda ()
       (should (equal (substring-no-properties
                       (plist-get (car (acm-backend-yas-candidates "foo")) :displayLabel))
                      "foobar [fb]"))))))

(ert-deftest acm-backend-yas-test-candidate-expand-and-doc ()
  "Expanding a candidate deletes up to the bound and expands its body."
  (with-temp-buffer
    (insert "pre-rest\n")
    (goto-char 5)
    (let (expanded)
      (cl-letf (((symbol-function 'yas-expand-snippet) (lambda (content) (setq expanded content))))
        (acm-backend-yas-candidate-expand (list :content "body") 1))
      (should (equal (buffer-string) "rest\n"))
      (should (equal expanded "body"))))
  (should (equal (acm-backend-yas-candidate-doc (list :content "body")) "body"))
  (should-not (acm-backend-yas-candidate-doc (list :label "no content"))))

(provide 'acm-backend-yas-test)
;;; acm-backend-yas-test.el ends here
