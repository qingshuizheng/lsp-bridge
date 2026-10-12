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

(provide 'acm-backend-yas-test)
;;; acm-backend-yas-test.el ends here
