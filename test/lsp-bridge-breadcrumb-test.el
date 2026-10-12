;;; lsp-bridge-breadcrumb-test.el --- The breadcrumb icons  -*- lexical-binding: t; -*-

;;; Commentary:
;; The breadcrumb shows an icon for the kind of the symbol at the top of the path
;; and builds it once per kind and state: those icons are SVG images, which are
;; not cheap to make.  What is checked here is that a kind already built is taken
;; from the cache instead of being made again, and that asking for an icon when
;; they are turned off costs nothing at all.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-breadcrumb)

(ert-deftest lsp-bridge-breadcrumb-test-icon-is-cached ()
  "A kind and state are built once, then taken from the cache."
  (let ((lsp-bridge-breadcrumb-show-icon t)
        (lsp-bridge-breadcrumb--icon-cache (make-hash-table :test 'equal))
        (acm-icon-width 2)
        (builds 0))
    (cl-letf (((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'acm-icon)
               (lambda (&rest _) (setq builds (1+ builds)) 'the-image)))
      (let ((first (lsp-bridge-breadcrumb--icon "function" t))
            (second (lsp-bridge-breadcrumb--icon "function" t)))
        (should (equal first second))
        (should (equal builds 1))
        (should (get-text-property 0 'display first))
        (lsp-bridge-breadcrumb--icon "function" nil)
        (should (equal builds 2))))))

(ert-deftest lsp-bridge-breadcrumb-test-icon-costs-nothing-when-off ()
  "With icons turned off there is a space and nothing else."
  (let ((lsp-bridge-breadcrumb-show-icon nil)
        (lsp-bridge-breadcrumb--icon-cache (make-hash-table :test 'equal))
        built)
    (cl-letf (((symbol-function 'acm-icon) (lambda (&rest _) (setq built t))))
      (should (equal (lsp-bridge-breadcrumb--icon "function" t) " "))
      (should-not built))))

(provide 'lsp-bridge-breadcrumb-test)
;;; lsp-bridge-breadcrumb-test.el ends here
