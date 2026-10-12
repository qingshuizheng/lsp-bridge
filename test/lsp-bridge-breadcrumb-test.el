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

(defmacro lsp-bridge-breadcrumb-test--with-buffer (&rest body)
  "Run BODY in a buffer with the breadcrumb state of a fresh buffer."
  `(with-temp-buffer
     (insert "one two\n")
     (let ((lsp-bridge-breadcrumb--timer nil)
           (lsp-bridge-breadcrumb--last-point nil)
           (lsp-bridge-breadcrumb--last-update-tick nil)
           (lsp-bridge-breadcrumb--in-process nil))
       (unwind-protect (progn ,@body)
         (when lsp-bridge-breadcrumb--timer
           (cancel-timer lsp-bridge-breadcrumb--timer))))))

(ert-deftest lsp-bridge-breadcrumb-test-the-first-look-asks-for-the-symbols ()
  "The first look at a buffer schedules the call that fetches the symbols."
  (lsp-bridge-breadcrumb-test--with-buffer
    (should-not lsp-bridge-breadcrumb--timer)
    (lsp-bridge-breadcrumb--header-line)
    (should (timerp lsp-bridge-breadcrumb--timer))
    (should (equal lsp-bridge-breadcrumb--last-point (point)))
    (should (equal lsp-bridge-breadcrumb--last-update-tick (buffer-chars-modified-tick)))))

(ert-deftest lsp-bridge-breadcrumb-test-nothing-changed-asks-for-nothing ()
  "Looking again without a change or a move schedules nothing new."
  (lsp-bridge-breadcrumb-test--with-buffer
    (lsp-bridge-breadcrumb--header-line)
    (let ((first lsp-bridge-breadcrumb--timer))
      (lsp-bridge-breadcrumb--header-line)
      (should (eq lsp-bridge-breadcrumb--timer first)))))

(ert-deftest lsp-bridge-breadcrumb-test-a-change-asks-again ()
  "A change in the buffer schedules the call again, replacing the old timer."
  (lsp-bridge-breadcrumb-test--with-buffer
    (lsp-bridge-breadcrumb--header-line)
    (let ((first lsp-bridge-breadcrumb--timer))
      (goto-char (point-min))
      (insert "x")
      (lsp-bridge-breadcrumb--header-line)
      (should (timerp lsp-bridge-breadcrumb--timer))
      (should-not (eq lsp-bridge-breadcrumb--timer first)))))

(ert-deftest lsp-bridge-breadcrumb-test-a-call-in-flight-asks-for-nothing ()
  "While the symbols are being fetched, looking at the buffer asks for nothing."
  (lsp-bridge-breadcrumb-test--with-buffer
    (setq lsp-bridge-breadcrumb--in-process t)
    (lsp-bridge-breadcrumb--header-line)
    (should-not lsp-bridge-breadcrumb--timer)))

(ert-deftest lsp-bridge-breadcrumb-test-going-to-the-last-symbol ()
  "The command goes to the position the last symbol of the answer holds."
  (lsp-bridge-breadcrumb-test--with-buffer
    (let ((lsp-bridge-breadcrumb--last-response
           (list (list :name "one" :pos '(:line 0 :character 0))
                 (list :name "two" :pos '(:line 0 :character 4)))))
      (goto-char (point-min))
      (lsp-bridge-breadcrumb-goto-last-definition)
      (should (equal (point) 5)))))

(provide 'lsp-bridge-breadcrumb-test)
;;; lsp-bridge-breadcrumb-test.el ends here
