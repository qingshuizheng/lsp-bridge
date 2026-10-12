;;; acm-backend-elisp-test.el --- The Emacs Lisp search backend  -*- lexical-binding: t; -*-

;;; Commentary:
;; The Elisp backend offers the symbols of the running Emacs as completion
;; candidates.  What is checked here is the part of it that needs no menu: the
;; kind reported for a symbol, and the search that asks the Python side for
;; matches - including when it declines to ask, which is the part that keeps an
;; empty prefix or a comment from filling the bridge with requests.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'acm-backend-elisp)

;; Built where the candidates are cached, which the compiler cannot see through.
(defvar acm-backend-elisp-cache-candiates)

(ert-deftest acm-backend-elisp-test-symbol-type ()
  "Each kind of definition a symbol can be is named for what it is."
  (should (featurep 'acm-backend-elisp))
  (should (equal (acm-backend-elisp-symbol-type 'acm-backend-elisp) "feature"))
  (should (equal (acm-backend-elisp-symbol-type 'if) "special form"))
  (should (equal (acm-backend-elisp-symbol-type 'car) "function"))
  (should (equal (acm-backend-elisp-symbol-type 'when) "macro"))
  (should (equal (acm-backend-elisp-symbol-type 'bold) "face"))
  (should (equal (acm-backend-elisp-symbol-type 'acm-enable-doc) "custom"))
  (should (equal (acm-backend-elisp-symbol-type 'acm-icon-width) "variable")))

(ert-deftest acm-backend-elisp-test-search-asks-the-bridge ()
  "A prefix in an Elisp buffer outside a comment is searched for."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (calls)
      (cl-letf (((symbol-function 'acm-is-elisp-mode-p) (lambda () t))
                ((symbol-function 'acm-in-comment-p) (lambda () nil))
                ((symbol-function 'lsp-bridge-call-async)
                 (lambda (&rest args) (push args calls))))
        (lsp-bridge-elisp-symbols-search "car")
        (should (equal calls '(("search_list_search" "elisp" "car"))))
        (setq calls nil)
        (lsp-bridge-elisp-symbols-search "")
        (should-not calls)
        (setq calls nil)
        (lsp-bridge-elisp-symbols-search nil)
        (should-not calls)))))

(ert-deftest acm-backend-elisp-test-search-skips-comments ()
  "A prefix inside a comment is not searched for."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let (calls)
      (cl-letf (((symbol-function 'acm-is-elisp-mode-p) (lambda () t))
                ((symbol-function 'acm-in-comment-p) (lambda () t))
                ((symbol-function 'lsp-bridge-call-async)
                 (lambda (&rest args) (push args calls))))
        (lsp-bridge-elisp-symbols-search "car")
        (should-not calls)))))

(ert-deftest acm-backend-elisp-test-search-skips-other-modes ()
  "A buffer that is not Elisp is not searched for."
  (with-temp-buffer
    (let (calls)
      (cl-letf (((symbol-function 'acm-is-elisp-mode-p) (lambda () nil))
                ((symbol-function 'acm-in-comment-p) (lambda () nil))
                ((symbol-function 'lsp-bridge-call-async)
                 (lambda (&rest args) (push args calls))))
        (lsp-bridge-elisp-symbols-search "car")
        (should-not calls)))))

(ert-deftest acm-backend-elisp-test-local-symbols-of-a-let ()
  "The names bound around point are found, in an elisp buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f ()\n  (let ((alpha 1)\n        (beta 2))\n    |))\n")
    (goto-char (point-min))
    (search-forward "|")
    (should (equal (sort (acm-backend-elisp-local-symbols) #'string<)
                   '("alpha" "beta")))))

(ert-deftest acm-backend-elisp-test-local-symbols-outside-a-binding ()
  "Point outside any binding form has no local symbols."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f ()\n  (message \"here\"))\n")
    (goto-char (point-max))
    (should-not (acm-backend-elisp-local-symbols))))

(ert-deftest acm-backend-elisp-test-local-symbols-elsewhere ()
  "A buffer that is not Emacs Lisp has none either."
  (with-temp-buffer
    (insert "(let ((alpha 1)))\n")
    (goto-char (point-max))
    (should-not (acm-backend-elisp-local-symbols))))

(ert-deftest acm-backend-elisp-test-get-symbols-puts-the-local-ones-first ()
  "The local symbols come before the global ones."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(let ((a-local-name 1))\n  |)\n")
    (goto-char (point-min))
    (search-forward "|")
    (let ((symbols (acm-backend-elisp-get-symbols)))
      (should (equal (car symbols) "a-local-name"))
      (should (> (length symbols) 1)))))

(ert-deftest acm-backend-elisp-test-candidates-and-clean ()
  "Every symbol of the buffer becomes a candidate, and cleaning drops them."
  (with-temp-buffer
    (setq-local acm-backend-elisp-items '("car" "bold"))
    (let ((candidates (acm-backend-elisp-candidates "c")))
      (should (equal (mapcar (lambda (candidate) (plist-get candidate :label)) candidates)
                     '("car" "bold")))
      (should (equal (plist-get (car candidates) :icon) "function"))
      (should (equal (plist-get (nth 1 candidates) :icon) "face"))
      (should (equal (plist-get (car candidates) :annotation) "Function"))
      (should (equal (plist-get (car candidates) :backend) "elisp")))
    (acm-backend-elisp-clean)
    (should-not acm-backend-elisp-items)
    (should-not acm-backend-elisp-cache-candiates)))

(provide 'acm-backend-elisp-test)
;;; acm-backend-elisp-test.el ends here
