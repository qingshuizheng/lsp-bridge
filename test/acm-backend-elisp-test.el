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

(provide 'acm-backend-elisp-test)
;;; acm-backend-elisp-test.el ends here
