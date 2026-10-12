;;; acm-backend-lsp-test.el --- Positions, edits and the LSP cache  -*- lexical-binding: t; -*-

;;; Commentary:
;; The LSP backend turns the positions a server speaks in into positions in a
;; buffer, and the edits it sends back have to be applied from the last one to the
;; first, otherwise the earlier ones shift the later ones.  Both are arithmetic on
;; a buffer, and are what is checked here, together with the reset that clears the
;; candidates of the buffer.  What is not here is the request itself, the fetching
;; of a completion item's documentation, and the expansion of a snippet into the
;; buffer, all of which need a server.

;;; Code:

(require 'ert)
(require 'acm)
(require 'acm-backend-lsp)

;; The cache and the items are made where they are recorded; the cache is written
;; through `acm-with-cache-candidates', which the compiler cannot see through.
(defvar acm-backend-lsp-cache-candidates)
(defvar acm-backend-lsp-completion-position)
(defvar acm-backend-lsp-completion-trigger-characters)

(ert-deftest acm-backend-lsp-test-the-positions-are-declared ()
  "The positions the module reads are declared rather than made on the fly.
They are set with `setq-local' while a completion starts, and read again when an
edit comes back, which in a buffer that never started one would have been a
void variable before they were declared."
  (should (boundp 'acm-backend-lsp-completion-position))
  (should (boundp 'acm-backend-lsp-completion-trigger-characters)))

(ert-deftest acm-backend-lsp-test-position-to-point ()
  "A line and character of the protocol become a position, or a marker."
  (with-temp-buffer
    (insert "first\nsecond line\n")
    (should (equal (acm-backend-lsp-position-to-point '(:line 0 :character 0)) 1))
    (should (equal (acm-backend-lsp-position-to-point '(:line 1 :character 7)) 14))
    (let ((marker (acm-backend-lsp-position-to-point '(:line 1 :character 0) t)))
      (should (markerp marker))
      (should (equal (marker-position marker) 7)))))

(ert-deftest acm-backend-lsp-test-make-sure-descending ()
  "Edits in increasing order are reversed, and anything else is left alone."
  (with-temp-buffer
    (insert "0123456789\n0123456789\n")
    (let ((first (list :range (list :start (list :line 0 :character 0))))
          (second (list :range (list :start (list :line 1 :character 0)))))
      (should (equal (acm-backend-lsp-make-sure-descending (list first second))
                     (list second first)))
      (should (equal (acm-backend-lsp-make-sure-descending (list second first))
                     (list second first)))
      (should (equal (acm-backend-lsp-make-sure-descending (list first))
                     (list first)))
      (should-not (acm-backend-lsp-make-sure-descending nil)))))

(ert-deftest acm-backend-lsp-test-clean-makes-an-empty-table ()
  "Cleaning leaves an empty hash for the Python side to fill in again."
  (with-temp-buffer
    (setq-local acm-backend-lsp-items '("stale"))
    (setq-local acm-backend-lsp-cache-candidates '("stale"))
    (acm-backend-lsp-clean)
    (should (hash-table-p acm-backend-lsp-items))
    (should (equal (hash-table-count acm-backend-lsp-items) 0))
    (should-not acm-backend-lsp-cache-candidates)))

(provide 'acm-backend-lsp-test)
;;; acm-backend-lsp-test.el ends here
