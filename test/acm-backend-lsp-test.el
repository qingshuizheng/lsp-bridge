;;; acm-backend-lsp-test.el --- Positions, edits and the LSP cache  -*- lexical-binding: t; -*-

;;; Commentary:
;; The LSP backend turns the positions a server speaks in into positions in a
;; buffer, and the edits it sends back have to be applied from the last one to the
;; first, otherwise the earlier ones shift the later ones.  Both are arithmetic on
;; a buffer, and are what is checked here, together with the reset that clears the
;; candidates of the buffer.  Applying the edits of an answer is asked here too,
;; and so is the documentation of a completion item, which is read from what the
;; buffer already holds and fetched when it does not.  What is not here is the
;; request itself and the expansion of a snippet, which pull in the completion
;; framework and a server.

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

(ert-deftest acm-backend-lsp-test-insert-new-text-replaces-and-keeps-point ()
  "The text of a range is replaced, and point is left where it was."
  (with-temp-buffer
    (insert "one two three\n")
    (goto-char 5)
    (acm-backend-lsp-insert-new-text '(:line 0 :character 4) '(:line 0 :character 7) "TWO")
    (should (equal (buffer-string) "one TWO three\n"))
    (should (equal (point) 5))))

(ert-deftest acm-backend-lsp-test-apply-text-edits ()
  "Edits are applied from the last position to the first."
  (with-temp-buffer
    (insert "alpha beta gamma\n")
    ;; Two edits in increasing order: the first one must not shift the second.
    (acm-backend-lsp-apply-text-edits
     (list (list :range (list :start (list :line 0 :character 0)
                              :end (list :line 0 :character 5))
                 :newText "ALPHA")
           (list :range (list :start (list :line 0 :character 11)
                              :end (list :line 0 :character 16))
                 :newText "GAMMA")))
    (should (equal (buffer-string) "ALPHA beta GAMMA\n"))))

(ert-deftest acm-backend-lsp-test-apply-text-edits-inserts ()
  "An edit whose range is empty inserts without deleting."
  (with-temp-buffer
    (insert "one three\n")
    (acm-backend-lsp-apply-text-edits
     (list (list :range (list :start (list :line 0 :character 4)
                              :end (list :line 0 :character 4))
                 :newText "two ")))
    (should (equal (buffer-string) "one two three\n"))))

(defun acm-backend-lsp-test--items (server key documentation)
  "Return an items table holding DOCUMENTATION for KEY under SERVER."
  (let ((by-key (make-hash-table :test 'equal)))
    (puthash key (list :documentation documentation) by-key)
    (let ((items (make-hash-table :test 'equal)))
      (puthash server by-key items)
      items)))

(ert-deftest acm-backend-lsp-test-candidate-doc-is-read-from-the-items ()
  "The documentation of a candidate is what the buffer holds for it."
  (with-temp-buffer
    (setq-local acm-backend-lsp-items (acm-backend-lsp-test--items "server" "candidate" "The doc."))
    (let (fetched)
      (let ((acm-backend-lsp-fetch-completion-item-func
             (lambda (candidate) (setq fetched candidate))))
        (should (equal (acm-backend-lsp-candidate-doc
                        (list :key "candidate" :server "server"))
                       "The doc."))
        ;; Nothing is asked of the server when the answer is already here.
        (should-not fetched)))))

(ert-deftest acm-backend-lsp-test-candidate-doc-is-fetched-when-missing ()
  "A missing documentation is asked for, and the ticker is cleared first."
  (with-temp-buffer
    (setq-local acm-backend-lsp-items (acm-backend-lsp-test--items "server" "candidate" nil))
    (setq-local acm-backend-lsp-fetch-completion-item-ticker 'something)
    (let (fetched)
      (let ((acm-backend-lsp-fetch-completion-item-func
             (lambda (candidate) (setq fetched candidate))))
        (should-not (acm-backend-lsp-candidate-doc (list :key "candidate" :server "server")))
        (should (equal fetched (list :key "candidate" :server "server")))
        (should-not acm-backend-lsp-fetch-completion-item-ticker)))))

(ert-deftest acm-backend-lsp-test-candidate-doc-of-an-empty-one-is-fetched ()
  "An empty documentation counts as a missing one."
  (with-temp-buffer
    (setq-local acm-backend-lsp-items (acm-backend-lsp-test--items "server" "candidate" ""))
    (let (fetched)
      (let ((acm-backend-lsp-fetch-completion-item-func
             (lambda (candidate) (setq fetched candidate))))
        (acm-backend-lsp-candidate-doc (list :key "candidate" :server "server"))
        (should fetched)))))

(ert-deftest acm-backend-lsp-test-snippet-expansion-needs-yas ()
  "The snippet expander is offered only while yasnippet is on.
The function is read as a special variable, so it is set rather than bound here."
  (let ((was-bound (boundp 'yas-minor-mode))
        (was-value (and (boundp 'yas-minor-mode) (symbol-value 'yas-minor-mode))))
    (unwind-protect
        (progn
          (setq yas-minor-mode nil)
          (should-not (acm-backend-lsp-snippet-expansion-fn))
          (setq yas-minor-mode t)
          (should (eq (acm-backend-lsp-snippet-expansion-fn) 'yas-expand-snippet)))
      (if was-bound
          (setq yas-minor-mode was-value)
        (makunbound 'yas-minor-mode)))))

(ert-deftest acm-backend-lsp-test-expand-inserts-the-insert-text ()
  "Expanding a plain candidate replaces the text from the menu onwards.
A completion replaces the rest of the word, and the rest of the line with it when
no `textEdit' says otherwise: what the server offers is the whole of what should
stand there."
  (let ((acm-backend-lsp-enable-auto-import nil))
    (with-temp-buffer
      (insert "pref-rest\n")
      (goto-char 10)
      (acm-backend-lsp-candidate-expand (list :label "l" :insertText "prefix") 1)
      (should (equal (buffer-string) "prefix\n")))))

(ert-deftest acm-backend-lsp-test-expand-falls-back-to-the-label ()
  "A candidate without an insert text inserts its label."
  (let ((acm-backend-lsp-enable-auto-import nil))
    (with-temp-buffer
      (insert "pre-rest\n")
      (goto-char 5)
      (acm-backend-lsp-candidate-expand (list :label "prefix" :insertText nil) 1)
      ;; Point was at 5, one past `pre-', so the label takes its place.
      (should (equal (buffer-string) "prefixrest\n")))))

(ert-deftest acm-backend-lsp-test-expand-in-preview-leaves-the-buffer ()
  "A preview shows the text in an overlay and changes nothing."
  (let ((acm-backend-lsp-enable-auto-import nil))
    (with-temp-buffer
      (insert "pre-rest\n")
      (goto-char 5)
      (let (shown)
        (cl-letf (((symbol-function 'acm-preview-create-overlay)
                   (lambda (bound-start bound-end text) (setq shown (list bound-start bound-end text)))))
          (acm-backend-lsp-candidate-expand (list :label "prefix" :insertText "prefix") 1 t))
        (should (equal (buffer-string) "pre-rest\n"))
        (should shown)))))

(provide 'acm-backend-lsp-test)
;;; acm-backend-lsp-test.el ends here
