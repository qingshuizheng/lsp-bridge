;;; lsp-bridge-semantic-tokens-test.el --- Semantic tokens on the buffer  -*- lexical-binding: t; -*-

;;; Commentary:
;; A server sends semantic tokens as a flat list of deltas - how many lines down,
;; how many columns along, how long, which type, which modifiers - and the module
;; turns each of them into an overlay with the face of its type.  The decoding is
;; the part that cannot be checked with a server any better than without one, so
;; that is what is checked here: the positions the deltas describe, the face a
;; type maps to, a modifier taking precedence over it, the overlays being keyed so
;; that the same token can be deleted again, and the combination of face
;; attributes used when the type has no foreground of its own.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-semantic-tokens)

(defconst lsp-bridge-semantic-tokens-test--type-face 'lsp-bridge-semantic-tokens-test-type)
(defconst lsp-bridge-semantic-tokens-test--function-face 'lsp-bridge-semantic-tokens-test-function)
(defconst lsp-bridge-semantic-tokens-test--modifier-face 'lsp-bridge-semantic-tokens-test-modifier)

(defun lsp-bridge-semantic-tokens-test--prepare ()
  "Set up the token faces and the overlay table, and return the overlay table."
  (dolist (face (list lsp-bridge-semantic-tokens-test--type-face
                      lsp-bridge-semantic-tokens-test--function-face
                      lsp-bridge-semantic-tokens-test--modifier-face))
    (make-empty-face face)
    (set-face-foreground face "red"))
  (setq-local lsp-bridge-semantic-tokens-type-faces
              (vector (cons "type" lsp-bridge-semantic-tokens-test--type-face)
                      (cons "function" lsp-bridge-semantic-tokens-test--function-face)))
  (setq-local lsp-bridge-semantic-tokens-type-modifier-faces
              (vector (cons "declaration" lsp-bridge-semantic-tokens-test--modifier-face)))
  (setq-local lsp-bridge-semantic-tokens--overlays (make-hash-table :test 'equal))
  lsp-bridge-semantic-tokens--overlays)

(ert-deftest lsp-bridge-semantic-tokens-test-render-decodes-the-deltas ()
  "The deltas of a token list become overlays over the text they name."
  (with-temp-buffer
    (insert "alpha beta\ngamma delta\n")
    (let ((table (lsp-bridge-semantic-tokens-test--prepare)))
      (lsp-bridge-semantic-tokens--render
       (buffer-name) nil
       ;; line delta, column delta, length, type, modifiers
       '((0 0 5 0 nil)      ; alpha, of type 0
         (0 6 4 1 nil)      ; beta, of type 1
         (1 0 5 0 (0))))    ; gamma, on the next line, with modifier 0
      (should (equal (hash-table-count table) 3))
      (let ((alpha (gethash '(0 0 5 0 nil) table))
            (beta (gethash '(0 6 4 1 nil) table))
            (gamma (gethash '(1 0 5 0 (0)) table)))
        (should (equal (buffer-substring-no-properties
                        (overlay-start alpha) (overlay-end alpha))
                       "alpha"))
        (should (equal (buffer-substring-no-properties
                        (overlay-start beta) (overlay-end beta))
                       "beta"))
        (should (equal (buffer-substring-no-properties
                        (overlay-start gamma) (overlay-end gamma))
                       "gamma"))
        (should (eq (overlay-get alpha 'face) lsp-bridge-semantic-tokens-test--type-face))
        (should (eq (overlay-get beta 'face) lsp-bridge-semantic-tokens-test--function-face))
        (should (eq (overlay-get gamma 'face)
                    lsp-bridge-semantic-tokens-test--modifier-face))))))

(ert-deftest lsp-bridge-semantic-tokens-test-delete-overlays-takes-the-keys ()
  "Deleting a token deletes the overlay it was rendered into, and no other."
  (with-temp-buffer
    (insert "alpha beta gamma\n")
    (let ((table (lsp-bridge-semantic-tokens-test--prepare)))
      (lsp-bridge-semantic-tokens--render
       (buffer-name) nil '((0 0 5 0 nil) (0 6 4 1 nil)))
      (let ((alpha (gethash '(0 0 5 0 nil) table))
            (beta (gethash '(0 6 4 1 nil) table)))
        (lsp-bridge-semantic-tokens--delete-overlays '((0 0 5 0 nil)))
        (should-not (overlay-start alpha))
        (should (overlay-start beta))
        (should (equal (hash-table-count table) 1))
        (should-not (gethash '(0 0 5 0 nil) table))))))

(ert-deftest lsp-bridge-semantic-tokens-test-combine-faces ()
  "The attributes of several faces become one, and none of them is left out."
  (should (equal (lsp-bridge-semantic-tokens--combine-faces nil) '((t))))
  (should (equal (lsp-bridge-semantic-tokens--combine-faces '(bold))
                 '((t :weight bold)))))

(provide 'lsp-bridge-semantic-tokens-test)
;;; lsp-bridge-semantic-tokens-test.el ends here
