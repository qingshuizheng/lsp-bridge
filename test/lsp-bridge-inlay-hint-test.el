;;; lsp-bridge-inlay-hint-test.el --- Drawing the hints of a buffer  -*- lexical-binding: t; -*-

;;; Commentary:
;; An inlay hint says something the source does not: the type of a variable, or
;; the name of a parameter that is only passed.  Turning a hint into the text that
;; is shown around it is pure - the padding on either side, and the label, which a
;; server may send as a string or as a list of pieces - and drawing it is a buffer
;; and overlays, with the type hints shown before the text they belong to and the
;; parameter hints after it.  Both are asked here.  What is not here is the request
;; itself, the scroll monitor that decides when to send it, and the retry after a
;; server refuses one.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-inlay-hint)

(defmacro lsp-bridge-inlay-hint-test--with-file (&rest body)
  "Run BODY in the buffer of a fresh temporary file with two lines of code."
  `(let ((file (make-temp-file "lsp-bridge-inlay-hint-test" nil ".py")))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (insert "value = call(first, second)\nother = 1\n")
           (setq-local lsp-bridge-inlay-hint-overlays nil)
           (setq-local lsp-bridge-inlay-hint-cache nil)
           (setq-local lsp-bridge-inlay-hint-cache-time nil)
           ,@body)
       (let ((buffer (get-file-buffer file)))
         (when buffer (kill-buffer buffer)))
       (delete-file file))))

(defun lsp-bridge-inlay-hint-test--hint (line character kind label &optional padding-left padding-right)
  "Return an inlay hint of KIND and LABEL at CHARACTER of LINE."
  (list :position (list :line line :character character)
        :kind kind
        :label label
        :paddingLeft padding-left
        :paddingRight padding-right))

(ert-deftest lsp-bridge-inlay-hint-test-padding-text ()
  "Padding is a space when it is wanted, and nothing when it is not."
  (with-temp-buffer
    (insert "ab")
    (goto-char 2)
    (should (equal (lsp-bridge-inlay-hint-padding-text t t) " "))
    (should (equal (lsp-bridge-inlay-hint-padding-text t nil) " "))
    ;; Nothing is added when the hint asked for no padding.
    (should-not (lsp-bridge-inlay-hint-padding-text nil t))
    (should-not (lsp-bridge-inlay-hint-padding-text nil nil))
    ;; `:json-false' is false, and so is a missing value.
    (should-not (lsp-bridge-inlay-hint-padding-text :json-false t))
    ;; A space or a tab next to the hint is padding enough already.
    (insert " ")
       (should-not (lsp-bridge-inlay-hint-padding-text t t))))

(ert-deftest lsp-bridge-inlay-hint-test-label-text ()
  "A label is a string, or pieces that are put next to each other."
  (should (equal (lsp-bridge-inlay-hint-label-text "the label") "the label"))
  (should (equal (lsp-bridge-inlay-hint-label-text
                  (list (list :value "first") (list :value ": ") (list :value "second")))
                 "first: second"))
  ;; Nought is a list as far as `listp' is concerned, so it takes the list branch
  ;; and comes out empty rather than printed.
  (should (equal (lsp-bridge-inlay-hint-label-text nil) "")))

(ert-deftest lsp-bridge-inlay-hint-test-render-draws-each-hint ()
  "A type hint is shown before its text and a parameter hint after it."
  (lsp-bridge-inlay-hint-test--with-file
   (lsp-bridge-inlay-hint--render
    (buffer-file-name) ""
    (list (lsp-bridge-inlay-hint-test--hint 0 0 1 ": int")
          (lsp-bridge-inlay-hint-test--hint 0 13 2 "first=" t nil)))
   (should (equal (length lsp-bridge-inlay-hint-overlays) 2))
   (let ((type-overlay (car (last lsp-bridge-inlay-hint-overlays)))
         (parameter-overlay (car lsp-bridge-inlay-hint-overlays)))
     (should (overlay-get type-overlay 'before-string))
     (should-not (overlay-get type-overlay 'after-string))
     (should (equal (get-text-property 0 'face (overlay-get type-overlay 'before-string))
                    'lsp-bridge-inlay-hint-face))
     ;; The cursor stops at the start of a type hint rather than jumping over it.
     (should (equal (get-text-property 0 'cursor (overlay-get type-overlay 'before-string)) 1))
     (should (overlay-get parameter-overlay 'after-string))
     (should-not (overlay-get parameter-overlay 'before-string))
     (should (equal (overlay-get parameter-overlay 'after-string) " first="))
     (should (overlay-get parameter-overlay 'evaporate)))))

(ert-deftest lsp-bridge-inlay-hint-test-render-remembers-what-it-drew ()
  "The hints and the time are cached, and the same ones are not drawn twice."
  (lsp-bridge-inlay-hint-test--with-file
   (let ((hints (list (lsp-bridge-inlay-hint-test--hint 0 0 1 ": int"))))
     (lsp-bridge-inlay-hint--render (buffer-file-name) "" hints)
     (should (equal lsp-bridge-inlay-hint-cache hints))
     (should lsp-bridge-inlay-hint-cache-time)
     (let ((first (car lsp-bridge-inlay-hint-overlays)))
       (lsp-bridge-inlay-hint--render (buffer-file-name) "" hints)
       ;; The same answer again leaves the overlays of the first one in place.
       (should (equal lsp-bridge-inlay-hint-overlays (list first)))))))

(ert-deftest lsp-bridge-inlay-hint-test-hide-removes-and-forgets ()
  "Hiding deletes every overlay and forgets the list."
  (with-temp-buffer
    (insert "value = 1\n")
    (setq-local lsp-bridge-inlay-hint-overlays nil)
    (let ((first (make-overlay 1 4))
          (second (make-overlay 5 8)))
      (setq lsp-bridge-inlay-hint-overlays (list first second))
      (lsp-bridge-inlay-hint-hide-overlays)
      (should-not (overlay-start first))
      (should-not (overlay-start second))
      (should-not lsp-bridge-inlay-hint-overlays))))

(provide 'lsp-bridge-inlay-hint-test)
;;; lsp-bridge-inlay-hint-test.el ends here
