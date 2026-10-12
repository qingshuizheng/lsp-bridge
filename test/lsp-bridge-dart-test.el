;;; lsp-bridge-dart-test.el --- The closing labels of Dart  -*- lexical-binding: t; -*-

;;; Commentary:
;; Dart sends the labels of the closing brackets of a block - `// }' at the end of
;; the line a block ends on - and the module turns each of them into an overlay
;; over that line.  Turning a list of labels into overlays, replacing the ones of
;; the previous answer and removing them all again is the whole of the module that
;; is not about the server, and is what is checked here.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-dart)

(defmacro lsp-bridge-dart-test--with-file (&rest body)
  "Run BODY in the buffer of a fresh temporary file with two lines of Dart."
  `(let ((file (make-temp-file "lsp-bridge-dart-test" nil ".dart")))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (insert "main() {\n}\n")
           (setq-local lsp-bridge-dart-closing-label-overlays nil)
           ,@body)
       (let ((buffer (get-file-buffer file)))
         (when buffer (kill-buffer buffer)))
       (delete-file file))))

(defun lsp-bridge-dart-test--label (line character label)
  "Return a closing label LABEL ending at CHARACTER of LINE."
  (list :label label
        :range (list :start (list :line line :character 0)
                     :end (list :line line :character character))))

(ert-deftest lsp-bridge-dart-test-render-makes-one-overlay-per-label ()
  "Each label becomes an overlay at the end of its line."
  (lsp-bridge-dart-test--with-file
   (lsp-bridge-dart-closing-labels--render
    (buffer-file-name) "" (list (lsp-bridge-dart-test--label 0 9 "{")))
   (should (equal (length lsp-bridge-dart-closing-label-overlays) 1))
   (let ((overlay (car lsp-bridge-dart-closing-label-overlays)))
     (should (equal (overlay-get overlay 'before-string)
                    (propertize " // {" 'face 'lsp-bridge-dart-closing-label-face)))
     (should (overlay-get overlay 'evaporate))
     (should (equal (overlay-start overlay)
                    (save-excursion (goto-char (point-min)) (end-of-line) (point)))))))

(ert-deftest lsp-bridge-dart-test-render-replaces-the-previous-labels ()
  "A second answer removes the overlays of the first."
  (lsp-bridge-dart-test--with-file
   (lsp-bridge-dart-closing-labels--render
    (buffer-file-name) "" (list (lsp-bridge-dart-test--label 0 9 "{")))
   (let ((first (car lsp-bridge-dart-closing-label-overlays)))
     (lsp-bridge-dart-closing-labels--render
      (buffer-file-name) "" (list (lsp-bridge-dart-test--label 1 1 "}")))
     (should (equal (length lsp-bridge-dart-closing-label-overlays) 1))
     (should-not (overlay-start first))
     (should (equal (overlay-get (car lsp-bridge-dart-closing-label-overlays) 'before-string)
                    (propertize " // }" 'face 'lsp-bridge-dart-closing-label-face))))))

(ert-deftest lsp-bridge-dart-test-an-empty-answer-clears-them ()
  "An answer with no labels leaves no overlay behind."
  (lsp-bridge-dart-test--with-file
   (lsp-bridge-dart-closing-labels--render
    (buffer-file-name) "" (list (lsp-bridge-dart-test--label 0 9 "{")))
   (lsp-bridge-dart-closing-labels--render (buffer-file-name) "" nil)
   (should-not lsp-bridge-dart-closing-label-overlays)))

(ert-deftest lsp-bridge-dart-test-hide-removes-and-forgets ()
  "Hiding deletes every overlay and forgets the list."
  (with-temp-buffer
    (insert "main() {\n}\n")
    (setq-local lsp-bridge-dart-closing-label-overlays nil)
    (let ((first (make-overlay 1 4))
          (second (make-overlay 5 8)))
      (setq lsp-bridge-dart-closing-label-overlays (list first second))
      (lsp-bridge-dart-hide-closing-label-overlays)
      (should-not (overlay-start first))
      (should-not (overlay-start second))
      (should-not lsp-bridge-dart-closing-label-overlays))))

(provide 'lsp-bridge-dart-test)
;;; lsp-bridge-dart-test.el ends here
