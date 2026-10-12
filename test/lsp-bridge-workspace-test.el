;;; lsp-bridge-workspace-test.el --- Paths, symbols and document highlights  -*- lexical-binding: t; -*-

;;; Commentary:
;; Three things in the workspace module are about text rather than about a server:
;; the file path of an LSP answer, which arrives as a URI and has to be turned back
;; into a path this machine understands; the line of a workspace symbol, which is
;; the kind, the name and the path relative to the project; and the document
;; highlight overlays, which are drawn over the ranges of an answer.
;;
;; The highlight tests also pin the order the module promises: the hook is run
;; before the overlays are drawn, so that an answer can be used before it is shown.
;; What is not here is everything that needs the server to answer first - the
;; requests themselves, the edits that come back, and the formatting.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-workspace)

(ert-deftest lsp-bridge-workspace-test-pick-file-path ()
  "A URI answer becomes the path it names."
  (should (equal (lsp-bridge-pick-file-path "file:///tmp/a.py") "/tmp/a.py"))
  (should (equal (lsp-bridge-pick-file-path ":file:///tmp/a.py") "/tmp/a.py"))
  (should (equal (lsp-bridge-pick-file-path "/tmp/a.py") "/tmp/a.py")))

(ert-deftest lsp-bridge-workspace-test-pick-file-path-decodes ()
  "Percent escapes are decoded, and the slash before a drive letter dropped."
  (should (equal (lsp-bridge-pick-file-path "file:///tmp/a%20b.py") "/tmp/a b.py"))
  (should (equal (lsp-bridge-pick-file-path "file:///c%3A/tmp/a.py") "c:/tmp/a.py"))
  (should (equal (lsp-bridge-pick-file-path "file:///C:/Users/a.py") "C:/Users/a.py")))

(ert-deftest lsp-bridge-workspace-test-transform-info ()
  "A workspace symbol becomes a line naming its kind, its name and its path."
  (let* ((root (file-name-as-directory (make-temp-file "lsp-bridge-workspace-test" t)))
         (source (expand-file-name "src" root))
         (default-directory root))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (make-directory source)
          (let ((line (lsp-bridge-workspace-transform-info
                       (list :name "my-function"
                             :kind 12
                             :location (list :uri (concat "file://" source "/a.el"))))))
            (should (string-prefix-p "[" line))
            (should (string-match-p (regexp-quote "] my-function") line))
            (should (string-suffix-p "src/a.el" line))
            ;; The kind and the path are propertized, the name itself is not.
            (should (get-text-property 0 'face line))
            (should-not (get-text-property (string-match-p "my-function" line) 'face line))))
      (delete-directory root t))))

(ert-deftest lsp-bridge-workspace-test-highlight-position-to-point ()
  "A line and character of an answer become a position in the buffer."
  (with-temp-buffer
    (insert "first\nsecond line\n")
    (should (equal (lsp-bridge-document-highlight--position-to-point 0 0) 1))
    (should (equal (lsp-bridge-document-highlight--position-to-point 1 7) 14))
    ;; Point is left where it was.
    (goto-char 3)
    (lsp-bridge-document-highlight--position-to-point 1 0)
    (should (equal (point) 3))))

(ert-deftest lsp-bridge-workspace-test-document-highlight-render ()
  "Every range of an answer becomes an overlay carrying the highlight face."
  (with-temp-buffer
    (insert "one two one\n")
    (setq-local lsp-bridge-document-highlight-timer nil)
    (lsp-bridge-document-highlight-render
     (list (list :range (list :start (list :line 0 :character 0)
                              :end (list :line 0 :character 3)))
           (list :range (list :start (list :line 0 :character 8)
                              :end (list :line 0 :character 11)))))
    (let ((overlays (cl-remove-if-not (lambda (overlay)
                                        (eq (overlay-get overlay 'face)
                                            'lsp-bridge-document-highlight-face))
                                      (overlays-in (point-min) (point-max)))))
      (should (equal (length overlays) 2))
      (should (equal (mapcar (lambda (overlay)
                               (buffer-substring-no-properties
                                (overlay-start overlay) (overlay-end overlay)))
                             (sort overlays (lambda (a b) (< (overlay-start a) (overlay-start b)))))
                     '("one" "one")))
      (should (eq (overlay-get (car overlays) 'window) (selected-window))))))

(ert-deftest lsp-bridge-workspace-test-document-highlight-hook-runs-first ()
  "The hook of the highlights is run before the overlays are drawn."
  (with-temp-buffer
    (insert "one two\n")
    (let ((seen nil)
          (overlay-count-when-run nil))
      (add-hook 'lsp-bridge-document-highlight-hook
                (lambda (highlights)
                  (setq seen highlights)
                  (setq overlay-count-when-run
                        (length (cl-remove-if-not
                                 (lambda (overlay)
                                   (eq (overlay-get overlay 'face)
                                       'lsp-bridge-document-highlight-face))
                                 (overlays-in (point-min) (point-max))))))
                nil t)
      (let ((highlights (list (list :range (list :start (list :line 0 :character 0)
                                                 :end (list :line 0 :character 3))))))
        (lsp-bridge-document-highlight-render highlights)
        (should (equal seen highlights))
        (should (equal overlay-count-when-run 0))))))

(defun lsp-bridge-workspace-test--highlights ()
  "Return the document highlight overlays of the current buffer."
  (cl-remove-if-not (lambda (overlay)
                      (eq (overlay-get overlay 'face)
                          'lsp-bridge-document-highlight-face))
                    (overlays-in (point-min) (point-max))))

(ert-deftest lsp-bridge-workspace-test-document-highlight-render-replaces ()
  "A new answer replaces the overlays of the old one, and cleanup removes them."
  (with-temp-buffer
    (insert "one two one\n")
    (let ((highlight (list :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 3)))))
      (lsp-bridge-document-highlight-render (list highlight))
      (lsp-bridge-document-highlight-render (list highlight))
      (should (equal (length (lsp-bridge-workspace-test--highlights)) 1))
      (lsp-bridge-document-highlight-cleanup)
      (should-not (lsp-bridge-workspace-test--highlights)))))

 ()
  "A new answer replaces the overlays of the old one."
  (with-temp-buffer
    (insert "one two one\n")
    (let ((highlight (list :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 3)))))
      (lsp-bridge-document-highlight-render (list highlight))
      (lsp-bridge-document-highlight-render (list highlight))
      (should (equal (length (cl-remove-if-not
                              (lambda (overlay)
                                (eq (overlay-get overlay 'face)
                                    'lsp-bridge-document-highlight-face))
                              (overlays-in (point-min) (point-max))))
                     1))
      (lsp-bridge-document-highlight-cleanup)
      (should-not (cl-remove-if-not (lambda (overlay)
                                      (eq (overlay-get overlay 'face)
                                          'lsp-bridge-document-highlight-face))
                                    (overlays-in (point-min) (point-max))))))

(provide 'lsp-bridge-workspace-test)
;;; lsp-bridge-workspace-test.el ends here
