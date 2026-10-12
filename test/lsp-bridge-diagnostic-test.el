;;; lsp-bridge-diagnostic-test.el --- Diagnostics over the buffer  -*- lexical-binding: t; -*-

;;; Commentary:
;; The diagnostics of a file are drawn as overlays over the text they belong to,
;; and read back through point.  That is the part of the module that needs no
;; server, because the diagnostics arrive as data.  What is checked here is that
;; the data becomes the overlays it should: one per diagnostic, with its face and
;; its message, a severity that is hidden left out, a diagnostic of no width given
;; one character to show, the tooltip numbering of several diagnostics - and that
;; the overlays are found at point and removed again.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-diagnostic)

(defmacro lsp-bridge-diagnostic-test--with-file (&rest body)
  "Run BODY in the buffer of a fresh temporary file holding one line of text."
  `(let ((file (make-temp-file "lsp-bridge-diagnostic-test" nil ".txt")))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (insert "one two three four five\n")
           ,@body)
       (let ((buffer (get-file-buffer file)))
         (when buffer (kill-buffer buffer)))
       (delete-file file))))

(defun lsp-bridge-diagnostic-test--diagnostic (start end severity message)
  "Return a diagnostic of SEVERITY and MESSAGE covering START..END on line 0."
  (list :severity severity :message message :source "test" :code "T1"
        :range (list :start (list :line 0 :character start)
                     :end (list :line 0 :character end))))

(ert-deftest lsp-bridge-diagnostic-test-render-makes-overlays ()
  "One overlay per shown diagnostic, with its face, message and numbering."
  (lsp-bridge-diagnostic-test--with-file
   (let ((lsp-bridge-diagnostic-enable-overlays t)
         (lsp-bridge-diagnostic-hide-severities '(4))
         updated)
     (setq-local lsp-bridge-diagnostic-overlays nil)
     (setq-local lsp-bridge-diagnostic-records nil)
     (setq-local lsp-bridge-diagnostic-count nil)
     (add-hook 'lsp-bridge-diagnostic-update-hook (lambda () (setq updated t)) nil t)
     (let ((diagnostics (list (lsp-bridge-diagnostic-test--diagnostic 0 3 1 "first")
                              (lsp-bridge-diagnostic-test--diagnostic 4 7 2 "second")
                              (lsp-bridge-diagnostic-test--diagnostic 8 12 4 "hidden"))))
       (lsp-bridge-diagnostic--render (buffer-file-name) "" diagnostics 3)
       (should updated)
       (should (equal lsp-bridge-diagnostic-count 3))
       (should (equal lsp-bridge-diagnostic-records diagnostics))
       (should (equal (length lsp-bridge-diagnostic-overlays) 2))
       (let ((overlay (car lsp-bridge-diagnostic-overlays)))
         (should (eq (overlay-get overlay 'face) 'lsp-bridge-diagnostics-error-face))
         (should (equal (overlay-get overlay 'message) "first"))
         (should (string-match-p "\\[1:3\\]" (overlay-get overlay 'display-message))))
       (let ((overlay (nth 1 lsp-bridge-diagnostic-overlays)))
         (should (eq (overlay-get overlay 'face) 'lsp-bridge-diagnostics-warning-face))
         (should (string-match-p "\\[2:3\\]" (overlay-get overlay 'display-message)))
         (should (equal (overlay-start overlay)
                        (acm-backend-lsp-position-to-point '(:line 0 :character 4)))))))))

(ert-deftest lsp-bridge-diagnostic-test-empty-range-gets-one-character ()
  "A diagnostic that covers no text still gets one character to show."
  (lsp-bridge-diagnostic-test--with-file
   (let ((lsp-bridge-diagnostic-enable-overlays t)
         (lsp-bridge-diagnostic-hide-severities nil))
     (setq-local lsp-bridge-diagnostic-overlays nil)
     (lsp-bridge-diagnostic--render (buffer-file-name) ""
                                    (list (lsp-bridge-diagnostic-test--diagnostic 4 4 1 "at point"))
                                    1)
     (should (equal (length lsp-bridge-diagnostic-overlays) 1))
     (let ((overlay (car lsp-bridge-diagnostic-overlays)))
       (should (equal (- (overlay-end overlay) (overlay-start overlay)) 1))
       (should (equal (overlay-get overlay 'display-message) "at point"))))))

(ert-deftest lsp-bridge-diagnostic-test-overlay-is-found-at-point ()
  "An overlay is found for the text it covers, and not at its end."
  (with-temp-buffer
    (insert "one two three\n")
    (setq-local lsp-bridge-diagnostic-overlays nil)
    (let ((overlay (make-overlay 5 8)))
      (setq lsp-bridge-diagnostic-overlays (list overlay))
      (goto-char 5)
      (should (eq (lsp-bridge-diagnostic-overlay-at-point) overlay))
      (goto-char 8)
      (should-not (lsp-bridge-diagnostic-overlay-at-point)))))

(ert-deftest lsp-bridge-diagnostic-test-hide-overlays-removes-them ()
  "Hiding deletes every overlay and forgets the list."
  (with-temp-buffer
    (insert "one two three\n")
    (setq-local lsp-bridge-diagnostic-overlays nil)
    (let ((first (make-overlay 1 4))
          (second (make-overlay 5 8)))
      (setq lsp-bridge-diagnostic-overlays (list first second))
      (lsp-bridge-diagnostic-hide-overlays)
      (should-not (overlay-start first))
      (should-not (overlay-start second))
      (should-not lsp-bridge-diagnostic-overlays))))

(ert-deftest lsp-bridge-diagnostic-test-insert-colored-string ()
  "The text of a tooltip line carries the colour it is given."
  (with-temp-buffer
    (lsp-bridge-diagnostic-insert-colored-string "red" "hello")
    (should (equal (buffer-string) "hello"))
    (should (equal (get-text-property 1 'face) '(:foreground "red")))))

(provide 'lsp-bridge-diagnostic-test)
;;; lsp-bridge-diagnostic-test.el ends here
