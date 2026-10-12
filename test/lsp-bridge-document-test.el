;;; lsp-bridge-document-test.el --- Rendering the documentation of a symbol  -*- lexical-binding: t; -*-

;;; Commentary:
;; The hover documentation of a symbol arrives as markdown, and the module puts it
;; into a buffer of its own, with the `&nbsp;' entities of the HTML inside it
;; turned into spaces and the buffer left read-only.  That much is text and a
;; buffer, and is what is checked here.
;;
;; The popup version fills its buffer the same way and then asks for an acm frame
;; next to it, and the frame has to be positioned against a real window: tried
;; here, the whole path from the frame to `window-pixel-edges' needs a display, so
;; it is not covered rather than covered by stubbing half of itself.  The signature
;; tooltip and the scrolling of either are not covered for the same reason.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-document)

(defmacro lsp-bridge-document-test--with-buffer (variable &rest body)
  "Run BODY with VARIABLE naming a fresh buffer, and kill it afterwards."
  `(let ((,variable (format " *lsp-bridge-document-test-%s*" (gensym))))
     (unwind-protect (progn ,@body)
       (when (get-buffer ,variable)
         (kill-buffer ,variable)))))

(ert-deftest lsp-bridge-document-test-replace-html-entities ()
  "Only `&nbsp;' is turned into a space, and every one of them."
  (with-temp-buffer
    (insert "one&nbsp;two&nbsp;three\n")
    (lsp-bridge-replace-html-entities)
    (should (equal (buffer-string) "one two three\n")))
  (with-temp-buffer
    (insert "nothing to replace\n")
    (lsp-bridge-replace-html-entities)
    (should (equal (buffer-string) "nothing to replace\n")))
  (with-temp-buffer
    ;; The other entities a server may send are left as they are; the docstring
    ;; names only `&nbsp;', and this says the same.
    (insert "a &amp; b &lt; c\n")
    (lsp-bridge-replace-html-entities)
    (should (equal (buffer-string) "a &amp; b &lt; c\n"))))

(ert-deftest lsp-bridge-document-test-show-documentation-callback ()
  "The callback fills the documentation buffer and leaves it read-only."
  (lsp-bridge-document-test--with-buffer lsp-bridge-buffer-documentation-buffer
    (lsp-bridge-show-documentation--callback "first&nbsp;line\n\nsecond\n")
    (let ((buffer (get-buffer lsp-bridge-buffer-documentation-buffer)))
      (should buffer)
      (with-current-buffer buffer
        (should (equal (buffer-string) "first line\n\nsecond\n"))
        (should buffer-read-only)
        (should-not truncate-lines)
        (should (equal (point) (point-min)))))))

(ert-deftest lsp-bridge-document-test-show-documentation-replaces-old-text ()
  "A second answer replaces the first one rather than adding to it."
  (lsp-bridge-document-test--with-buffer lsp-bridge-buffer-documentation-buffer
    (lsp-bridge-show-documentation--callback "old\n")
    (lsp-bridge-show-documentation--callback "new\n")
    (with-current-buffer (get-buffer lsp-bridge-buffer-documentation-buffer)
      (should (equal (buffer-string) "new\n")))))

(ert-deftest lsp-bridge-document-test-scrolling-without-a-popup ()
  "Scrolling the documentation popup when there is none does nothing.
The two commands are bound to keys of the popup, so they can be pressed while it
is already gone; the scroll command must not be reached then."
  (should-not (frame-live-p (acm-popup hover-frame)))
  (let (scrolled)
    (cl-letf (((symbol-function 'scroll-up-command) (lambda (&rest _args) (setq scrolled 'up)))
              ((symbol-function 'scroll-down-command) (lambda (&rest _args) (setq scrolled 'down))))
      (lsp-bridge-popup-documentation-scroll-up)
      (should-not scrolled)
      (lsp-bridge-popup-documentation-scroll-down)
      (should-not scrolled))))

(ert-deftest lsp-bridge-document-test-hiding-what-is-not-there ()
  "Hiding the tooltips asks the frame code to hide whatever the slot holds.
The slot is nil when nothing is shown, and that is passed on as it is: what to do
about a frame that is not there is the frame code's business, not this module's."
  (let (hidden)
    (cl-letf (((symbol-function 'acm-frame-hide-frame)
               (lambda (frame) (setq hidden frame))))
      (should-not (acm-popup hover-frame))
      (lsp-bridge-hide-doc-tooltip)
      (should (null hidden))
      (should-not (acm-popup signature-frame))
      (lsp-bridge-hide-signature-tooltip)
      (should (null hidden)))))

(provide 'lsp-bridge-document-test)
;;; lsp-bridge-document-test.el ends here
