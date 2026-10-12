;;; acm-frame-test.el --- Tests for the popup frame helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Most of the frame module is about frames that cannot be made in batch Emacs, but
;; three of its helpers are not: blending two colours, reading whether the theme is
;; dark or light from the frame, and saying whether a child frame could be displayed
;; at all - which in batch it could not.
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/acm-frame-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'acm-frame)

(ert-deftest acm-frame-test-blending-at-the-ends ()
  "An alpha of one is the first colour and of nought the second."
  (should (equal (acm-frame-color-blend "#ff0000" "#0000ff" 1.0) "#ff0000"))
  (should (equal (acm-frame-color-blend "#ff0000" "#0000ff" 0.0) "#0000ff")))

(ert-deftest acm-frame-test-blending-in-the-middle ()
  "Half way between two colours is the middle of each of their channels."
  ;; The channels run to 65535, and half of that rounds to 32768, whose high byte
  ;; is 128 - so the middle of two colours is 80 and not 7f.
  (should (equal (acm-frame-color-blend "#000000" "#ffffff" 0.5) "#808080"))
  (should (equal (acm-frame-color-blend "#ff0000" "#00ff00" 0.5) "#808000"))
  ;; The result is written in lower case, whatever case came in.
  (should (equal (acm-frame-color-blend "#FFFFFF" "#FFFFFF" 0.5) "#ffffff")))

(ert-deftest acm-frame-test-the-theme-mode-is-read-from-the-frame ()
  "Whether the theme is dark or light is read from the frame's parameter."
  (let ((before (frame-parameter nil 'background-mode)))
    (unwind-protect
        (progn
          (set-frame-parameter nil 'background-mode 'dark)
          (should (equal (acm-frame-get-theme-mode) "dark"))
          (set-frame-parameter nil 'background-mode 'light)
          (should (equal (acm-frame-get-theme-mode) "light")))
      (set-frame-parameter nil 'background-mode before))))

(ert-deftest acm-frame-test-a-child-frame-cannot-be-displayed-in-batch ()
  "Batch Emacs cannot show a child frame, and says so."
  (should-not (acm-frame-can-display-p)))

(provide 'acm-frame-test)
;;; acm-frame-test.el ends here
