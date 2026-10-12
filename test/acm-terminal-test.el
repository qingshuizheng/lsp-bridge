;;; acm-terminal-test.el --- The terminal menu front end  -*- lexical-binding: t; -*-

;;; Commentary:
;; `acm-terminal' draws the menu of acm with `popon' overlays, for an Emacs that
;; has no child frames, and it does that by putting its own functions in
;; `acm-ui-overrides' rather than by advising the functions of acm.  What is
;; checked here is that taking the menu over and giving it back leaves those
;; functions alone, that a missing dependency is reported instead of worked
;; around, and the wrapping the terminal menu is laid out with.

;;; Code:

(require 'ert)
(require 'acm)
(require 'acm-terminal)

(defconst acm-terminal-test-overridden
  '(acm-frame-init-colors acm-hide acm-update acm-doc-try-show acm-doc-hide
    acm-doc-scroll-up acm-doc-scroll-down acm-menu-max-length acm-menu-render
    acm-menu-render-items)
  "The functions of acm the terminal front end answers itself.")

(ert-deftest acm-terminal-test-no-advice-on-acm ()
  "Taking the menu over does not wrap any function of acm."
  (dolist (function acm-terminal-test-overridden)
    (should-not (advice-member-p 'acm-terminal-hide function))
    (should-not (advice-member-p 'acm-terminal-update function))
    (should-not (advice-member-p 'acm-terminal-doc-hide function))))

(ert-deftest acm-terminal-test-active-without-popon-says-so ()
  "Without `popon' the menu is not taken over, and nothing is changed."
  (let ((acm-terminal--popon-available nil)
        (acm-ui-overrides nil))
    (should-error (acm-terminal-active) :type 'user-error)
    (should-not acm-ui-overrides)))

(ert-deftest acm-terminal-test-active-and-deactive ()
  "The overrides of the terminal are installed and given back."
  (let ((acm-terminal--popon-available t)
        (acm-ui-overrides nil))
    (acm-terminal-active)
    (should (equal acm-ui-overrides acm-terminal-ui-overrides))
    (should (eq (cdr (assq 'acm-menu-render acm-ui-overrides))
                'acm-terminal-menu-render))
    (acm-terminal-deactive)
    (should-not acm-ui-overrides)))

(ert-deftest acm-terminal-test-nsplit-string-wraps-with-a-continuation ()
  "The wrapping the menu is laid out with: every piece is WIDTH wide.
The last piece is padded with spaces, and no empty piece is made when the text
divides evenly - both of which the examples in the docstring used to get wrong."
  (should (equal (acm-terminal-nsplit-string "foobarbazq" 3 "↩")
                 '("fo↩" "ob↩" "ar↩" "ba↩" "zq ")))
  (should (equal (acm-terminal-nsplit-string "foobarbazq" 3)
                 '("foo" "bar" "baz" "q  ")))
  (should (equal (acm-terminal-nsplit-string "foobar" 3)
                 '("foo" "bar"))))

(provide 'acm-terminal-test)
(ert-deftest acm-terminal-test-the-line-number-width ()
  "The menu leaves room for the line numbers only while they are shown."
  (with-temp-buffer
    (display-line-numbers-mode -1)
    (should (equal (acm-terminal-line-number-display-width) 0))
    (display-line-numbers-mode 1)
    (should (equal (acm-terminal-line-number-display-width)
                   (+ (line-number-display-width) 2)))))

(ert-deftest acm-terminal-test-the-menu-is-two-columns-wider ()
  "The menu is as wide as the candidates need and no wider than the window."
  (with-temp-buffer
    (should (equal (acm-terminal-max-length 10) 12))
    ;; It never goes past the width the window has room for.
    (should (<= (acm-terminal-max-length 1000) (window-width)))))

(ert-deftest acm-terminal-test-the-front-end-registers-itself ()
  "Turning the terminal front end on is what puts its dispatch points in place."
  (let ((before acm-ui-overrides))
    (unwind-protect
        (progn
          (acm-terminal-deactive)
          (should-not acm-ui-overrides)
          (acm-terminal-active)
          ;; The front end takes the menu and the documentation over through the
          ;; alist, rather than advising the commands of the menu.
          (should (eq acm-ui-overrides acm-terminal-ui-overrides))
          ;; The alist is a dispatch point of acm to what the terminal does in its
          ;; place: every key names one of those points, every value is a function.
          (should acm-terminal-ui-overrides)
          (should (seq-every-p #'symbolp (mapcar #'car acm-terminal-ui-overrides)))
          (should (seq-every-p (lambda (pair) (functionp (cdr pair)))
                               acm-terminal-ui-overrides))
          ;; One of them is read out of the list above rather than guessed at.
          (should (eq (cdr (assq 'acm-hide acm-terminal-ui-overrides))
                      'acm-terminal-hide)))
      ;; The alist is put back afterwards, whatever the tests above found.
      (setq acm-ui-overrides before))))

;;; acm-terminal-test.el ends here
