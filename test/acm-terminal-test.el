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
;;; acm-terminal-test.el ends here
