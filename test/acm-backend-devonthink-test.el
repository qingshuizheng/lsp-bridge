;;; acm-backend-devonthink-test.el --- DEVONthink completion backend  -*- lexical-binding: t; -*-

;;; Commentary:
;; No AppleScript runs here.  What is covered is what the backend builds and
;; what it does with an answer: the two trigger shapes, the search string and its
;; escaping, the parsing, the cache, the org link, and the exception that keeps
;; the popup alive after the trigger.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'acm-backend-devonthink)

(defmacro acm-backend-devonthink-test--with-backend (&rest body)
  "Run BODY with the backend enabled, a clean cache and no timer.
The debounce is set far into the future so that no test can start a real
AppleScript process."
  (declare (indent 0) (debug t))
  `(let ((acm-enable-devonthink t)
         (acm-backend-devonthink--results nil)
         (acm-backend-devonthink--pending nil)
         (acm-backend-devonthink--timer nil)
         (acm-backend-devonthink-debounce 3600))
     ,@body))

(ert-deftest acm-backend-devonthink-test-script-searches-record-names ()
  (acm-backend-devonthink-test--with-backend
    (let ((script (acm-backend-devonthink--script "foo")))
      ;; the application is addressed by its AppleScript id, which the renames
      ;; from DEVONthink Pro to DEVONthink 3 to DEVONthink left alone
      (should (string-match-p (regexp-quote "tell application id \"DNtp\"") script))
      (should (string-match-p (regexp-quote "search \"name:foo*\"") script))
      (should (string-match-p (regexp-quote "uuid of theRecord") script))
      ;; `tab' inside DEVONthink's tell block answers with the letters of its
      ;; name, so the separator is spelled out
      (should (string-match-p (regexp-quote "(ASCII character 9)") script))
      (should-not (string-match-p "& tab &" script)))))

(ert-deftest acm-backend-devonthink-test-a-running-search-offers-a-placeholder ()
  (acm-backend-devonthink-test--with-backend
    (setq acm-backend-devonthink--pending "foo")
    ;; the menu has to appear on the keystroke that starts the search
    (should (equal '("Searching DEVONthink...")
                   (mapcar (lambda (candidate) (plist-get candidate :label))
                           (acm-backend-devonthink-candidates "dt:foo"))))
    ;; and the placeholder cannot become a link
    (with-temp-buffer
      (insert "dt:foo")
      (goto-char (point-max))
      (acm-backend-devonthink-candidate-expand
       (car (acm-backend-devonthink-candidates "dt:foo")) (point-min) nil)
      (should (equal "dt:foo" (buffer-string))))))

(ert-deftest acm-backend-devonthink-test-script-escapes-the-query ()
  (acm-backend-devonthink-test--with-backend
    (should (string-match-p (regexp-quote "name:a\\\"b*")
                            (acm-backend-devonthink--script "a\"b")))))

(ert-deftest acm-backend-devonthink-test-parse-answers ()
  (acm-backend-devonthink-test--with-backend
    (let ((candidates (acm-backend-devonthink--parse
                       "UUID-1\tNotes\trtf\nUUID-2\tPapers\tpdf\n")))
      (should (equal '("Notes" "Papers")
                     (mapcar (lambda (candidate) (plist-get candidate :label)) candidates)))
      (should (equal "UUID-1" (plist-get (car candidates) :uuid)))
      (should (equal "pdf" (plist-get (cadr candidates) :annotation)))
      (should (equal "devonthink" (plist-get (car candidates) :backend))))))

(ert-deftest acm-backend-devonthink-test-trigger-shapes ()
  "The trigger is accepted inside the keyword and right before point."
  (acm-backend-devonthink-test--with-backend
    (with-temp-buffer
      (insert "dt:foo")
      (goto-char (point-max))
      ;; the string prefix style hands the trigger over inside the keyword
      (should (acm-backend-devonthink--trigger-p "dt:foo"))
      ;; the symbol and ascii styles leave it before point
      (should (acm-backend-devonthink--trigger-p "foo"))
      (should-not (acm-backend-devonthink--trigger-p "bar")))
    (with-temp-buffer
      (insert "xfoo")
      (goto-char (point-max))
      (should-not (acm-backend-devonthink--trigger-p "foo")))))

(ert-deftest acm-backend-devonthink-test-candidates-come-from-the-cache ()
  (acm-backend-devonthink-test--with-backend
    (setq acm-backend-devonthink--results
          (list (cons "foo" (cons (float-time)
                                  (list (list :label "Notes" :uuid "U1"))))))
    (should (equal '("Notes")
                   (mapcar (lambda (candidate) (plist-get candidate :label))
                           (acm-backend-devonthink-candidates "dt:foo"))))
    ;; the trigger on its own asks DEVONthink for nothing, but says so
    (should (equal '("Type to search DEVONthink")
                   (mapcar (lambda (candidate) (plist-get candidate :label))
                           (acm-backend-devonthink-candidates "dt:"))))
    (should-not acm-backend-devonthink--timer)))

(ert-deftest acm-backend-devonthink-test-expand-inserts-an-org-link ()
  (acm-backend-devonthink-test--with-backend
    (cl-flet ((insert-link (text bound-start)
                (with-temp-buffer
                  (insert text)
                  (goto-char (point-max))
                  (acm-backend-devonthink-candidate-expand
                   (list :label "Notes" :uuid "U1") bound-start nil)
                  (buffer-string))))
      ;; the string prefix style: the prefix carries the trigger
      (should (equal "[[x-devonthink-item://U1][Notes]]"
                     (insert-link "dt:foo" (point-min))))
      ;; the symbol styles: the trigger sits right before the prefix and goes too
      (should (equal "[[x-devonthink-item://U1][Notes]]"
                     (insert-link "dt:foo" 4)))
      ;; without a trigger in front, nothing else is removed
      (should (equal "x[[x-devonthink-item://U1][Notes]]"
                     (insert-link "xfoo" 2))))))

(ert-deftest acm-backend-devonthink-test-colon-after-the-trigger-keeps-the-popup ()
  "The trigger ends in a colon, which would otherwise hide the menu."
  (dolist (text '("dt:" "dt:f" "dt:foo"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-max))
      (should (lsp-bridge--not-match-hide-characters))))
  ;; without a colon nothing hides the popup in the first place
  (dolist (text '("dt" "foo"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-max))
      (should (lsp-bridge--not-match-hide-characters))))
  (dolist (text '("ab:" "xfoo:"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-max))
      (should-not (lsp-bridge--not-match-hide-characters)))))

(provide 'acm-backend-devonthink-test)
;;; acm-backend-devonthink-test.el ends here
