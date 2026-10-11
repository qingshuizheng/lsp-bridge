;;; acm-backend-capf-test.el --- The CAPF backend and its exit function  -*- lexical-binding: t -*-

;;; Commentary:
;; A CAPF candidate is the complete text that replaces the region its
;; `completion-at-point-functions' entry returned.  ekg's transclusion
;; completion is the case that shows why it matters: its region begins after the
;; ">t" the user typed, so anything prepended to the candidates would appear
;; twice in the buffer and would be the string handed to `:exit-function'.

;;; Code:

(require 'ert)
(require 'acm-backend-capf)

(defun acm-backend-capf-test--setup (exit-fn)
  "Install a CAPF whose region starts after the \">t\" in the buffer."
  (let ((start (+ (point-min) 2))
        (end (point-max)))
    (setq-local major-mode 'acm-backend-capf-test-mode)
    (setq-local completion-at-point-functions
                (list (lambda ()
                        (list start end '("Foo" "Fob")
                              :exit-function exit-fn))))
    (cons start end)))

(ert-deftest acm-backend-capf-test-candidates-are-the-complete-text ()
  "Candidates are handed over as they are, without the text before the region."
  (with-temp-buffer
    (insert ">tFo")
    (let ((acm-backend-capf-mode-list '(acm-backend-capf-test-mode)))
      (acm-backend-capf-test--setup #'ignore)
      ;; the keyword carries ">t" (the string prefix style gives it that way),
      ;; while the CAPF region starts after it
      (should (equal '("Foo" "Fob")
                     (mapcar (lambda (c) (plist-get c :label))
                             (acm-backend-capf-candidates ">tFo")))))))

(ert-deftest acm-backend-capf-test-expand-replaces-the-capf-region ()
  "Expansion replaces the CAPF region and reports the text to the exit function."
  (with-temp-buffer
    (insert ">tFo")
    (let ((got nil)
          (acm-backend-capf-mode-list '(acm-backend-capf-test-mode))
          bounds)
      (setq bounds (acm-backend-capf-test--setup
                    (lambda (string status) (setq got (list string status)))))
      (acm-backend-capf-candidate-expand (list :label "Foo") (car bounds) nil)
      ;; the ">t" is left alone: it is not part of the region
      (should (equal ">tFoo" (buffer-string)))
      (should (equal '("Foo" finished) got)))))

(ert-deftest acm-backend-capf-test-exit-function-errors-are-reported ()
  "A failing exit function must not undo the completion."
  (with-temp-buffer
    (insert ">tFo")
    (let ((acm-backend-capf-mode-list '(acm-backend-capf-test-mode))
          (got nil) (reported nil) bounds)
      (setq bounds (acm-backend-capf-test--setup
                    (lambda (string status) (setq got (list string status))
                      (error "ekg would signal here"))))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (setq reported (apply #'format format args)))))
        (acm-backend-capf-candidate-expand (list :label "Foo") (car bounds) nil))
      ;; the text is in the buffer, the string handed over is the completed text
      (should (equal ">tFoo" (buffer-string)))
      (should (equal '("Foo" finished) got))
      (should (string-match-p "exit-function signalled" reported)))))


(provide 'acm-backend-capf-test)
;;; acm-backend-capf-test.el ends here
