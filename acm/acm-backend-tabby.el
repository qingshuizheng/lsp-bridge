;;; acm-backend-tabby.el --- acm tabby support -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:

(defgroup acm-backend-tabby nil
  "ACM tabby support."
  :group 'acm)

(defcustom acm-enable-tabby nil
  "Enable tabby support."
  :type 'boolean
  :group 'acm-backend-tabby)

(defvar-local acm-backend-tabby-items nil
  "List of tabby completion candidates recorded for this buffer.")

(defun acm-backend-tabby-candidates (keyword)
  "Return tabby completion candidates matching KEYWORD.
Candidates are read from `acm-backend-tabby-items', sorted by prefix
for KEYWORD and memoized in `acm-backend-tabby-cache-candiates'."
  (acm-with-cache-candidates
   acm-backend-tabby-cache-candiates
   (acm-candidate-sort-by-prefix
    keyword
    acm-backend-tabby-items)))


(defun acm-backend-tabby-find-max-common (str1 str2)
  "Return STR1 and STR2 joined on their longest overlap, or nil.
The overlap is the longest suffix of STR1 that is also a prefix of
STR2; the two strings are concatenated without repeating it.  Return
nil when they do not overlap."
  (let* ((max-count (min (length str1) (length str2)))
         (overlap-size
          (catch 'found
            (dotimes (i max-count)
              (let ((size (- max-count i)))
                (when (string= (substring str1 (- (length str1) size))
                               (substring str2 0 size))
                  (throw 'found size))))
            0)))
    (if (and (> overlap-size 0)
             (string= (substring str1 (- (length str1) overlap-size))
                      (substring str2 0 overlap-size)))
        (concat str1 (substring str2 overlap-size))
      nil)))


(defun acm-backend-tabby--get-candidates (&optional current-symbol is-manual)
  "Ask tabby for completion candidates in the current buffer.
CURRENT-SYMBOL, when a string, is merged with the symbol before point
into a longer completion by `acm-backend-tabby-find-max-common'.
IS-MANUAL is passed to `tabby--get-completion-context'.  Cancel the
ongoing tabby request instead of starting a new one.  The candidates
are stored through `lsp-bridge-search-backend--record-items'."
  (interactive)
  (when (string= tabby--status "initialization_done")
    (if (not (zerop tabby--ongoing-request-id))
        (tabby--agent-cancel-request tabby--ongoing-request-id)
      (let* ((request (tabby--get-completion-context is-manual))
             (before-request-pos (point))
             (before-request-symbol (thing-at-point 'symbol t))
             (on-response `(lambda (response)
                             (let* ((choices (plist-get (cadr response) :choices))
                                    (candidates (mapcar (lambda (choice) (plist-get choice :text))
                                                        choices))
                                    (candidates (if (stringp ,current-symbol)
                                                    (mapcar (lambda (choice) (if (acm-backend-tabby-find-max-common ,before-request-symbol choice)
                                                                                 (acm-backend-tabby-find-max-common ,before-request-symbol choice)
                                                                               choice)) candidates)
                                                  candidates))

                                    (candidates (seq-filter #'stringp candidates))

                                    (candidates-with-metadata (mapcar (lambda (candidate)
                                                                        (list :key candidate
                                                                              :icon "tabby"
                                                                              :label candidate
                                                                              :displayLabel candidate
                                                                              :annotation "tabby"
                                                                              :backend "tabby"))
                                                                      candidates)))
                               (when candidates
                                 (lsp-bridge-search-backend--record-items "tabby" candidates-with-metadata))))))

        (setq tabby--current-completion-request request)
        (setq tabby--ongoing-request-id
              (tabby--agent-provide-completions request on-response))))))

(defun acm-backend-tabby-record (current-symbol)
  "Request tabby completions for the current buffer.
Load the tabby library unless it is already available, clear
`acm-backend-tabby-items', then ask for completions filtered by
CURRENT-SYMBOL."
  (interactive)
  (unless (fboundp 'tabby--agent-provide-completions)
    (require 'tabby))
  (setq-local acm-backend-tabby-items nil)
  (acm-backend-tabby--get-candidates current-symbol nil))

(defun acm-backend-tabby-clean ()
  "Clear the tabby candidates and the candidate cache in this buffer."
  (setq-local acm-backend-tabby-items nil)
  (setq-local acm-backend-tabby-cache-candiates nil))

(provide 'acm-backend-tabby)
;;; acm-backend-tabby.el ends here
