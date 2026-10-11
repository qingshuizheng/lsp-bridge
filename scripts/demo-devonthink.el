;;; demo-devonthink.el --- Show a DEVONthink completion end to end  -*- lexical-binding: t; -*-

;; Filename: demo-devonthink.el
;; Description: Drive one DEVONthink completion through the real acm path.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2026, Andy Stewart, all rights reserved.
;; Created: 2026-10-12 00:05:00
;; Version: 0.1
;; Last-Updated: 2026-10-12 00:05:00
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/lsp-bridge
;; Keywords: completion, devonthink
;; Compatibility: GNU Emacs 30.0.50

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; A manual check of the DEVONthink backend, kept next to the other scripts
;; because it cannot be part of the test suite: it needs DEVONthink running and
;; the automation permission granted, and it asks the real database.
;;
;; Run it with the Emacs that has the package installed:
;;
;;   emacs -Q --batch -l scripts/demo-devonthink.el
;;
;; It walks the same steps a keystroke does.  The first fetch asks for the
;; candidates while the debounced search is still on its way and answers nil;
;; the second one, after the AppleScript round trip, answers with the records,
;; and the selected one is expanded into an org link exactly as `acm-complete'
;; expands it.

;;; Code:

(require 'lsp-bridge)
(require 'acm-backend-devonthink)

(defun demo-devonthink--show (format &rest args)
  "Print a numbered line for the demonstration, FORMAT and ARGS."
  (princ (concat "  " (apply #'format format args) "\n")))

(with-temp-buffer
  (setq major-mode 'org-mode)
  (setq-local lsp-bridge-mode t)
  (setq-local acm-enable-devonthink t)
  ;; What the configuration sets, and the shape the trigger checks accept.
  (setq-local acm-input-bound-style "string")
  (insert "dt:a")
  (goto-char (point-max))
  (let* ((keyword (acm-get-input-prefix))
         (bound-start (cdr (acm-get-input-prefix-bound))))
    (demo-devonthink--show "buffer %S, acm prefix %S, bound-start %S"
                           (buffer-string) keyword bound-start)
    (demo-devonthink--show "first fetch (starts the debounced search): %S"
                           (mapcar (lambda (candidate) (plist-get candidate :label))
                                   (acm-backend-devonthink-candidates keyword)))
    (sleep-for 0.5)
    (accept-process-output acm-backend-devonthink--process 15)
    (sleep-for 0.3)
    (let* ((candidates (acm-backend-devonthink-candidates keyword))
           (pick (car candidates)))
      (demo-devonthink--show "second fetch: %d candidates" (length candidates))
      (dolist (candidate (seq-take candidates 3))
        (demo-devonthink--show "  %-34s [%s]"
                               (plist-get candidate :label)
                               (plist-get candidate :annotation)))
      (acm-backend-expand pick bound-start)
      (demo-devonthink--show "after choosing the first one: %S" (buffer-string))
      (demo-devonthink--show "its UUID starts with %s"
                             (substring (plist-get pick :uuid) 0 8)))))

(provide 'demo-devonthink)
;;; demo-devonthink.el ends here
