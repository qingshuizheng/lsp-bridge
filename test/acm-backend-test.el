;;; acm-backend-test.el --- Tests for the backend registry  -*- lexical-binding: t; -*-

;;; Commentary:

;; The registry is the only place that knows which backends exist, so these
;; tests pin it down: the shape of a registration, the candidate protocol it
;; normalizes, the rules that decide whether a backend is asked at all
;; (enabled, inside a comment, short-circuit, deduplication) and the order in
;; which the menu merges what came back.
;;
;;   Emacs -Q --batch -L . -L acm -L test -l test/acm-backend-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'acm)
(require 'acm-backend)

(defvar acm-backend-test--calls nil
  "Backends whose fetch function ran in the current test, most recent first.")

(defvar acm-backend-test--candidates nil
  "Alist of stub backend name to the labels its fetch function returns.")

(defun acm-backend-test--fetch (name)
  "Return the fetch function of the stub backend called NAME.
It records the call and answers with the labels of
`acm-backend-test--candidates'."
  (lambda (_keyword)
    (push name acm-backend-test--calls)
    (mapcar (lambda (label) (list :label label :backend name))
            (cdr (assoc name acm-backend-test--candidates)))))

(defun acm-backend-test--backend (name group &rest plist)
  "Return a stub backend called NAME contributing to GROUP.
PLIST is passed on to `make-acm-backend'."
  (apply #'make-acm-backend
         :name name :group group :fetch-fn (acm-backend-test--fetch name)
         plist))

(defmacro acm-backend-test--with-buffer (text &rest body)
  "Run BODY in a temporary `emacs-lisp-mode' buffer holding TEXT.
Point starts at the end of TEXT, which is where the completion prefix is
read from."
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,text)
     (goto-char (point-max))
     (let ((acm-backend-test--calls nil)
           (acm-backend-test--candidates nil))
       ,@body)))

(defun acm-backend-test--labels (candidates)
  "Return the labels of CANDIDATES."
  (mapcar (lambda (candidate) (plist-get candidate :label)) candidates))

;;; The candidate protocol

(ert-deftest acm-backend-test-protocol-make ()
  "`acm-candidate-make' fills the defaults and refuses a broken candidate."
  (let ((candidate (acm-candidate-make :label "Alpha" :backend "test")))
    (should (equal (plist-get candidate :key) "Alpha"))
    (should (equal (plist-get candidate :displayLabel) "Alpha")))
  ;; An explicit key/display label is kept.
  (let ((candidate (acm-candidate-make :label "Alpha" :backend "test"
                                       :key "a" :displayLabel "A")))
    (should (equal (plist-get candidate :key) "a"))
    (should (equal (plist-get candidate :displayLabel) "A")))
  (should-error (acm-candidate-make :backend "test"))
  (should-error (acm-candidate-make :label "Alpha")))

(ert-deftest acm-backend-test-protocol-normalize ()
  "Normalizing fills the backend in and drops what is not a candidate."
  (let ((candidates (acm-candidates-normalize
                     (list (list :label "Alpha")            ; no backend yet
                           (list :backend "other" :label "Beta")
                           (list :backend "test")           ; no label
                           "junk"
                           nil)
                     "test")))
    (should (equal (acm-backend-test--labels candidates) '("Alpha" "Beta")))
    (should (equal (plist-get (car candidates) :backend) "test"))
    (should (equal (plist-get (cadr candidates) :backend) "other"))
    (should (equal (plist-get (car candidates) :key) "Alpha"))))

;;; Registration

(ert-deftest acm-backend-test-register-replaces ()
  "Registering a name twice keeps one entry, the latest one."
  (let ((acm-backends nil))
    (acm-backend-register (acm-backend-test--backend "one" "one-candidates"))
    (acm-backend-register (acm-backend-test--backend "two" "two-candidates"))
    (acm-backend-register (acm-backend-test--backend "one" "other-candidates"))
    ;; Re-registering keeps the position, so the fetch order does not change
    ;; under the feet of a backend that short-circuits the fetching.
    (should (equal (acm-backend-names) '("one" "two")))
    (should (equal (acm-backend-group (acm-backend-find "one")) "other-candidates"))
    (should-not (acm-backend-find "unknown"))))

(ert-deftest acm-backend-test-built-ins-are-registered ()
  "The backends of this tree are all registered, with their capabilities."
  (should (equal (sort (acm-backend-names) #'string<)
                 '("capf" "copilot" "ctags" "devonthink" "elisp" "jupyter"
                   "lsp" "lsp-workspace-symbol" "path" "search-file-words"
                   "tabby" "telega" "tempel" "vulpea" "yas")))
  (let ((path (acm-backend-find "path"))
        (lsp (acm-backend-find "lsp"))
        (yas (acm-backend-find "yas"))
        (words (acm-backend-find "search-file-words")))
    (should (acm-backend-short-circuit path))
    (should (acm-backend-preview path))
    (should (acm-backend-skip-in-comment lsp))
    (should (acm-backend-preview lsp))
    (should (acm-backend-doc-fn lsp))
    (should (acm-backend-clean-fn lsp))
    ;; The template backends have no group: the menu places them itself.
    (should (acm-backend-template yas))
    (should-not (acm-backend-group yas))
    ;; The words of the project are dropped when the server had the label.
    (should (acm-backend-dedupe-against-lsp words))
    ;; The group of a backend is not its name.
    (should (equal (acm-backend-group words) "file-words-candidates"))))

;;; Fetching

(ert-deftest acm-backend-test-fetch-honours-enabled ()
  "A disabled backend is not asked at all."
  (let ((acm-backend-test--calls nil)
        (acm-backend-test--candidates '(("on" "one")))
        (acm-backends (list (acm-backend-test--backend
                             "off" "off-candidates"
                             :enabled (lambda () nil))
                            (acm-backend-test--backend
                             "on" "on-candidates"
                             :enabled (lambda () t)))))
    (should-not (acm-backend-fetch (acm-backend-find "off") "a"))
    (should (equal (acm-backend-test--labels (acm-backend-candidates "on" "a"))
                   '("one")))
    (should (equal acm-backend-test--calls '("on")))))

(ert-deftest acm-backend-test-fetch-honours-comments ()
  "A backend marked `skip-in-comment' is not asked inside a comment."
  (let ((acm-backend-test--calls nil)
        (acm-backend-test--candidates '(("ctags" "one")))
        (acm-backends (list (acm-backend-test--backend
                             "ctags" "ctags-candidates" :skip-in-comment t))))
    (cl-letf (((symbol-function 'acm-in-comment-p) (lambda () t)))
      (should-not (acm-backend-candidates "ctags" "a")))
    (should-not acm-backend-test--calls)          ; never asked inside a comment
    (cl-letf (((symbol-function 'acm-in-comment-p) (lambda () nil)))
      (should (equal (acm-backend-test--labels (acm-backend-candidates "ctags" "a"))
                     '("one"))))
    (should (equal acm-backend-test--calls '("ctags")))))

(ert-deftest acm-backend-test-fetch-short-circuits ()
  "A short-circuiting backend stops the fetching when it has candidates."
  (let ((acm-backend-test--calls nil)
        (acm-backend-test--candidates nil)
        (acm-backends (list (acm-backend-test--backend
                             "path" "path-candidates" :short-circuit t)
                            (acm-backend-test--backend "lsp" "lsp-candidates"))))
    ;; Nothing found: the language server is still asked.
    (should-not (acm-backends-group (acm-backends-fetch "a") "path-candidates"))
    (should (equal acm-backend-test--calls '("lsp" "path")))
    ;; A path at point is what the user types: the server is not asked.
    (setq acm-backend-test--calls nil
          acm-backend-test--candidates '(("path" "file.el")))
    (let ((queries (acm-backends-fetch "a")))
      (should (equal (acm-backend-test--labels (acm-backends-group queries "path-candidates"))
                     '("file.el")))
      (should-not (acm-backends-group queries "lsp-candidates"))
      (should (equal acm-backend-test--calls '("path"))))))

(ert-deftest acm-backend-test-fetch-skips-templates ()
  "The template backends are collected by the menu, not by the group fetch."
  (let ((acm-backend-test--calls nil)
        (acm-backend-test--candidates '(("lsp" "one") ("yas" "snippet")))
        (acm-backends (list (acm-backend-test--backend "yas" nil :template t)
                            (acm-backend-test--backend "lsp" "lsp-candidates"))))
    (should (equal (mapcar #'car (acm-backends-fetch "a")) '("lsp-candidates")))
    (should (equal acm-backend-test--calls '("lsp")))
    ;; They are still reachable by name for the ticker logic.
    (should (equal (acm-backend-test--labels (acm-backend-candidates "yas" "a"))
                   '("snippet")))))

(ert-deftest acm-backend-test-dedupe-against-lsp ()
  "Labels the language server already returned are dropped from a group."
  (let* ((queries (list (cons "lsp-candidates" (list (list :label "same")
                                                     (list :label "other")))
                        (cons "file-words-candidates" (list (list :label "same")
                                                            (list :label "word")))))
         (acm-backends (list (acm-backend-test--backend
                              "search-file-words" "file-words-candidates"
                              :dedupe-against-lsp t))))
    (acm-backends-dedupe-against-lsp queries)
    (should (equal (acm-backend-test--labels (acm-backends-group queries "file-words-candidates"))
                   '("word")))
    (should (equal (acm-backend-test--labels (acm-backends-group queries "lsp-candidates"))
                   '("same" "other")))))

(ert-deftest acm-backend-test-group-candidates-order ()
  "Groups are concatenated in the order the caller asks for."
  (let ((queries (list (cons "a-candidates" (list (list :label "a1") (list :label "a2")))
                       (cons "b-candidates" (list (list :label "b1"))))))
    (should (equal (acm-backend-test--labels
                    (acm-backends-group-candidates queries '("b-candidates" "a-candidates")))
                   '("b1" "a1" "a2")))
    (should (equal (acm-backend-test--labels
                    (acm-backends-group-candidates queries '("b-candidates" "unknown")))
                   '("b1")))))

;;; The menu pipeline

(ert-deftest acm-backend-test-pipeline-order ()
  "The merged menu follows the group order and the menu length split."
  (acm-backend-test--with-buffer "abc"
    (setq acm-backend-test--candidates '(("lsp" "lsp1" "lsp2" "lsp3")
                                         ("ctags" "ctags1")
                                         ("copilot" "cop1")))
    (let ((acm-backends (list (acm-backend-test--backend "lsp" "lsp-candidates")
                              (acm-backend-test--backend "ctags" "ctags-candidates")
                              (acm-backend-test--backend "copilot" "copilot-candidates")))
          (acm-completion-mode-candidates-merge-order '("lsp-candidates" "ctags-candidates"))
          (acm-backend-order '("mode-first-part-candidates"
                               "template-first-part-candidates"
                               "copilot-candidates"
                               "template-second-part-candidates"
                               "mode-second-part-candidates"))
          (acm-menu-length 10))
      (should (equal (acm-backend-test--labels (acm-update-candidates))
                     '("lsp1" "lsp2" "lsp3" "ctags1" "cop1")))
      ;; A shorter menu splits the mode candidates around the Copilot group.
      (let ((acm-menu-length 3))
        (should (equal (acm-backend-test--labels (acm-update-candidates))
                       '("lsp1" "lsp2" "lsp3" "cop1" "ctags1")))))))

(ert-deftest acm-backend-test-pipeline-templates-split ()
  "The first template candidates join the first menu page."
  (acm-backend-test--with-buffer "abc"
    (setq acm-backend-test--candidates '(("ctags" "ctags1" "ctags2" "ctags3")
                                         ("yas" "yas1" "yas2")
                                         ("tempel" "tmp1")))
    (let ((acm-backends (list (acm-backend-test--backend "ctags" "ctags-candidates")
                              (acm-backend-test--backend "yas" nil :template t)
                              (acm-backend-test--backend "tempel" nil :template t)))
          (acm-completion-mode-candidates-merge-order '("ctags-candidates"))
          (acm-backend-order '("mode-first-part-candidates"
                               "template-first-part-candidates"
                               "copilot-candidates"
                               "template-second-part-candidates"
                               "mode-second-part-candidates"))
          (acm-menu-length 10))
      ;; Templates are shown because the ticker asked for them.
      (setq-local acm-template-candidate-show-p t)
      (should (equal (acm-backend-test--labels (acm-update-candidates))
                     '("ctags1" "ctags2" "ctags3" "yas1" "yas2" "tmp1"))))))

(ert-deftest acm-backend-test-pipeline-path-short-circuit ()
  "A path at point replaces every other candidate."
  (acm-backend-test--with-buffer "abc"
    (setq acm-backend-test--candidates '(("path" "dir/file.el")
                                         ("lsp" "lsp1")))
    (let ((acm-backends (list (acm-backend-test--backend "path" "path-candidates"
                                                         :short-circuit t)
                              (acm-backend-test--backend "lsp" "lsp-candidates")))
          (acm-completion-mode-candidates-merge-order '("lsp-candidates"))
          (acm-menu-length 10))
      (should (equal (acm-backend-test--labels (acm-update-candidates))
                     '("dir/file.el")))
      (should (equal acm-backend-test--calls '("path"))))))

;;; Dispatch

(ert-deftest acm-backend-test-dispatch ()
  "Expanding, documenting and cleaning go through the registry."
  (let* ((expanded nil)
         (documented nil)
         (cleaned nil)
         (acm-backends
          (list (make-acm-backend
                 :name "test" :group "test-candidates"
                 :fetch-fn (acm-backend-test--fetch "test")
                 :expand-fn (lambda (candidate bound-start preview)
                           (setq expanded (list candidate bound-start preview)))
                 :doc-fn (lambda (candidate) (setq documented candidate) "doc")
                 :clean-fn (lambda () (setq cleaned t))
                 :preview t)))
         (candidate (list :label "Alpha" :backend "test")))
    (should (acm-backend-expand-p candidate))
    (should (acm-backend-preview-p candidate))
    (should (acm-backend-doc-p candidate))
    (acm-backend-expand candidate 3)
    (should (equal expanded (list candidate 3 nil)))
    (acm-backend-expand candidate 3 t)
    (should (equal expanded (list candidate 3 t)))
    (should (equal (acm-backend-doc candidate) "doc"))
    (should (equal documented candidate))
    (acm-backend-clean "test")
    (should cleaned)
    ;; A backend without those functions reports no capability.
    (let ((plain (list :label "Alpha" :backend "unknown")))
      (should-not (acm-backend-expand-p plain))
      (should-not (acm-backend-preview-p plain))
      (should-not (acm-backend-doc-p plain))
      (should-not (acm-backend-doc plain))
      (should-not (acm-backend-expand plain 3))
      (should-not (acm-backend-clean "unknown")))))

(ert-deftest acm-backend-test-a-signalling-backend-does-not-stop-the-others ()
  "One buggy backend must not hide what the others return.
This is the shape of the ekg report: its tag completion signalled with stale
match data when a background refresh asked for candidates, and the error aborted
the whole update, so a candidate another backend had already fetched never
reached the menu."
  (let ((acm-backends nil)
        (acm-backend-test--candidates '(("good" . ("Alpha"))))
        (acm-backend--reported-error nil)
        reported)
    (acm-backend-register
     (make-acm-backend :name "broken" :group "broken-candidates"
                       :fetch-fn (lambda (_keyword) (error "stale match data"))
                       :enabled (lambda () t)))
    (acm-backend-register (acm-backend-test--backend "good" "good-candidates"))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args) (setq reported (apply #'format format args)))))
      (let ((queries (acm-backends-fetch "x")))
        (should (equal '("Alpha")
                       (mapcar (lambda (candidate) (plist-get candidate :label))
                               (cdr (assoc "good-candidates" queries)))))
        (should-not (assoc "broken-candidates" queries))))
    (should (string-match-p "broken" reported))))


(provide 'acm-backend-test)
;;; acm-backend-test.el ends here
