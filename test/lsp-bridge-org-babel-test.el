;;; lsp-bridge-org-babel-test.el --- Org server lookup tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'org)
(require 'lsp-bridge)

(defmacro lsp-bridge-test-with-org-block (language &rest body)
  "Run BODY in a temporary Org buffer with one LANGUAGE source block.
The buffer holds a single `#+begin_src LANGUAGE' block, point starts on
its first line, the variable `buffer-file-name' points at a temporary .org
file and
the language-server lookup is exercised with Org Babel support enabled."
  (declare (indent 1))
  `(let ((lsp-bridge-enable-org-babel t)
         (lsp-bridge-use-wenls-in-org-mode nil)
         (lsp-bridge-use-ds-pinyin-in-org-mode nil)
         (lsp-bridge-org-babel-lang-list '("python"))
         (org-src-lang-modes '(("jupyter-python" . python)))
         (lsp-bridge-python-lsp-server "basedpyright"))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert "#+begin_src " ,language "\nimport os\n#+end_src\n")
       (setq buffer-file-name
             (expand-file-name "lsp-bridge-org-babel-test.org" temporary-file-directory))
       (set-buffer-modified-p nil)
       (goto-char (point-min))
       (forward-line 1)
       ,@body)))

(ert-deftest lsp-bridge-org-inline-python-server ()
  (lsp-bridge-test-with-org-block "python"
    (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                   "basedpyright"))))

(ert-deftest lsp-bridge-org-inline-jupyter-python-server ()
  (lsp-bridge-test-with-org-block "jupyter-python"
    (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                   "basedpyright"))))

(ert-deftest lsp-bridge-org-server-from-unrelated-callback-buffer ()
  (lsp-bridge-test-with-org-block "python"
    (let ((filename buffer-file-name))
      (with-temp-buffer
        (should (equal (lsp-bridge-get-single-lang-server-by-file-mode filename)
                       "basedpyright"))))))

(ert-deftest lsp-bridge-org-src-edit-server-still-takes-precedence ()
  (lsp-bridge-test-with-org-block "unsupported-language"
    (let ((filename buffer-file-name)
          (marker (point-marker)))
      (with-temp-buffer
        (setq major-mode 'python-mode)
        (setq-local org-src-mode t)
        (setq-local org-src--beg-marker marker)
        (should (equal (lsp-bridge-get-single-lang-server-by-file-mode filename)
                       "basedpyright"))))))

(ert-deftest lsp-bridge-org-no-server-on-block-boundaries ()
  (lsp-bridge-test-with-org-block "python"
    (goto-char (point-min))
    (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name))
    (forward-line 2)
    (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name))))

(ert-deftest lsp-bridge-org-disabled-or-unsupported-babel ()
  (lsp-bridge-test-with-org-block "python"
    (let ((lsp-bridge-enable-org-babel nil))
      (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)))
    (let ((lsp-bridge-org-babel-lang-list '("bash")))
      (should-not (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)))))

(ert-deftest lsp-bridge-org-non-babel-server-options ()
  (lsp-bridge-test-with-org-block "python"
    (let ((lsp-bridge-use-wenls-in-org-mode t))
      (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                     "wen")))
    (let ((lsp-bridge-use-ds-pinyin-in-org-mode t))
      (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                     "ds-pinyin")))))

(ert-deftest lsp-bridge-org-missing-buffer ()
  (should-not
   (lsp-bridge-get-single-lang-server-by-file-mode
    (expand-file-name "lsp-bridge-no-buffer.org" temporary-file-directory))))

(ert-deftest lsp-bridge-regular-python-server-unchanged ()
  (with-temp-buffer
    (setq buffer-file-name
          (expand-file-name "lsp-bridge-org-babel-test.py" temporary-file-directory))
    (setq major-mode 'python-mode)
    (let ((lsp-bridge-python-lsp-server "basedpyright"))
      (should (equal (lsp-bridge-get-single-lang-server-by-file-mode buffer-file-name)
                     "basedpyright")))))

(ert-deftest lsp-bridge-org-babel-test-a-position-inside-the-block ()
  "The block is the stretch from its first body position to its last."
  (with-temp-buffer
    (setq-local lsp-bridge-org-babel--block-bop 10)
    (setq-local lsp-bridge-org-babel--block-eop 20)
    ;; Both ends count as inside.
    (should (lsp-bridge-org-babel-in-block-p 10))
    (should (lsp-bridge-org-babel-in-block-p 15))
    (should (lsp-bridge-org-babel-in-block-p 20))
    (should-not (lsp-bridge-org-babel-in-block-p 9))
    (should-not (lsp-bridge-org-babel-in-block-p 21))))

(ert-deftest lsp-bridge-org-babel-test-a-block-that-is-not-known ()
  "Nothing is in a block before one has been found."
  (with-temp-buffer
    (should-not (lsp-bridge-org-babel-in-block-p 10))
    (setq-local lsp-bridge-org-babel--block-bop 10)
    (should-not (lsp-bridge-org-babel-in-block-p 10))))

(ert-deftest lsp-bridge-org-babel-test-cleaning-the-cache ()
  "Cleaning forgets the block and what was parsed for it."
  (with-temp-buffer
    (setq-local lsp-bridge-org-babel--info-cache 'something)
    (setq-local lsp-bridge-org-babel--block-bop 10)
    (setq-local lsp-bridge-org-babel--block-eop 20)
    (lsp-bridge-org-babel-clean-cache)
    (should-not lsp-bridge-org-babel--info-cache)
    (should-not lsp-bridge-org-babel--block-bop)
    (should-not lsp-bridge-org-babel--block-eop)))

(ert-deftest lsp-bridge-org-babel-test-a-change-inside-the-block-moves-its-end ()
  "Typing inside the block makes its end follow the text that was added."
  (with-temp-buffer
    (org-mode)
    (setq-local lsp-bridge-enable-org-babel t)
    (setq-local lsp-bridge-org-babel--info-cache 'block)
    (setq-local lsp-bridge-org-babel--block-bop 10)
    (setq-local lsp-bridge-org-babel--block-eop 20)
    ;; The arguments of an after-change call: BEGIN and END cover the text as it is
    ;; now, and LENGTH is how long the text it replaced was.  Three characters were
    ;; inserted at 15, so the end of the block follows them.
    (lsp-bridge-org-babel-monitor-after-change 15 18 0)
    (should (equal lsp-bridge-org-babel--block-eop 23))
    (should (eq lsp-bridge-org-babel--info-cache 'block))
    ;; Three characters were deleted at 15: nothing stands there now, and the text
    ;; that was there was three characters long.
    (lsp-bridge-org-babel-monitor-after-change 15 15 3)
    (should (equal lsp-bridge-org-babel--block-eop 20))))

(ert-deftest lsp-bridge-org-babel-test-a-change-elsewhere-forgets-the-block ()
  "A change outside the block, or one that collapses it, clears the cache."
  (with-temp-buffer
    (org-mode)
    (setq-local lsp-bridge-enable-org-babel t)
    (setq-local lsp-bridge-org-babel--info-cache 'block)
    (setq-local lsp-bridge-org-babel--block-bop 10)
    (setq-local lsp-bridge-org-babel--block-eop 20)
    ;; One character was typed outside the block.
    (lsp-bridge-org-babel-monitor-after-change 30 31 0)
    (should-not lsp-bridge-org-babel--info-cache))
  (with-temp-buffer
    (org-mode)
    (setq-local lsp-bridge-enable-org-babel t)
    (setq-local lsp-bridge-org-babel--info-cache 'block)
    (setq-local lsp-bridge-org-babel--block-bop 10)
    (setq-local lsp-bridge-org-babel--block-eop 11)
    ;; Ten characters were deleted, which brings the end to the beginning.
    (lsp-bridge-org-babel-monitor-after-change 10 10 10)
    (should-not lsp-bridge-org-babel--info-cache)))

(ert-deftest lsp-bridge-org-babel-test-nothing-is-monitored-outside-org ()
  "A change in a buffer that is not Org is left alone."
  (with-temp-buffer
    (setq-local lsp-bridge-enable-org-babel t)
    (setq-local lsp-bridge-org-babel--block-bop 10)
    (setq-local lsp-bridge-org-babel--block-eop 20)
    (lsp-bridge-org-babel-monitor-after-change 30 30 0)
    (should (equal lsp-bridge-org-babel--block-eop 20))))

(ert-deftest lsp-bridge-org-babel-test-the-block-is-only-pushed-when-it-changed ()
  "The block is sent to the server once, and only when it has changed."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src python\n")
    (insert "print(1)\n")
    (insert "#+end_src\n")
    (setq-local lsp-bridge-enable-org-babel t)
    (setq-local lsp-bridge-org-babel--block-bop (save-excursion
                                                  (goto-char (point-min))
                                                  (forward-line 1)
                                                  (point)))
    (setq-local lsp-bridge-org-babel--update-file-before-change t)
    (let (calls)
      (cl-letf (((symbol-function 'lsp-bridge-call-file-api)
                 (lambda (&rest args) (push args calls))))
        (lsp-bridge-org-babel-send-src-block-to-lsp-server)
        ;; The flag is cleared, so a second call sends nothing.
        (lsp-bridge-org-babel-send-src-block-to-lsp-server))
      (should (equal (length calls) 1))
      (should (equal (car (car calls)) "update_file"))
      ;; The block is reported at the first line of its body, counted from nought.
      (should (equal (nth 2 (car calls)) 1)))
    (should-not lsp-bridge-org-babel--update-file-before-change)))

;;; lsp-bridge-org-babel-test.el ends here
