;;; lsp-bridge-jdtls-test.el --- Where JDTLS is told to keep things  -*- lexical-binding: t; -*-

;;; Commentary:
;; JDTLS is given a cache directory, a configuration file and a data directory,
;; and where those are is computed from the project and the file: a project gets a
;; directory named after it and hashed, a file opened on its own gets one named
;; after its own hash, and a project that is its own file counts as a single file.
;; That arithmetic is what is checked here, since it is the whole of it - no
;; server, no Java project.  The configuration that is written is checked through
;; the one function that writes it.

;;; Code:

(require 'ert)
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)

(defmacro lsp-bridge-jdtls-test--with-workspace (&rest body)
  "Run BODY with the JDTLS workspace in a fresh temporary directory."
  `(let ((lsp-bridge-jdtls-workspace (make-temp-file "lsp-bridge-jdtls-test" t)))
     (unwind-protect (progn ,@body)
       (delete-directory lsp-bridge-jdtls-workspace t))))

(ert-deftest lsp-bridge-jdtls-test-project-cache-dir ()
  "A project keeps its cache under its own name and hash."
  (lsp-bridge-jdtls-test--with-workspace
   (should (equal (lsp-bridge-jdtls-project-cache-dir "/tmp/my-project")
                  (expand-file-name (concat "my-project-" (md5 "/tmp/my-project"))
                                    lsp-bridge-jdtls-workspace)))))

(ert-deftest lsp-bridge-jdtls-test-single-file-cache-dir ()
  "A file on its own keeps its cache under its own hash."
  (lsp-bridge-jdtls-test--with-workspace
   (should (equal (lsp-bridge-jdtls-single-file-cache-dir "/tmp/One.java")
                  (expand-file-name (md5 "/tmp/One.java")
                                    lsp-bridge-jdtls-workspace)))))

(ert-deftest lsp-bridge-jdtls-test-cache-dir-picks-the-single-file-one ()
  "The same project and file is a file of its own, anything else a project."
  (lsp-bridge-jdtls-test--with-workspace
   (should (lsp-bridge-jdtls-single-file-mode? "/tmp/One.java" "/tmp/One.java"))
   (should (equal (lsp-bridge-jdtls-cache-dir "/tmp/One.java" "/tmp/One.java")
                  (lsp-bridge-jdtls-single-file-cache-dir "/tmp/One.java")))
   (should (equal (lsp-bridge-jdtls-cache-dir "/tmp/project" "/tmp/project/A.java")
                  (lsp-bridge-jdtls-project-cache-dir "/tmp/project")))))

(ert-deftest lsp-bridge-jdtls-test-config-file-and-data-dir ()
  "The configuration file and the data directory live in the cache directory."
  (lsp-bridge-jdtls-test--with-workspace
   (let ((cache (lsp-bridge-jdtls-project-cache-dir "/tmp/project")))
     (should (equal (lsp-bridge-jdtls-config-file "/tmp/project" "/tmp/project/A.java")
                    (expand-file-name lsp-bridge-jdtls-workspace-file-name cache)))
     (should (equal (lsp-bridge-jdtls-project-data-dir "/tmp/project" "/tmp/project/A.java")
                    (expand-file-name "jdtls-data" cache))))))

(ert-deftest lsp-bridge-jdtls-test-server-by-project-writes-the-config ()
  "Asking for the server of an unknown project writes its configuration once."
  (lsp-bridge-jdtls-test--with-workspace
   (let* ((project (make-temp-file "lsp-bridge-jdtls-project" t))
          (file (expand-file-name "A.java" project))
          (config (lsp-bridge-jdtls-config-file project file)))
     (unwind-protect
         (progn
           (should-not (file-exists-p config))
           (should (equal (lsp-bridge-get-jdtls-server-by-project project file) config))
           (should (file-exists-p config))
           (should (equal (lsp-bridge-get-jdtls-server-by-project project file) config)))
       (delete-directory project t)))))

(provide 'lsp-bridge-jdtls-test)
;;; lsp-bridge-jdtls-test.el ends here
