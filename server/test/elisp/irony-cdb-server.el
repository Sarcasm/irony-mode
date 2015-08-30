;; -*-no-byte-compile: t; -*-
(load
 (concat (file-name-directory (or load-file-name buffer-file-name))
         "test-config"))

(require 'irony-cdb-server)

;; Set the directory
(unless (boundp 'irony-elisp-test-dir)
  (setq irony-elisp-test-dir default-directory))
(message (concat "Source dir: " irony-elisp-test-dir))

(defconst llvm-db-file
  (expand-file-name "compile_commands.json" irony-elisp-test-dir))
(defconst src-file-names (mapcar #'(lambda (entry) (cdr (assoc 'file entry)))
                             (json-read-file llvm-db-file)))

(defun irony-cdb-json--exact-flags-for-testing (file file-cdb)
  (mapcar #'(lambda (e)
              (cons (nth 1 e) (nth 2 e)))
          (irony--assoc-all file file-cdb)))

(ert-deftest test-same-output-json-and-server ()
  "Tests that irony-cdb-json and irony-cdb-server give the same result."
    (let ((db (irony-cdb-json--load-db llvm-db-file)))
      ; Database should be loaded
      (should db)
      (dolist (src-file src-file-names)
        (let ((json-cmds
               (irony-cdb-json--exact-flags-for-testing src-file db))
              (server-cmds
               (irony-cdb-server--server-exact-flags src-file llvm-db-file)))
          ; If one file occurs several times, the commands might not be in
          ; order. Therefore, do not use equal directly.
          (should (equal (length json-cmds) (length server-cmds)))
          (dolist (json-cmd json-cmds)
            (should (member json-cmd server-cmds)))))))

(defconst before-db (expand-file-name "before.json" irony-elisp-test-dir))
(defconst after-db (expand-file-name "after.json" irony-elisp-test-dir))
(defconst reload-db (expand-file-name "reload.json" default-directory))

(ert-deftest test-database-reload ()
  "Test that the database is reloaded if updated."
  (let ((src-file "/home/user/directory/SrcFile.cpp"))
    (copy-file before-db reload-db t)
    (should (equal
             (caar (irony-cdb-server--server-exact-flags src-file reload-db))
             '( "original" )))
    (copy-file after-db reload-db t)
    (should (equal
             (caar (irony-cdb-server--server-exact-flags src-file reload-db))
             '( "changed" )))
    (delete-file reload-db)
    ))
