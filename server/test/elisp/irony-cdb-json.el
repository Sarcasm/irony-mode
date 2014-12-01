;; -*-no-byte-compile: t; -*-
(load
 (concat (file-name-directory (or load-file-name buffer-file-name))
         "test-config"))

(require 'irony-cdb-json)

(defconst irony-cdb/compile-command
  '((file      . "../src/file.cc")
    (directory . "/home/user/project/build")
    (command   . "/usr/bin/clang++ -DSOMEDEF=1 -c -o file.o /home/user/project/src/file.cc")))

(ert-deftest cdb/parse/simple/path-is-absolute ()
  (should
   (equal "/home/user/project/src/file.cc"
          (nth 0 (irony-cdb-json--transform-compile-command
                  irony-cdb/compile-command)))))

(ert-deftest cdb/parse/simple/compile-options ()
  (should
   (equal '("-DSOMEDEF=1")
          (nth 1 (irony-cdb-json--transform-compile-command
                  irony-cdb/compile-command)))))

(ert-deftest cdb/parse/simple/invocation-directory ()
  (should
   (equal "/home/user/project/build"
          (nth 2 (irony-cdb-json--transform-compile-command
                  irony-cdb/compile-command)))))
