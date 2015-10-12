;; -*-no-byte-compile: t; -*-
(load
 (concat (file-name-directory (or load-file-name buffer-file-name))
         "test-config"))

(require 'irony-cdb-json)
(require 'cl-lib)

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

(ert-deftest cdb/choose-closest-path/chooses-closest ()
  (should
   (equal "/tmp/a/cdb"
          (irony-cdb--choose-closest-path "/tmp/a/1"
                                          '("/tmp/a/cdb" "/tmp/cdb")))))

(ert-deftest cdb/choose-closest-path/chooses-closest2 ()
  (should
   (equal "/tmp/a/cdb"
          (irony-cdb--choose-closest-path "/tmp/a/1"
                                          '("/tmp/cdb" "/tmp/a/cdb")))))

(ert-deftest cdb/choose-closest-path/prefers-deeper ()
  (should
   (equal "/tmp/a/build/cdb"
          (irony-cdb--choose-closest-path "/tmp/a/1"
                                          '("/tmp/a/build/cdb" "/tmp/cdb")))))

(ert-deftest cdb/choose-closest-path/prefers-deeper2 ()
  (should
   (equal "/tmp/a/build/cdb"
          (irony-cdb--choose-closest-path "/tmp/a/1"
                                          '("/tmp/cdb" "/tmp/a/build/cdb")))))

(ert-deftest cdb/choose-closest-path/will-survive-garbage ()
  (should
   (equal nil
          (irony-cdb--choose-closest-path "/tmp/a/1"
                                          'ordures))))

; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
(ert-deftest cdb/locate-db/choose-among-candidates ()
  (should
   (equal "/foo/build/cdb"
          (cl-letf (((symbol-function 'locate-dominating-file)
                     (lambda  (file name)
                       (cond
                         ((string= name "./cdb") "/") ; found /cdb
                         ((string= name "build/cdb") "/foo/") ; found /foo/build/cdb
                         ))))
            (irony-cdb--locate-dominating-file-with-dirs "/foo/bar/qux.cpp"
                                                         "cdb"
                                                         '("." "build" "out/x86_64"))))))

(ert-deftest cdb/locate-dominating-file-with-dirs/children-first ()
  (should
   (equal "/tmp/foo/bar/out/x86_64/cdb"
          (cl-letf (((symbol-function 'locate-dominating-file)
                     (lambda  (file name)
                       (cond
                         ((string= name "./cdb") "/tmp/foo/") ; found /tmp/foo/cdb
                         ((string= name "out/x86_64/cdb") "/tmp/foo/bar/") ;found /tmp/foo/bar/out/x86_64/cdb
                         ))))
                    (irony-cdb--locate-dominating-file-with-dirs "/tmp/foo/bar/qux.cpp"
                                                                 "cdb"
                                                                 '("." "out/x86_64" ))))))
