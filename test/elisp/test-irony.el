(load (concat (file-name-directory (or load-file-name
                                       buffer-file-name))
              "test-config"))

(ert-deftest irony/include-directories-1 ()
  (let ((irony-compile-flags '("-Iinclude" "-I/tmp/foo"))
        (irony-compile-flags-work-dir "/tmp/blah/"))
    (should (equal
             '("/tmp/blah/include" "/tmp/foo")
             (irony-header-search-paths)))))

(ert-deftest irony/include-directories-2 ()
  (let ((irony-compile-flags '("-Wextra" "-Iinclude" "-I" "foo" "-Wall"))
        (irony-compile-flags-work-dir "/tmp/blah/"))
    (should (equal
             '("/tmp/blah/include"
               "/tmp/blah/foo")
             (irony-header-search-paths)))))

(ert-deftest irony/extract-working-dir-flag/present-1 ()
  (let ((extra-flags '("-working-directory" "/tmp/lol")))
    (should (equal "/tmp/lol"
                   (irony-extract-working-dir-flag extra-flags)))))

(ert-deftest irony/extract-working-dir-flag/present-2 ()
  (let ((extra-flags '("-Wall" "-working-directory=/tmp/lol" "-Wshadow")))
    (should (equal "/tmp/lol"
                   (irony-extract-working-dir-flag extra-flags)))))
