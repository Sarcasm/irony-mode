(load (concat (file-name-directory (or load-file-name
                                       buffer-file-name))
              "../test-config"))

(irony-enable 'compilation-db)

(require 'json)

(ert-deftest compilation-db/extract-working-dir/none-present ()
  (let ((extra-flags '("-Wall" "a.c")))
    (should
     (not (irony-compilation-db-extract-working-dir extra-flags)))))

(ert-deftest compilation-db/gen-clang-args-1/end-of-opts ()
  (should (equal
           (list nil nil)
           (irony-compilation-db-gen-clang-args-1 '("--" "a.c" "b.c")))))

(ert-deftest compilation-db/gen-clang-args/order-kept ()
  "Test if the arguments are given back in the same order as they
were given.

An argument my be dependent of the previous one, we want to keep
the order. -W"
  (let ((args '("-Wall" "-ferror-limit" "42" "-Wextra")))
    (should (equal
             (list nil args)
             (irony-compilation-db-gen-clang-args-1 args)))))

(ert-deftest compilation-db/gen-clang-args/remove-files-1 ()
  "Test if files are removed from the arguments list."
  (let ((args '("a.c" "b.c")))
    (should (equal
             (list nil nil)
             (irony-compilation-db-gen-clang-args-1 args)))))

(ert-deftest compilation-db/gen-clang-args/remove-files-2 ()
  "Test if files are removed from the arguments list."
  (let ((args '("-Wall" "-D_BLAH" "a.c" "b.c" "-Werror" "-I/usr/include/blah")))
    (should (equal
             (list '("/usr/include/blah")
                   '("-Wall" "-D_BLAH" "-Werror"))
             (irony-compilation-db-gen-clang-args-1 args)))))

(ert-deftest compilation-db/gen-clang-args/gen-workdir ()
  "Test if the -working-directory is correctly added."
  (let* ((entry (json-read-from-string
                 "{\
                   \"directory\": \"/tmp/blah\",\
                   \"command\"  : \"gcc -O2 -Wall -Iinclude src/main.c\",\
                   \"file\"     : \"/tmp/blah/main.c\"\
                  }"))
         (result (irony-compilation-db-parse-entry entry)))
    (prin1 result)
    (should (not (ert--plist-difference-explanation
                  (list :file "/tmp/blah/main.c"
                        :include-dirs '("include")
                        :extra-flags '("-O2" "-Wall")
                        :work-dir "/tmp/blah")
                  result)))))
