;; -*-no-byte-compile: t; -*-
(load (concat (file-name-directory (or load-file-name
                                       buffer-file-name))
              "test-config"))

(require 'irony-cdb)

;; (ert-deftest cdb/gen-clang-args/end-of-opts ()
;;   (should (not
;;            (irony-cdb-gen-clang-args '("--" "a.c" "b.c")))))

;; (ert-deftest cdb/gen-clang-args/order-kept ()
;;   "Test if the arguments are given back in the same order as they
;; were given.

;; An argument may be dependent of the previous one, it's important
;; to keep the ordering right."
;;   (let ((args '("-Wall" "-ferror-limit" "42" "-Wextra")))
;;     (should (equal
;;              args
;;              (irony-cdb-gen-clang-args args)))))

;; (ert-deftest cdb/gen-clang-args/remove-files-1 ()
;;   "Test if files are removed from the arguments list."
;;   (let ((args '("a.c" "b.c")))
;;     (should (not
;;              (irony-cdb-gen-clang-args args)))))

;; (ert-deftest cdb/gen-clang-args/remove-files-2 ()
;;   "Test if files are removed from the arguments list."
;;   (let ((args '("-Wall" "-D_BLAH" "a.c" "b.c" "-Werror" "-I/usr/include/blah")))
;;     (should (equal
;;              '("-Wall" "-D_BLAH" "-Werror" "-I/usr/include/blah")
;;              (irony-cdb-gen-clang-args args)))))
