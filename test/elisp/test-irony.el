(load (concat (file-name-directory (or load-file-name
                                       buffer-file-name))
              "test-config"))

(ert-deftest irony/test-addition ()
  (should (= (+ 1 2) 3)))
