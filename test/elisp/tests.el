(defvar test-dir (if load-file-name
                     (expand-file-name (concat (file-name-directory
                                                load-file-name))))
  "Directory of this script.")

(unless (require 'irony nil t)
  (if load-file-name
      (let ((irony-dir (expand-file-name test-dir "../../elisp"))))
        (add-to-list 'load-path irony-dir)
        (require 'irony)))

(unless (require 'ert nil t)
  ;; check stolen from https://github.com/dimitri/el-get
  (defmacro* ert-deftest (name () &body docstring-keys-and-body)
    (message "Skipping tests, ERT is not available")))

(ert-deftest dummy-test ()
  (should (= (+ 1 2) 3)))

