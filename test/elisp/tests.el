(defvar test-dir (if load-file-name
                     (file-name-as-directory
                      (expand-file-name (concat (file-name-directory
                                                 load-file-name)))))
  "Directory of this script.")

(unless (require 'irony nil t)
  (if load-file-name
      (let ((irony-dir (expand-file-name test-dir "../../elisp"))))
    (add-to-list 'load-path irony-dir)
    (require 'irony)))

(or (require 'ert nil t)
 (let ((load-path (cons (expand-file-name "support" test-dir)
                        load-path)))
   (require 'ert)))

(ert-deftest dummy-test ()
  (should (= (+ 1 2) 3)))
