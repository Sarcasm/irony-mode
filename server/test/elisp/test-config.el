;; -*-no-byte-compile: t; -*-
(defvar test-dir (if load-file-name
                     (file-name-as-directory
                      (expand-file-name (concat (file-name-directory
                                                 load-file-name)))))
  "Elisp test directory path.")

;; load irony
(unless (require 'irony nil t)
  (let ((irony-dir (expand-file-name "../../.." test-dir)))
    (add-to-list 'load-path irony-dir)
    (require 'irony)))

;; load ERT, fallback to a bundled version if not found in `load-path'
(unless (require 'ert nil t)
  (let ((load-path (cons (expand-file-name "support" test-dir)
                         load-path)))
    (require 'ert)))
