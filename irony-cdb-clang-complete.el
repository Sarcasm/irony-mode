;;; irony-cdb-clang-complete.el --- .clang_complete compilation database

;; Copyright (C) 2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: c, convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file defines a compilation database for .clang_complete and
;; compile_flags.txt, both of which have the same format.
;;

;;; Code:

(require 'irony-cdb)

(require 'cl-lib)

;;;###autoload
(defun irony-cdb-clang-complete (command &rest args)
  (cl-case command
    (get-compile-options (irony-cdb-clang-complete--get-compile-options))))

(defun irony-cdb-clang-complete--get-compile-options ()
  (irony--awhen (irony-cdb-clang-complete--locate-db)
    (irony-cdb-clang-complete--load-db it)))

(defun irony-cdb-clang-complete--locate-db ()
  (when buffer-file-name
    (catch 'fname
      (locate-dominating-file
       buffer-file-name
       ;; locate-dominating-file will invoke the lambda on suitable
       ;; directories, and if we have either of our files there, we
       ;; return its filename, by throwing it.
       (lambda (d)
         (let ((cfname (concat (file-name-as-directory d) "compile_flags.txt"))
               (ccname (concat (file-name-as-directory d) ".clang_complete")))
           (if (file-exists-p cfname)
               (throw 'fname cfname)
             (if (file-exists-p ccname)
                 (throw 'fname ccname)
               nil))))))))

(defun irony-cdb-clang-complete--load-db (cc-file)
  (with-temp-buffer
    (insert-file-contents cc-file)
    (list
     (cons
      ;; compile options with trailing whitespaces removed
      (mapcar #'(lambda (line)
                  (if (string-match "[ \t]+$" line)
                      (replace-match "" t t line)
                    line))
              (split-string (buffer-string) "\n" t))
      ;; working directory
      (expand-file-name (file-name-directory cc-file))))))

(provide 'irony-cdb-clang-complete)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-cdb-clang-complete ends here
