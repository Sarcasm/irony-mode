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
;; This file defines a compilation database for .clang_complete file.
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
    (irony--awhen (locate-dominating-file buffer-file-name ".clang_complete")
      (concat (file-name-as-directory it) ".clang_complete"))))

(defun irony-cdb-clang-complete--load-db (cc-file)
  (with-temp-buffer
    (insert-file-contents cc-file)
    ;; shell eval any forms in backticks
    (save-excursion
      (while (re-search-forward "`\\(.*?\\)`" nil t)
        (replace-match
         ;; strip newlines
         (replace-regexp-in-string "\n\\'" ""
                                   (shell-command-to-string
                                    (match-string 1))))))
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
