;;; irony-cdb.el --- compilation databases support for irony

;; Copyright (C) 2012-2014  Guillaume Papin

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
;; This file defines the compilation database interface of irony-mode.
;;
;; Note:For compilation database that looks for a specific file, such as
;; .clang_complete or compile_commands.json, favor `locate-dominating-file' to a
;; handwritten logic if possible as it may be configured by the user to do "the
;; Right Thing (TM)". See `locate-dominating-stop-dir-regexp'.
;;

;;; Code:

(require 'irony)

(require 'cl-lib)

(autoload 'irony-cdb-clang-complete "irony-cdb-clang-complete")
(autoload 'irony-cdb-json "irony-cdb-json")


;;
;; Customizable variables
;;

(defgroup irony-cdb nil
  "Irony's compilation database interface."
  :group 'irony)

(defcustom irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                             irony-cdb-json)
  "List of active compilation databases.

The compilation database should respond for the following commands:

`get-compile-options': Takes no argument. This function finds the
compile options used for the current buffer. It must return a
list of cons where the first element is a set of compile options
and the second element the working directory expected for these
commands. The compilation database should return an empty list
for files that it cannot handle."
  :type '(repeat function)
  :group 'irony-cdb)


;;
;; Internal variables
;;

(defvar-local irony-cdb--compilation-database nil)


;;
;; Irony Compilation Database Interface
;;

;;;###autoload
(defun irony-cdb-autosetup-compile-options ()
  (interactive)
  (irony--awhen (irony-cdb--autodetect-compile-options)
    (setq irony-cdb--compilation-database (nth 0 it))
    (irony-cdb--update-compile-options (nth 1 it) (nth 2 it))))

;;;###autoload
(defun irony-cdb-menu ()
  (interactive)
  (let ((compilation-database irony-cdb--compilation-database)
        (working-directory irony--working-directory)
        (compile-options irony--compile-options))
    (save-excursion
      (save-window-excursion
        (delete-other-windows)
        (let ((buffer (get-buffer-create "*Irony/Compilation DB Menu*")))
          (with-current-buffer buffer
            (erase-buffer)
            (if (null compilation-database)
                (insert "No compilation database in use.\n")
              (insert (format "Compilation Database: %s\n\n"
                              (symbol-name compilation-database)))
              (insert (format "  Working Directory: %s\n" working-directory))
              (insert (format "  Compile Options:   %s\n"
                              (mapconcat 'identity compile-options " "))))
            (insert "\n[q] to quit"))
          (let ((pop-up-windows t))
            (display-buffer buffer t))
          (fit-window-to-buffer (get-buffer-window buffer))
          (irony--read-char-choice "Irony CDB Buffer" (list ?q)))))
    ;; clear `read-char-choice' prompt
    (message "")))


;;
;; Functions
;;

(defun irony-cdb--update-compile-options (compile-options
                                          &optional working-directory)
  (setq irony--compile-options compile-options
        irony--working-directory working-directory))

(defun irony-cdb--autodetect-compile-options ()
  (catch 'found
    (dolist (compilation-database irony-cdb-compilation-databases)
      (irony--awhen (funcall compilation-database 'get-compile-options)
        (throw 'found (list compilation-database
                            (caar it)
                            (cdar it)))))))

(provide 'irony-cdb)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-cdb.el ends here
