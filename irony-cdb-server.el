;;; irony-cdb-server.el --- Compilation Database querying irony-server

;; Copyright (C) 2015  Karl Hylén

;; Author: Karl Hylén <karl.hylen@gmail.com>
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

;;; Code:

(require 'irony-cdb)
(require 'irony-cdb-json)

(require 'cl-lib)

;;;###autoload
(defun irony-cdb-server (command &rest args)
  (cl-case command
    (get-compile-options (irony-cdb-server--get-compile-options))))

(defun irony-cdb-server--get-compile-options ()
  (irony--awhen (irony-cdb-json--locate-db)
    (or (irony-cdb-server--server-exact-flags buffer-file-name it)
        (irony-cdb-server--server-guess-flags buffer-file-name it))))

(defun irony-cdb-server--server-exact-flags (src-file db-file)
  "Get compilation options from irony-server.

The parameter SRC-FILE is the source file we seek the compile command of and
DB-FILE is the database file."
  (irony-cdb-server--adjust-options-and-remove-compiler
   src-file
   (irony--send-request-sync "get-compile-options"
                             db-file
                             src-file)))

(defun irony-cdb-server--server-guess-flags (src-file db-file)
  "Make irony-server guess compilation arguments of a file.

The parameter SRC-FILE is the source file we seek the compile command of and
DB-FILE is the database file."
  (irony-cdb-server--adjust-options-and-remove-compiler
   src-file
   (list (irony--send-request-sync "guess-compile-options"
                                   db-file
                                   src-file))))

(defun irony-cdb-server--adjust-options-and-remove-compiler (file cmds)
  "Remove compiler, target file FILE and output file from CMDS.

The parameter CMDS is a list of conses. In each cons, the car holds the options
and the cdr holds the working directory where the compile command was issued."
  (mapcar (lambda (cmd)
            (let ((opt (irony-cdb--remove-compiler-from-flags (car cmd)))
                  (wdir (cdr cmd)))
              (cons
               (irony-cdb-json--adjust-compile-options opt file wdir)
               wdir)))
          cmds))

(provide 'irony-cdb-server)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-cdb-server ends here
