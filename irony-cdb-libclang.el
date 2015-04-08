;;; irony-cdb-libclang.el --- Compilation Database for irony using libclang

;; Copyright (C) 2014  Guillaume Papin
;;                     Karl Hylén

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;;         Karl Hylén <karl.hylen@gmail.com>
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
;; Compilation Database support for Irony using libclangs CXCompilationDatabase,
;; http://clang.llvm.org/doxygen/group__COMPILATIONDB.html
;;; Code:

(require 'irony-cdb)

(require 'cl-lib)

;;;###autoload
(defun irony-cdb-libclang (command &rest args)
  (cl-case command
    (get-compile-options (irony-cdb-libclang--get-compile-options))))

(defvar-local irony-cdb-libclang--server-compile-options nil
  "Compiler options got from irony-server.")

(defun irony-cdb-libclang--request-handler (flags result)
  (set result flags))

;; TODO: sometimes there is garbage on the callback-queue, determine why
(defun irony-cdb-libclang--get-compile-options ()
  (irony--awhen (irony-cdb-json--locate-db)
    (irony-cdb-libclang--server-exact-flags it)))

(defun irony-cdb-libclang--server-exact-flags (db-file)
  "Get compile options from server"
  (let ((project-root (file-name-directory db-file))
          (file buffer-file-name))
      ;; Reset compile options
      (setq irony-cdb-libclang--server-compile-options nil)
      ;; Get the compile options from the server
      (irony--send-request-sync
       "get-compile-options"
       (list 'irony-cdb-libclang--request-handler
             'irony-cdb-libclang--server-compile-options)
       project-root file)
      ;; Adjust the compile options and return
      (irony-cdb-libclang--adjust-options-and-remove-compiler
       file irony-cdb-libclang--server-compile-options)))

(defun irony-cdb-libclang--adjust-options-and-remove-compiler (file cmds)
  "Remove compiler, target file and output file from cmds

cmds is a list of conses, with car holding the options and cdr holding the
working directory where the command was issued."
  (mapcar (lambda (cmd)
          (let ((opt (car cmd))
                (wdir (cdr cmd)))
            (cons (cdr
                   (irony-cdb-json--adjust-compile-options
                    opt file wdir))
                  wdir)))
          cmds))

(provide 'irony-cdb-libclang)

;;; irony-cdb-libclang ends here
