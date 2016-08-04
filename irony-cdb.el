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
(autoload 'irony-cdb-libclang "irony-cdb-libclang")


;;
;; Customizable variables
;;

(defgroup irony-cdb nil
  "Irony's compilation database interface."
  :group 'irony)

(defcustom irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                             irony-cdb-libclang
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

(defcustom irony-cdb-search-directory-list '("." "build")
  "List of relative subdirectory paths to be searched for cdb files

Irony looks for cdb files in any of the supported format by checking
each directory from the currently loaded file and recursively through
parent directories until it hits the root directory or a cdb is
found. At each level of the search irony looks at the subdirectories
listed in `'irony-cdb-search-directory-list` for the files. Customize this
list if your cdb is held in a custom directory within you project,
such as a custom named build directory.
"
  :type '(repeat string)
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

(defun irony-cdb--choose-closest-path (file paths)
  "Find the \"best\" path in PATHS matching FILE

If any paths in PATHS is belongs to the same directory
or a subdirectory of file, we disregard other candidates.

For remaining candidates, \"nearest\" is measured as abs. difference
in path depth.
- We prefer deeper paths at level +N to those at level -N.
- If multiple paths are equally good, we return the last one.

Returns nil if paths isn't a list of at least one element.
"
  (when (listp paths)
    (let ((paths (or
                  ;; if we find a cdb in cwd or below, don't consider other candidates
                  (cl-remove-if-not (lambda (x) (string-prefix-p (file-name-directory file) x)) paths)
                  paths)))
      (cl-loop for path in paths
         with best-depth-delta = 999999 ; start at +inf
         with best-path = nil ; we keep the best so far here
         ;; all candidates have their depth compared to that of target file
         with file-depth = (length (split-string (expand-file-name file) "/")) ;
         for candidate-depth = (length (split-string (expand-file-name path) "/"))
         ;; Our metric. We use signum as a tie-breaker to choose deeper candidates
         for depth-delta = (+ (abs (- file-depth candidate-depth))
                              (* 0.1 (- file-depth candidate-depth)))
         do (when (< depth-delta best-depth-delta)
              (progn
                (setq best-depth-delta depth-delta)
                (setq best-path path)))
         finally return best-path))))

(defun irony-cdb--locate-dominating-file-with-dirs (file
                                                    name
                                                    subdirectories)
  "Convenience wrapper around `locate-dominating-file'

Looks up the directory hierarchy from FILE for to locate any directory
in `subdirectories` which contains NAME. If multiple files are found,
chooses the one located at the nearest directory level. if multiple
files are found at the same level, picks the first one encountered.
returns the full path to file if found, or nil otherwise."
  (let ((candidates
         (cl-loop for subdir in subdirectories
            for relpath = (concat (file-name-as-directory subdir) name)
            for match-maybe = (locate-dominating-file file relpath)
            when match-maybe collect (expand-file-name (concat match-maybe relpath)))))
    (irony-cdb--choose-closest-path file candidates)))


(defun irony-cdb--update-compile-options (compile-options
                                          &optional working-directory)
  (setq irony--compile-options compile-options
        irony--working-directory working-directory))

(defun irony-cdb--autodetect-compile-options ()
  (catch 'found
    (dolist (compilation-database irony-cdb-compilation-databases)
      (with-demoted-errors "Irony CDB: error in compilation database: %S"
        (irony--awhen (funcall compilation-database 'get-compile-options)
          (throw 'found (list compilation-database
                              (caar it)
                              (cdar it))))))))

(defun irony-cdb--string-suffix-p (suffix string &optional ignore-case)
  "Return non-nil if SUFFIX is a suffix of STRING."
  (let ((start-pos (- (length string) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                string start-pos nil ignore-case)))))

(defun irony-cdb--remove-compiler-from-flags (flags)
  "Remove the compiler from FLAGS read from a compilation database.

When using ccache, the compiler might be present in FLAGS since
the compiler is `ccache compiler'."
  (let* ((first (car flags))
         (flags (cdr flags)))
    (if (irony-cdb--string-suffix-p "ccache" first) (cdr flags) flags)))

(provide 'irony-cdb)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-cdb.el ends here
