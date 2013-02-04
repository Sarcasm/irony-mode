;;; irony/compilation-db.el --- `irony-mode` CMake integration

;; Copyright (C) 2012-2013  Guillaume Papin

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

;; Usage:
;;     (irony-enable 'compilation-db)

;;; Code:

(require 'irony)

(eval-when-compile
  (require 'cl))

(defcustom irony-compilation-db-build-dir "emacs-build"
  "Default directory name to use for the cmake build directory."
  :type 'string
  :require 'irony
  :group 'irony)

(defcustom irony-compilation-db-cmake-generator nil
  "Generator to use when calling CMake, nil means use the default
  CMake generator. If non-nil the CMake option \"-G
  `irony-compilation-db-cmake-generator'\" will be the default for CMake
  generation.

Known working values are:
- Unix Makefiles
- Ninja"
  :type 'string
  :require 'irony
  :group 'irony)

(defcustom irony-compilation-db-known-binaries '("cc"       "c++"
                                                 "gcc"      "g++"
                                                 "clang"    "clang++"
                                                 "llvm-gcc" "llvm-g++")
  "List of valid compilers in a compilation_commands.json file.
They should be compatible with the libclang
clang_parseTranslationUnit() command line arguments."
  :type '(choice (repeat string)
                 (function :tag "Known binaries with clang compatible options: "))
  :require 'irony
  :group 'irony)


;; Internal variables
;;

(defvar irony-compilation-db-enabled nil
  "A boolean value to inform if yes or no the setup method had
  already been called. The hook seems to be called twice for no
  apparent reasons, this is a way to get around this strange
  behavior.")
(make-variable-buffer-local 'irony-compilation-db-enabled)

;; TODO: Rewrite (it seems to work but it's ugly...)
(defun irony-compilation-db-find-cmake-root-1 (dir)
  "Find the root CMakeLists.txt file starting from DIR and
looking back until the first CMakeLists.txt is found (assuming
each directory after the root CMakeLists.txt contain a
CMakeLists.txt).
Returns nil if no directory is found."
  (let ((old-dir ""))
    ;; Loop until a directory containing a CMakeLists.txt file is found
    (while (and (not (string= old-dir dir))
                (not (file-exists-p (expand-file-name "CMakeLists.txt" dir))))
      (setq old-dir dir
            dir (file-truename (expand-file-name ".." dir))))
    ;; Loop until we have a CMakeLists.txt file, return the last directory if any
    (when (not (string= old-dir dir))
      (setq old-dir dir
            dir (file-truename (expand-file-name ".." dir)))
      (while (file-exists-p (expand-file-name "CMakeLists.txt" dir))
        (setq old-dir dir
              dir (file-truename (expand-file-name ".." dir))))
      old-dir)))

(defvar irony-compilation-db-root-dir-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found CMake project roots.
- the key is a directory for which a root as already been found
- the value is the root")

(defvar irony-compilation-db-root-dir-list nil
  "*internal variable* Memoize already found CMake project
  roots.")

(defun irony-compilation-db-find-cmake-root ()
  "Look for the root directory of the project, the directory
where we can find the CMakeLists.txt. If a CMakeLists.txt can't
be found or if it's the first time (in this Emacs session) that
this root is referenced we ask the user to provide the
directory.

This might be a bad idea but the function returns either:
- a string:
      The path to the root directory of the project.
- nil:
      Couldn't find a root even if it seems to be a CMake
      project (maybe the user was just annoyed by the prompt).
- 'not-a-cmake-project:
      The project is apparently not a CMake project but we can
      look for some other compilation-db solution than CMake
      compile_commands.json
"
  ;; looking for the root of the CMake project
  (let ((current-dir (or (irony-current-directory)
                         default-directory)))
    ;; either we return the cached value or we calculate the good one
    (or (gethash current-dir irony-compilation-db-root-dir-hash)
        (let ((root (irony-compilation-db-find-cmake-root-1 current-dir)))
          ;; if the directory is known, return it, otherwise ask
          ;; confirmation
          (cond
           ((member root irony-compilation-db-root-dir-list)
            root)
           ;; let the caller know it's not a CMake project
           ((not root)
            'not-a-cmake-project)
           (t
            (let ((inhibit-quit t))
              (setq root
                    (with-local-quit
                      (read-directory-name "Confirm CMake directory: " root)
                      (if (file-exists-p (expand-file-name "CMakeLists.txt" root))
                          (progn
                            (setq irony-compilation-db-root-dir-list
                                  (cons root irony-compilation-db-root-dir-list))
                            (puthash current-dir root irony-compilation-db-root-dir-hash))
                        (message "No CMakeLists.txt found for this buffer."))))
              (setq quit-flag nil)
              root)))))))

(defvar irony-compilation-db-build-dir-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found CMake build dir
  containing a compile_commands.json file.
 - the key is the root directory containing the CMakeLists.txt
 - the value is the build dir")

(defun irony-compilation-db-generate-cmake (src-dir)
  "Prompt the user for arguments and then generate a cmake build.
Return t if the generation was successful (0 returned by the
command), return nil on error."
  ;; we hide the fact -DEXPORT_COMPILE_COMMANDS=ON will be added and
  ;; the source directory
  (let ((args (read-string "Additionnal CMake arguments: "
                           (if irony-compilation-db-cmake-generator
                               (format "-G \"%s\" "
                                       irony-compilation-db-cmake-generator)))))
    (eq 0 (shell-command
           (concat "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON "
                   args " "
                   src-dir)))))

(defun irony-compilation-db-find-build-dir (root)
  "Find or create the CMake build directory containing a
compile_command.json file."
  (or (gethash root irony-compilation-db-build-dir-hash)
      (let ((build-dir (read-directory-name
                        "CMake build directory: "
                        (expand-file-name irony-compilation-db-build-dir
                                          root))))
        (unless (string= build-dir "")
          ;; ensure final '/' for required by `default-directory'
          (setq build-dir (file-name-as-directory build-dir))
          (unless (file-exists-p (expand-file-name "compile_commands.json" build-dir))
            ;; create directory invoke CMake ask for additionnal arguments
            (when (ignore-errors (make-directory build-dir t) t)
              (let ((default-directory build-dir))
                (irony-compilation-db-generate-cmake root))))
          (if (file-exists-p (expand-file-name "compile_commands.json" build-dir))
              (puthash root build-dir irony-compilation-db-build-dir-hash))))))

(defvar irony-compilation-db-compilation-db-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found compilation databases.
- the key is a build directory for which a compilation DB as
  already been found
- the value is the compilation database")

;; FIXME: the directory argument is not handled correctly. For example
;; the output file (in this case it doesn't matter...) will be -o
;; src/foo.o, it should be /directory/given/in/the/entry/src/foo.o but
;; it should works for most cases...
(defun irony-compilation-db-parse-entry (entry)
  "Return a list as follows (file include-dirs extra-flags) or
Nil if the entry doesn't seems correct (for example an unknown
compilers/command).

For example:
    '(\"/tmp/foo.cpp\"
      (\"/usr/include/foo\" \"/usr/include/bar\")
      (\"-Wall\" \"-D_GNU_SOURCE\"))"
  (let ((file (cdr (assq 'file entry)))
        (dir (cdr (assq 'directory entry)))
        ;; FIXME: can't handle escaped quotes correctly, like in:
        ;;      /usr/bin/c++ -DLLVM_VERSION_INFO=\\\"3.2svn\\\" <args>
        (cmd (ignore-errors
               (split-string-and-unquote (cdr (assq 'command entry))))))
    (when (and cmd
               (member (file-name-nondirectory (car cmd))
                       irony-compilation-db-known-binaries))
      (cons (file-truename file)
            (loop for arg in (cdr cmd)
                  ;; XXX: this is bad, this is an assumption on the
                  ;; format of compile_commands.json that each
                  ;; compilation command will finish with:
                  ;;    -o <output-file> -c <source-file>
                  until (string= arg "-o")
                  if (string-prefix-p "-I" arg)
                  collect (expand-file-name (substring arg 2) dir)
                          into include-dirs
                  else
                    collect arg into extra-flags
                  finally return (list include-dirs extra-flags))))))

(defun irony-compilation-db-parse-file (compilation-db-file)
  "Parse the compilation database by calling
  `irony-compilation-db-parse-entry' on each entry and filling
  a hash with a filename as a key."
  (let ((entries (mapcar 'irony-compilation-db-parse-entry
                         (json-read-file compilation-db-file)))
        (compilation-db (make-hash-table :test 'equal)))
    (dolist (entry entries compilation-db)
      (when entry
        (puthash (car entry) (cdr entry) compilation-db)))))

(defun irony-compilation-db-get-db (build-dir)
  "Get or load the compilation database present in BUILD-DIR."
  (or (gethash build-dir irony-compilation-db-compilation-db-hash)
      (let ((compilation-db (irony-compilation-db-parse-file
                             ;; FIXME: redundancy with the calls in
                             ;; `irony-compilation-db-find-build-dir'
                             (expand-file-name "compile_commands.json" build-dir))))
        (puthash build-dir compilation-db irony-compilation-db-compilation-db-hash))))

(defun irony-compilation-db-setup ()
  "Irony-mode hook for irony-compilation-db plugin."
  ;; looking for the root of the CMake project
  (interactive)
  (when (and buffer-file-name
             (not irony-compilation-db-enabled))
    (setq irony-compilation-db-enabled t)
    (let ((root (irony-compilation-db-find-cmake-root)))
      (cond ((stringp root)
             (let* ((build-dir (irony-compilation-db-find-build-dir root))
                    (compilation-db (irony-compilation-db-get-db build-dir))
                    (flags (gethash (file-truename buffer-file-name) compilation-db)))
               (when flags
                 (setq irony-header-directories (car flags)
                       irony-extra-flags (cadr flags)))))
            ((eq root 'not-a-cmake-project)
             (message "find work around..."))
            ((not root)
             (message "leave the user alone..."))))))

(defun irony-compilation-db-enable ()
  "Enable cmake settings for `irony-mode'."
  (interactive)
  (add-hook 'irony-mode-hook 'irony-compilation-db-setup))

(defun irony-compilation-db-disable ()
  "Disable cmake settings for `irony-mode'."
  (interactive)
  (remove-hook 'irony-mode-hook 'irony-compilation-db-setup))

(provide 'irony/compilation-db)

;; Local variables:
;; generated-autoload-load-name: "irony/cmake"
;; End:

;;; irony/compilation-db.el ends here
