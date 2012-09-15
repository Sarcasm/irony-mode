;;; irony-cmake.el --- `irony-mode` CMake integration

;; Copyright (C) 2012  Guillaume Papin

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
;; TODO: ...

;;; Code:

(require 'irony)

(eval-when-compile
  (require 'cl))

(defcustom irony-cmake-build-directory-name "emacs-build"
  "Default directory name to use for the cmake build directory."
  :type 'string
  :require 'irony
  :group 'irony)

(defcustom irony-cmake-generator nil
  "Generator to use when calling CMake, nil means use the default
  CMake generator. If non-nil the CMake option \"-G
  `irony-cmake-generator'\" will be the default for CMake
  generation.

Known working values are:
- Unix Makefiles
- Ninja"
  :type 'string
  :require 'irony
  :group 'irony)

(defcustom irony-cmake-known-binaries '("cc" "c++"
                                        "gcc" "g++"
                                        "clang" "clang++"
                                        "llvm-gcc" "llvm-g++")
  "List of valid compilers in a compilation_commands.json file.
They should be compatible with the libclang
clang_parseTranslationUnit() command line arguments."
  :type '(choice (repeat string)
                 (function :tag "Known binaries with clang compatible options: "))
  :require 'irony
  :group 'irony)

;;
;; Internal variables
;;

(defvar irony-cmake-enabled nil
  "A boolean value to inform if yes or not the setup method had
  already been called. The hook seems to be called twice for no
  apparent reason, this is a way to get around this strange
  behavior.")
(make-variable-buffer-local 'irony-cmake-enabled)

;; TODO: re-write (It seems to work but it's ugly...)
(defun irony-cmake-find-root-1 (dir)
  "Find the root CMakeLists.txt file starting from DIR and
looking back until the first CMakeLists.txt is found (assuming
each directory after the root CMakeLists.txt contain a
CMakeLists.txt)."
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

(defvar irony-cmake-root-dir-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found CMake project roots.
- the key is a directory for which a root as already been found
- the value is the root")

(defvar irony-cmake-root-dir-list nil
  "*internal variable* Memoize already found CMake project
  roots.")

(defun irony-cmake-find-root ()
  "Look for the root directory of the project, the directory
where we can find the CMakeLists.txt. If a CMakeLists.txt can't
be found or if it's the first time (in this Emacs session) that
this root is referenced we ask the user to provide the
directory."
  ;; looking for the root of the CMake project
  (let ((current-dir (or (if buffer-file-name
                             (file-name-directory buffer-file-name))
                         default-directory)))
    ;; either we return the cached value or we calculate the good one
    (or (gethash current-dir irony-cmake-root-dir-hash)
        (let ((root (irony-cmake-find-root-1 current-dir))
              (need-prompt t))
          ;; if the directory is know return it otherwise ask confirmation
          (if (member root irony-cmake-root-dir-list)
              root
            (setq root (read-directory-name "Confirm CMake root directory: " root))
            (if (file-exists-p (expand-file-name "CMakeLists.txt" root))
                (progn
                  (setq irony-cmake-root-dir-list (cons root irony-cmake-root-dir-list))
                  (puthash current-dir root irony-cmake-root-dir-hash))
              (message "No CMakeLists.txt found for this buffer.")
              nil))))))

(defvar irony-cmake-build-dir-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found CMake build dir
  containing a compile_commands.json file.
 - the key is the root directory containing the CMakeLists.txt
 - the value is the build dir")

(defun irony-cmake-generate-cmake (src-dir)
  "Prompt the user for arguments and then generate a cmake build.
Return t if the generation was successful (0 returned by the
command), return nil on error."
  ;; we hide the fact -DEXPORT_COMPILE_COMMANDS=ON will be added and
  ;; the source directory
  (let ((args (read-string "Additionnal CMake arguments: "
                           (if irony-cmake-generator
                               (concat "-G \"" irony-cmake-generator "\" ")))))
    (eq 0
        (shell-command
         (concat "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON " src-dir " " args)))))

(defun irony-cmake-find-build-dir (root)
  "Find or create the CMake build directory which should contain
a compile_command.json file."
  (or (gethash root irony-cmake-build-dir-hash)
      (let ((build-dir (read-directory-name
                        "CMake build directory: "
                        (expand-file-name irony-cmake-build-directory-name
                                          root))))
        (unless (string= build-dir "")
          ;; ensure final '/' for required by `default-directory'
          (setq build-dir (file-name-as-directory build-dir))
          (unless (file-exists-p (expand-file-name "compile_commands.json" build-dir))
            ;; create directory invoke CMake ask for additionnal arguments
            (when (ignore-errors (make-directory build-dir t) t)
              (let ((default-directory build-dir))
                (irony-cmake-generate-cmake root))))
          (if (file-exists-p (expand-file-name "compile_commands.json" build-dir))
              (puthash root build-dir irony-cmake-build-dir-hash))))))

(defvar irony-cmake-compilation-db-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found compilation databases.
- the key is a build directory for which a compilation DB as
  already been found
- the value is the compilation database")

;; FIXME: the directory argument is not handle correctly, for example
;; the output file (in this case it doesn't matter...) will be -o
;; src/foo.o, it should be /directory/given/in/the/entry/src/foo.o
;; but it should works for most of the case like this...
(defun irony-cmake-parse-compilation-entry (entry)
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
                       irony-cmake-known-binaries))
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

(defun irony-cmake-parse-compilation-db (compilation-db-file)
  "Parse the compilation database by calling
  `irony-cmake-parse-compilation-entry' on each entry and filling
  a hash with a filename as a key."
  (let ((entries (mapcar 'irony-cmake-parse-compilation-entry
                         (json-read-file compilation-db-file)))
        (compilation-db (make-hash-table :test 'equal)))
    (dolist (entry entries compilation-db)
      (when entry
        (puthash (car entry) (cdr entry) compilation-db)))))

(defun irony-cmake-get-compilation-db (build-dir)
  "Get or load the compilation database present in BUILD-DIR."
  (or (gethash build-dir irony-cmake-compilation-db-hash)
      (let ((compilation-db (irony-cmake-parse-compilation-db
                             ;; FIXME: redundancy with the calls in
                             ;; `irony-cmake-find-build-dir'
                             (expand-file-name "compile_commands.json" build-dir))))
        (puthash build-dir compilation-db irony-cmake-compilation-db-hash))))

(defun irony-cmake-setup ()
  "Irony-mode hook for irony-cmake plugin."
  ;; looking for the root of the CMake project
  (when (and buffer-file-name
             (not irony-cmake-enabled))
    (setq irony-cmake-enabled t)
    (let ((root (irony-cmake-find-root)))
      (when root
        (let* ((build-dir (irony-cmake-find-build-dir root))
               (compilation-db (irony-cmake-get-compilation-db build-dir))
               (flags (gethash (file-truename buffer-file-name) compilation-db)))
          (when flags
            (setq irony-header-directories (car flags)
                  irony-extra-flags (cadr flags))))))))

(defun irony-cmake-enable ()
  "Enable cmake settings for `irony-mode'."
  (add-hook 'irony-mode-hook 'irony-cmake-setup))

(defun irony-cmake-disable ()
  "Disable cmake settings for `irony-mode'."
  (remove-hook 'irony-mode-hook 'irony-cmake-setup))


(provide 'irony-cmake)
;;; irony-cmake.el ends here
