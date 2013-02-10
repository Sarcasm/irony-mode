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

(defcustom irony-compilation-db-build-dir-names
  '("build"         "Build"
    "Release"       "release"
    "debug"         "Debug"
    "Build/Release" "build/release"
    "Build/Debug"   "build/debug")
  "List of commons names for a build directory.

Subdirectory can be part of the pattern. For example
\"Build/Release\" is considered to be valid.

Directory at the beginning of the list have higher precedence
than those at the end.

Note: it is not necessary to add `irony-compilation-db-build-dir'
to this list."
  :type '(choice (repeat string))
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
  "List of valid compilers in a compile_commands.json file.
They should be compatible with the libclang
clang_parseTranslationUnit() command line arguments."
  :type '(choice (repeat string)
                 (function :tag "Known binaries with clang compatible options: "))
  :require 'irony
  :group 'irony)


;; Internal variables
;;

(defvar irony-compilation-db-enabled nil
  "*internal variable* A boolean value to inform if yes or no the
  setup method had already been called. The hook seems to be
  called twice for no apparent reasons, this is a way to get
  around this strange behavior.")
(make-variable-buffer-local 'irony-compilation-db-enabled)

;; TODO: `expand-file-name' says it's a bad idea to use it to traverse
;; the filesystem, `file-truename' is used but maybe the proposed
;; method is better.
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

(defvar irony-compilation-db-cmake-root-dir-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found CMake project roots.
- the key is a directory for which a root as already been found
- the value is the root")

(defvar irony-compilation-db-root-cmake-dir-list nil
  "*internal variable* Memoize already found CMake project
  roots.")

;; FIXME: Some projects (only libRocket in mind) provides a
;; CMakeLists.txt in a subfolder of the root directory and not in the
;; directory itself. Allow the user to say manually he wants a CMake
;; project with a user-provided root.
(defun irony-compilation-db-find-cmake-root ()
  "Look for the root directory of the CMake project.
Make the user confirm it's the right directory or to provide
another if we find a CMakeLists.txt at some point (otherwise
let's assume we are not in a CMake project).

Return the root or nil if not found."
  ;; looking for the root of the CMake project
  (let ((current-dir (or (irony-current-directory)
                         default-directory)))
    ;; either we return the cached value or we calculate the good one
    (or (gethash current-dir irony-compilation-db-cmake-root-dir-hash)
        (let ((root (irony-compilation-db-find-cmake-root-1 current-dir)))
          ;; if the directory is known, return it, otherwise ask
          ;; confirmation
          (cond
           ((member root irony-compilation-db-root-cmake-dir-list)
            root)
           ;; let the caller know it's not a CMake project
           (root
            (let ((inhibit-quit t))
              (setq root
                    (with-local-quit
                      (read-directory-name "Confirm CMake directory: " root)
                      (if (file-exists-p (expand-file-name "CMakeLists.txt" root))
                          (progn
                            (setq irony-compilation-db-root-cmake-dir-list
                                  (cons root irony-compilation-db-root-cmake-dir-list))
                            (puthash current-dir root irony-compilation-db-cmake-root-dir-hash))
                        (message "No CMakeLists.txt found for this buffer."))))
              (setq quit-flag nil)
              root)))))))

(defvar irony-compilation-db-cmake-build-dir-hash (make-hash-table :test 'equal)
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

(defun irony-compilation-db-find-cmake-build-dir ()
  "Find the build directory for a CMake project.

If the build directory doesn't exists the user the user get a
  prompt for the CMake command to enter and generate a
  compile_commands.json."
  (let ((cmake-root (irony-compilation-db-find-cmake-root)))
    (when cmake-root
      (or (gethash cmake-root irony-compilation-db-cmake-build-dir-hash)
          (let ((build-dir (read-directory-name
                            "CMake build directory: "
                            (expand-file-name irony-compilation-db-build-dir
                                              cmake-root))))
            (unless (string= build-dir "")
              ;; ensure final '/' for required by `default-directory'
              (setq build-dir (file-name-as-directory build-dir))
              (unless (file-exists-p (expand-file-name "compile_commands.json" build-dir))
                ;; create directory invoke CMake ask for additionnal arguments
                (when (ignore-errors (make-directory build-dir t) t)
                  (let ((default-directory build-dir))
                    (irony-compilation-db-generate-cmake cmake-root))))
              (if (file-exists-p (expand-file-name "compile_commands.json" build-dir))
                  ;; puthash returns the value (the build dir)
                  (puthash cmake-root build-dir
                           irony-compilation-db-cmake-build-dir-hash))))))))

(defun irony-compilation-db-find-build-dir ()
  "Find the build directory.

- Return the cached value if already found once

- If we detect the use of CMake for the project we will ask
  nicely if they want us to generate the
  compile_commands.json (if it doesn't already exist) and return
  this directory.

- `irony-compilation-db-build-dir' and
  `irony-compilation-db-build-dir-names' are looked for a
  compile_commands.json, if we find it it's returned.

- finally we ask politely the user
"
  (or (loop
       for x in (cons irony-compilation-db-build-dir
                      irony-compilation-db-build-dir-names)
       with cur-dir = (or (irony-current-directory) default-directory)
       for build-dir = (file-name-as-directory x)
       for subpath = (concat build-dir "compile_commands.json")
       for found = (irony-find-traverse-for-subpath subpath cur-dir)
       if found
         return (concat found build-dir))
      (irony-compilation-db-find-cmake-build-dir)
      (read-directory-name "compile_commands.json directory: ")))

(defvar irony-compilation-db-compilation-db-hash (make-hash-table :test 'equal)
  "*internal variable* Memoize already found compilation databases.
- the key is a build directory for which a compilation DB as
  already been found
- the value is the compilation database")

(defun irony-compilation-db-gen-clang-args-1 (cmd-args)
  "Split CMD-ARGS in 2 separate lists.

- the first list contains the include directories
- the second one the extra flags such as: -DFLAG, -Wall, ...

It also discards some irrelevant arguments, the source files. If
we have '(\"-Wall\"\ \"a.c\" \"b.c\") only \"-Wall\" will
subsist."
  (let (arg include-dirs extra-flags)
    (while (and cmd-args (not (string= "--" (car cmd-args))))
      (setq arg (car cmd-args))
      (cond
       ((string= "-I" arg)
        (push (nth 1 cmd-args) include-dirs)
        (setq cmd-args (cdr cmd-args))) ;skip next arg
       ((string-prefix-p "-I" arg)
        (push (substring arg 2) include-dirs))
       ((or (string-prefix-p "-" arg)
            (not (member (file-name-extension arg) ;skip source files
                         irony-known-source-extensions)))
        (push arg extra-flags)))
      (setq cmd-args (cdr cmd-args)))
    (list include-dirs (nreverse extra-flags))))

;; TODO: DOC
(defun irony-compilation-db-gen-clang-args (cmd-args working-dir)
  "Return a plist as follows (:include-dirs '(\"inc\") :extra-flags).

WORKING-DIR is the directory were the commands are started, it
means relative path are relative to this directory.

CMD-ARGS is a list of compiler arguments as a list, such as:

    '(\"-I/usr/include/foo\"
      \"-Wall\"
      \"-D_GNU_SOURCE\"
      \"file_a.cpp\"
      \"file_b.cpp\")

Expected result example:
    '((\"/usr/include/foo\" \"/usr/include/bar\")
      (\"-Wall\"
       \"-D_GNU_SOURCE\"
       \"-working-directory\" \"WORKING-DIR\"))"
  (let* ((x (irony-compilation-db-gen-clang-args-1 cmd-args))
         (extra-flags (nth 1 x))
         (work-dir (irony-compilation-db-extract-working-dir extra-flags)))
    (when work-dir
      (setq extra-flags (append '("-working-directory" working-dir)
                                extra-flags)))
    (list :work-dir (or work-dir working-dir)
          :include-dirs (car x)
          :extra-flags extra-flags)))

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
      (plist-put
       (irony-compilation-db-gen-clang-args (cdr cmd) dir)
       :file (file-truename file)))))

(defun irony-compilation-db-parse-file (compilation-db-file)
  "Parse the compilation database by calling
  `irony-compilation-db-parse-entry' on each entry and filling
  a hash with a filename as a key.

Note: will fail if the file doesn't exists."
  (let ((entries (mapcar 'irony-compilation-db-parse-entry
                         (json-read-file compilation-db-file)))
        (compilation-db (make-hash-table :test 'equal)))
    (dolist (entry entries compilation-db)
      (when entry
        (puthash (car entry) (cdr entry) compilation-db)))))

(defun irony-compilation-db-get-db (build-dir)
  "Get or load the compilation database present in BUILD-DIR."
  (message "build-dir: %s" build-dir)
  (or (gethash build-dir irony-compilation-db-compilation-db-hash)
      (let ((compilation-db (ignore-errors
                              (irony-compilation-db-parse-file
                               (expand-file-name "compile_commands.json"
                                                 build-dir)))))
        (if compilation-db
            (puthash build-dir compilation-db
                     irony-compilation-db-compilation-db-hash)
          (error "Failed to parse compilation DB")))))

(defun irony-compilation-db-setup ()
  "Irony-mode hook for irony-compilation-db plugin."
  (interactive)
  (when (and buffer-file-name
             (not irony-compilation-db-enabled))
    (setq irony-compilation-db-enabled t)
    (let ((build-dir (irony-compilation-db-find-build-dir))
          compilation-db
          flags)
      (when build-dir
        (when (setq compilation-db
                    (irony-compilation-db-get-db build-dir))
          (when (setq flags (gethash (file-truename buffer-file-name)
                                     compilation-db))
            (setq irony-header-directories (car flags)
                  irony-extra-flags (cadr flags))))))))

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
