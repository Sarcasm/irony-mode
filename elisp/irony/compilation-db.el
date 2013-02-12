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

(defcustom irony-cdb-cmake-executable (executable-find "cmake")
  "Location of the cmake executable."
  :group 'irony
  :type 'file)

(defcustom irony-cdb-build-dir "emacs-build"
  "Default directory name to use for the cmake build directory."
  :type 'string
  :require 'irony
  :group 'irony)

(defcustom irony-cdb-build-dir-names
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

Note: it is not necessary to add `irony-cdb-build-dir'
to this list."
  :type '(choice (repeat string))
  :require 'irony
  :group 'irony)

(defcustom irony-cdb-cmake-generator nil
  "Generator to use when calling CMake, nil means use the default
  CMake generator. If non-nil the CMake option \"-G
  `irony-cdb-cmake-generator'\" will be the default for CMake
  generation.

Known working values are:
- Unix Makefiles
- Ninja"
  :type 'string
  :require 'irony
  :group 'irony)

(defcustom irony-cdb-known-binaries '("cc"       "c++"
                                      "gcc"      "g++"
                                      "clang"    "clang++"
                                      "llvm-gcc" "llvm-g++")
  "List of valid compilers in a compile_commands.json file.
They should be compatible with the libclang
clang_parseTranslationUnit() command line arguments."
  :type '(choice
          (repeat string)
          (function :tag "Known binaries with clang compatible options: "))
  :require 'irony
  :group 'irony)


;; Internal variables
;;

(make-variable-buffer-local
 (defvar irony-cdb-enabled nil
   "*internal variable* A boolean value to inform if yes or no
  the setup method had already been called. The hook seems to be
  called twice for no apparent reasons, this is a way to get
  around this strange behavior."))

(defvar irony-cdb-compile-db-cache (make-hash-table :test 'equal)
  "*internal variable* Compilation commands cache.

The key is the true-name of the file (see `file-truename'.
The value is a cons of (compile-flags . working-dir) where both
can have a nil value.")


;; Functions
;;

;; TODO: take a look at `file-relative-name'
(defun irony-cdb-shorten-path (path)
  "Make the given path if possible while still describing the
same path.

The given path can be considered understandable by human but not
necessary a valid path string to use in code. It's only purpose
is to be displayed."
  (let ((user-home (getenv "HOME")))
    (cond
     ((string-prefix-p user-home path)
      (concat (if (string-match-p "/$" user-home)
                  "~/"
                "~")
              (substring path (length user-home))))
     (t
      path))))

(defun irony-cdb-get-menu-items ()
  "Generate the menu items for the current buffer.

To be used by `irony-cdb-menu'."
  (list
   ;; see `substitute-env' function
   (irony-cdb-cmake-item)
   (irony-cdb-compile-commands-item)
   ;; '(:keys ((?b message "Bear build...")) :desc "Bear build")
   ;; TODO: .clang_complete
   ;; '(:keys ((?u message "User provided build...")) :desc "User provided")
   '(:keys ((?p customize-apropos-options "irony-cdb")) :desc "Preferences")))

(defun irony-cdb-menu-make-str (item)
  (let ((keys (plist-get item :keys))
        (desc (plist-get item :desc)))
    (when (> (length keys) 3)
      (error "too many shortcut keys for one menu item"))
    (when (> (length desc) 70)
      (error "description too long for a menu item"))
    (mapc (lambda (key)
            (if (member (car key) '(?q ?Q))
                (error "menu items key is reserved")))
          keys)
    (format "%-7s %s"
            (format "[%s]"
                    (mapconcat 'identity
                               (mapcar '(lambda (k)
                                          (char-to-string (car k)))
                                       keys)
                               "/"))
            desc)))

(defun irony-cdb-menu-all-keys (items)
  "Return all keys and the associated action in a list."
  (loop for item in items
        append (plist-get item :keys) into keys
        finally return keys))

(defun irony-cdb-menu ()
  "Display a build configuration menu."
  (interactive)
  (let* ((items (irony-cdb-get-menu-items))
         (items-str (mapcar 'irony-cdb-menu-make-str items))
         (keys (irony-cdb-menu-all-keys items))
         cmd)
    (save-excursion
      (save-window-excursion
	(delete-other-windows)
	(with-output-to-temp-buffer "*Irony/Compilation DB*"
	  (mapc (lambda (str)
                  (princ (concat str "\n")))
                items-str)
          (princ "\n[q] to quit"))
        (fit-window-to-buffer (get-buffer-window "*Irony/Compilation DB*"))
        (let ((k (read-char-choice "Compilation DB: "
                                   (cons ?q (mapcar 'car keys)))))
          (message "")                ;clear `read-char-choice' prompt
          (unless (eq ?q k)
            (setq cmd (cdr (assoc k keys)))))))
    (when cmd
      (apply 'funcall cmd))))

(defun irony-cdb-load-flags (compile-flags &optional work-dir)
  "Set the compile flags for the current buffer and trigger a
reload of the cache with `irony-reload-flags'."
  (setq irony-compile-flags compile-flags)
  (setq irony-compile-flags-work-dir work-dir)
  (irony-reload-flags))

(defun irony-cdb-try-load-from-cache ()
  "Set the flags for the current buffer if a cache entry exists
for this path."
  (when buffer-file-name
    (let ((flags-work-dir (gethash (file-truename buffer-file-name)
                                   irony-cdb-compile-db-cache)))
      (when flags-work-dir
        (irony-cdb-load-flags (car flags-work-dir)
                              (cdr flags-work-dir))))))

(defun irony-compilation-db-setup ()
  "Irony-mode hook for irony-cdb plugin."
  (when (and buffer-file-name
             (not irony-cdb-enabled))
    (setq irony-cdb-enabled t)
    (define-key irony-mode-map (kbd "C-c C-b") 'irony-cdb-menu)
    (irony-cdb-try-load-from-cache)))

(defun irony-compilation-db-enable ()
  "Enable cmake settings for `irony-mode'."
  (interactive)
  (add-hook 'irony-mode-hook 'irony-compilation-db-setup))

(defun irony-compilation-db-disable ()
  "Disable cmake settings for `irony-mode'."
  (interactive)
  (remove-hook 'irony-mode-hook 'irony-compilation-db-setup))


;; compile-commands.json handling

(defun irony-cdb-gen-clang-args (cmd-args)
  "Find the compile flags discarding irrelevant arguments.

Discarded option are source files for example.

If we have '(\"-Wall\"\ \"a.c\" \"b.c\") only \"-Wall\" will
subsist."
  (let (arg compile-flags)
    (while (and cmd-args (not (string= "--" (car cmd-args))))
      (setq arg (car cmd-args))
      (if (or (string-prefix-p "-" arg)
              (not (member (file-name-extension arg) ;skip source files
                           irony-known-source-extensions)))
          (push arg compile-flags))
      (setq cmd-args (cdr cmd-args)))
    (nreverse compile-flags)))

(defun irony-cdb-parse-entry (entry)
  "Extract the flags of the entry and add them to cache."
  (let ((file (cdr (assq 'file entry)))
        (work-dir (cdr (assq 'directory entry)))
        (cmd (irony-split-command-line (cdr (assq 'command entry)))))
    (when (and cmd
               (member (file-name-nondirectory (car cmd))
                       irony-cdb-known-binaries))
      (let ((compile-flags (irony-cdb-gen-clang-args (cdr cmd))))
        (puthash (file-truename file)
                 (cons compile-flags
                       (unless (irony-extract-working-dir-flag compile-flags)
                         work-dir))
                 irony-cdb-compile-db-cache)))))

(defun irony-cdb-parse-compile-commands (cc-file)
  "Parse a compile_commands.json file and add its entries to the
cache `irony-cdb-compile-db-cache'.

Returns nil on failure (if the file doesn't exist for example),
otherwise return t."
  (condition-case err
      (progn
	(mapc 'irony-cdb-parse-entry (json-read-file cc-file))
	t)
    (file-error
     (message "%s" (error-message-string err))
     nil)))

(defun irony-cdb-find-compile-commands-dir ()
  "Find a directory containing a compile_commands.json."
  (let ((cur-dir (or (irony-current-directory) default-directory)))
    (or (irony-find-traverse-for-subpath "compile_commands.json" cur-dir)
        (loop for x in (cons irony-cdb-build-dir irony-cdb-build-dir-names)
              for build-dir = (file-name-as-directory x)
              for subpath = (concat build-dir "compile_commands.json")
              for found = (irony-find-traverse-for-subpath subpath cur-dir)
              if found
              return (concat found build-dir)))))

(defun irony-cdb-load-compile-commands (cc-file)
  "Load CC-FILE if not already loaded and use load call
`irony-cdb-try-load-from-cache' to load the flags in the current
buffer."
  (interactive
   (list (read-file-name "compile_commands.json path: "
                         (irony-cdb-find-compile-commands-dir)
                         nil
                         t
                         "compile_commands.json")))
  (when (irony-cdb-parse-compile-commands cc-file)
    (irony-cdb-try-load-from-cache)))

(defun irony-cdb-compile-commands-item ()
  (let ((cc-file (irony-cdb-find-compile-commands-dir))
        (limit 40)
        (keys (list '(?j call-interactively irony-cdb-load-compile-commands)))
        cc-file-str)
    (when cc-file
      (setq cc-file (concat cc-file "compile_commands.json")
            cc-file-str (irony-cdb-shorten-path cc-file))
      (when (> (length cc-file-str) limit)
        (setq cc-file-str (concat "..." (substring cc-file-str
                                                  (- (- limit 3))))))
      (add-to-list 'keys (list ?J 'irony-cdb-load-compile-commands cc-file) t))
    (list
     :keys keys
     :short-desc "Compile commands"
     :desc (concat "Compile commands" (if cc-file-str
                                          (format " [with path=%s]"
                                                  cc-file-str))))))


;; CMake
;;

;; TODO: `expand-file-name' says it's a bad idea to use it to traverse
;; the filesystem, `file-truename' is used but maybe the proposed
;; method is better.
(defun irony-cdb-find-cmake-root ()
  "Find the root CMakeLists.txt file starting from DIR and
looking back until the first CMakeLists.txt is found (assuming
each directory after the root CMakeLists.txt contain a
CMakeLists.txt).
Returns nil if no directory is found."
  (let ((dir (or (irony-current-directory) default-directory))
        (old-dir ""))
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

;; TODO: :check is-cmake-2.8-available? otherwise put the string with
;;       a face `shadow' and change help add a :disabled-help
(defun irony-cdb-cmake-item ()
  (let ((cmake-dir (irony-cdb-find-cmake-root))
        (limit 40)
        (keys (list '(?c call-interactively irony-cdb-cmake-build)))
        cmake-dir-str)
    (when cmake-dir
      (setq cmake-dir-str (irony-cdb-shorten-path cmake-dir))
      (when (> (length cmake-dir-str) limit)
        (setq cmake-dir-str (concat "..." (substring cmake-dir-str
                                                     (- (- limit 3))))))
      (add-to-list 'keys (list ?C 'irony-cdb-cmake-build cmake-dir) t))
    (list
     :keys keys
     :short-desc "cmake"
     :desc (concat "CMake build" (if cmake-dir-str
                                     (format " [with root=%s]"
                                             cmake-dir-str))))))

;; TODO: What if the project needs a more elaborate configuration?
;;       such as with cmake -i or ccmake or the CMake GUI?
(defun irony-cdb-generate-cmake (cmake-root build-dir)
  "Prompt the user for arguments and then generate a CMake
build."
  ;; ensure final '/' required by `default-directory'
  (setq build-dir (file-name-as-directory build-dir))
  ;; create directory invoke CMake ask for additionnal arguments
  (when (ignore-errors (make-directory build-dir t) t)
    (let* ((default-directory build-dir)
           ;; we hide the fact -DEXPORT_COMPILE_COMMANDS=ON will be
           ;; added and the source directory
           (args (read-string "Additionnal CMake arguments: "
                              (if irony-cdb-cmake-generator
                                  (format "-G \"%s\" "
                                          irony-cdb-cmake-generator))))
           (cmake-buffer (get-buffer-create "*CMake Command Output*"))
           (cmake-cmd (format "%s -DCMAKE_EXPORT_COMPILE_COMMANDS=ON %s %s"
                              irony-cdb-cmake-executable
                              args
                              (file-relative-name cmake-root))))
      (message "Running: %s" cmake-cmd)
      (shell-command cmake-cmd cmake-buffer)
      (message ""))))

(defun irony-cdb-cmake-build (root-dir &optional build-dir)
  "Configure a CMake build by loading (optionally generating) the
compile_commands.json file."
  (interactive
   (list (read-directory-name
          "CMake root directory: "
          (irony-cdb-find-cmake-root))))
  (unless (file-exists-p (expand-file-name "CMakeLists.txt" root-dir))
    (error "CMake root doesn't contain a CMakeLists.txt"))
  (unless build-dir
    (setq build-dir (read-directory-name
                     "CMake build directory: "
                     (expand-file-name irony-cdb-build-dir root-dir))))
  (let ((cc-file (expand-file-name "compile_commands.json" build-dir)))
    (unless (file-exists-p cc-file)
      (irony-cdb-generate-cmake root-dir build-dir))
    (irony-cdb-parse-compile-commands cc-file)
    (irony-cdb-try-load-from-cache)))

(provide 'irony/compilation-db)

;; Local variables:
;; generated-autoload-load-name: "irony/compilation-db"
;; End:

;;; irony/compilation-db.el ends here
