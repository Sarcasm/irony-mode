;;; irony.el --- C/C++ minor mode powered by libclang

;; Copyright (C) 2011-2016  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Version: 1.5.0
;; URL: https://github.com/Sarcasm/irony-mode
;; Compatibility: GNU Emacs 24.x
;; Keywords: c, convenience, tools
;; Package-Requires: ((cl-lib "0.5") (json "1.2"))

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
;; This file provides `irony-mode', a minor mode for C, C++ and Objective-C.
;;
;; Usage:
;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     (add-hook 'c-mode-hook 'irony-mode)
;;     (add-hook 'objc-mode-hook 'irony-mode)
;;
;;     ;; Windows performance tweaks
;;     ;;
;;     (when (boundp 'w32-pipe-read-delay)
;;       (setq w32-pipe-read-delay 0))
;;     ;; Set the buffer size to 64K on Windows (from the original 4K)
;;     (when (boundp 'w32-pipe-buffer-size)
;;       (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
;;
;; See also:
;; - https://github.com/Sarcasm/company-irony
;; - https://github.com/Sarcasm/flycheck-irony
;; - https://github.com/Sarcasm/ac-irony

;;; Code:

(require 'irony-iotask)

(autoload 'irony-completion--enter "irony-completion")
(autoload 'irony-completion--exit "irony-completion")

(require 'cl-lib)

(autoload 'find-library-name "find-func")
(autoload 'lm-version "lisp-mnt")


;;
;; Compatibility
;;

(eval-and-compile

  ;; As seen in flycheck/magit
  ;;
  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being
automatically buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))

  ) ;; eval-and-compile


;;
;; Customizable variables
;;

(defgroup irony nil
  "C/C++ minor mode powered by libclang."
  :group 'c)

(defcustom irony-lighter " Irony"
  "Text to display in the mode line when irony mode is on."
  :type 'string
  :group 'irony)

(defcustom irony-extra-cmake-args nil
  "Extra arguments to CMake when compiling the server."
  :type '(repeat string)
  :group 'irony)

(defcustom irony-user-dir (locate-user-emacs-file "irony/")
  "Directory containing the Irony generated files.

The slash is expected at the end."
  :type 'directory
  :risky t
  :group 'irony)

(defcustom irony-supported-major-modes '(c++-mode
                                         c-mode
                                         objc-mode)
  "List of modes known to be compatible with Irony."
  :type '(repeat symbol)
  :group 'irony)

;;;###autoload
(defcustom irony-additional-clang-options nil
  "Additional command line options to pass down to libclang.

Please, do NOT use this variable to add header search paths, only
additional warnings or compiler options.

These compiler options will be prepended to the command line, in
order to not override the value coming from a compilation
database."
  :type '(repeat string)
  :options '("-Wdocumentation")
  :group 'irony)

(defcustom irony-lang-compile-option-alist
  '((c++-mode  . "c++")
    (c-mode    . "c")
    (objc-mode . "objective-c"))
  "Alist to decide the language option to used based on the `major-mode'."
  :type '(alist :key-type symbol :value-type string)
  :group 'irony)

(defcustom irony-cmake-executable "cmake"
  "Name or path of the CMake executable."
  :type 'string
  :group 'irony)

(defcustom irony-server-source-dir nil
  "Points to the irony-server source directory.

This should point to the directory that contains the top-most
CMakeLists.txt used to build the server.

By default it will find the directory based on the irony.el directory."
  :type 'directory
  :group 'irony
  :package-version '(irony . "1.2.0"))

(defcustom irony-server-build-dir nil
  "Build directory for irony-server.

If set to nil the default is to create a build directory in
`temporary-file-directory'/build-irony-server-`(irony-version)'."
  :type 'directory
  :group 'irony)

(defcustom irony-server-install-prefix irony-user-dir
  "Installation prefix used to install irony-server.

The irony-server executable is expected to be in
`irony-server-install-prefix'/bin/."
  :type 'directory
  :group 'irony)

(defcustom irony-server-w32-pipe-buffer-size nil
  "Windows-only setting,
the buffer size to use for the irony-server process pipe on Windows.

Larger values can improve performances on large buffers.

If non-nil, `w32-pipe-buffer-size' will be let-bound to this value
during the creation of the irony-server process.")


;;
;; Public/API variables
;;
;; Non-customizable variables provided by Irony that can be useful to other
;; packages.
;;
;; Note that they shouldn't be modified directly by external packages, just
;; read.
;;

;; TODO: make this variable public when the CDB API stabilizes.
(defvar-local irony--compile-options nil
  "Compile options for the current file.

The compile options used by the compiler to build the current
buffer file.")

;; TODO: make this variable public when the CDB API stabilizes.
(defvar-local irony--working-directory nil
  "The working directory to pass to libclang, if any.")


;;
;; Internal variables
;;
;; The prefix `irony--' is used when something can completely change (or
;; disappear) from one release to the other.
;;
;; -- https://lists.gnu.org/archive/html/emacs-devel/2013-06/msg01129.html

(defconst irony--eot "\n;;EOT\n"
  "String sent by the server to signal the end of a response.")


;;
;; Error conditions
;;

;; `define-error' breaks backward compatibility with Emacs < 24.4
(defun irony--define-error (name message &optional parent)
  "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
  (unless parent (setq parent 'error))
  (let ((conditions
         (if (consp parent)
             (apply #'nconc
                    (mapcar (lambda (parent)
                              (cons parent
                                    (or (get parent 'error-conditions)
                                        (error "Unknown signal `%s'" parent))))
                            parent))
           (cons parent (get parent 'error-conditions)))))
    (put name 'error-conditions
         (delete-dups (copy-sequence (cons name conditions))))
    (when message (put name 'error-message message))))

(irony--define-error 'irony-error "Irony-Mode error")
(irony--define-error 'irony-parse-error "Irony-Mode parsing error" 'irony-error)
(irony--define-error 'irony-server-error "Irony-Mode server error" 'irony-error)


;;
;; Utility functions & macros
;;

;; TODO: remove and use `if-let' when supported version jumps to Emacs 25.1
(defmacro irony--aif (test if-expr &rest else-body)
  (declare (indent 2))
  `(let ((it ,test))
     (if it
         ,if-expr
       (progn ,@else-body))))

;; TODO: remove and use `when-let' when supported version jumps to Emacs 25.1
(defmacro irony--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test))
     (when it
       (progn ,@body))))

(defun irony--assoc-all (key list)
  (delq nil (mapcar (lambda (c)
                      (when (equal (car c) key)
                        c))
                    list)))

(defmacro irony--without-narrowing (&rest body)
  "Remove the effect of narrowing for the current buffer.

Note: If `save-excursion' is needed for BODY, it should be used
before calling this macro."
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))

(defun irony--buffer-size-in-bytes ()
  "Return the buffer size, in bytes."
  (1- (position-bytes (point-max))))

(defun irony--read-char-choice (prompt chars)
  "Wrapper around `read-char-choice', available since Emacs 24."
  (setq prompt (concat prompt " [" chars "]: "))
  (if (fboundp 'read-char-choice)
      (read-char-choice prompt chars)
    (setq prompt (propertize prompt 'face 'minibuffer-prompt))
    (let ((cursor-in-echo-area t)
          k)
      (while (not (member k chars))
        (setq k (read-char-exclusive prompt)))
      k)))

(defun irony--shorten-path (path)
  "Make PATH as short as possible.

The given path can be considered understandable by human but not
necessary a valid path string to use in code. Its only purpose is
to be displayed to the user."
  (let ((relative (file-relative-name path))
        (abbreviated (abbreviate-file-name path)))
    (if (< (string-width relative) (string-width abbreviated))
        relative
      abbreviated)))

(defun irony--split-command-line-1 (quoted-str)
  "Remove the escaped quotes and backlash from a QUOTED-STR.

Return a list of the final characters in the reverse order.

Only to be consumed by `irony--split-command-line'."
  (let ((len (length quoted-str))
        (i 0)
        ch next-ch
        result)
    (while (< i len)
      (setq ch (aref quoted-str i))
      (when (eq ch ?\\)
        (let ((next-ch (and (< (1+ i) len)
                            (aref quoted-str (1+ i)))))
          (when (member next-ch '(?\\ ?\"))
            (setq ch next-ch)
            (cl-incf i))))
      (push ch result)
      (cl-incf i))
    result))

;; TODO: rewrite the function correctly to handle things like the following:
;;
;; "/usr/bin/clang++ -Irelative -DSOMEDEF=\"With spaces, quotes and \\-es.\" <args...>"
(defun irony--split-command-line (cmd-line)
  "Split CMD-LINE into a list of arguments.

Takes care of double quotes as well as backslash.

Sadly I had to write this because `split-string-and-unquote'
breaks with escaped quotes in compile_commands.json, such as in:

    /usr/bin/c++ -DLLVM_VERSION_INFO=\\\\\\\"3.2svn\\\\\\\" <args>"
  ;; everytime I write a function like this one, it makes me feel bad
  (let* ((len (length cmd-line))
         (spaces (string-to-list " \f\t\n\r\v"))
         (first-not-spaces-re (concat "[^" spaces "]"))
         (i 0)
         ch
         args cur-arg)
    (while (< i len)
      (setq ch (aref cmd-line i))
      (cond
       ((member ch spaces)              ;spaces
        (when cur-arg
          (setq args (cons (apply 'string (nreverse cur-arg)) args)
                cur-arg nil))
        ;; move to the next char
        (setq i (or (string-match-p first-not-spaces-re cmd-line i)
                    len)))
       ((eq ch ?\")                     ;quoted string
        (let ((endq (string-match-p "[^\\]\"" cmd-line i)))
          (unless endq
            (signal 'irony-parse-error (list "ill formed command line" cmd-line)))
          (let ((quoted-str (substring cmd-line (1+ i) (1+ endq))))
            (setq cur-arg (append (irony--split-command-line-1 quoted-str)
                                  cur-arg)
                  i (+ endq 2)))))
       (t                             ;a valid char
        ;; if it's an escape of: a backslash, a quote or a space push
        ;; only the following char.
        (when (eq ch ?\\)
          (let ((next-ch (and (< (1+ i) len)
                              (aref cmd-line (1+ i)))))
            (when (or (member next-ch '(?\\ ?\"))
                      (member next-ch spaces))
              (setq ch next-ch)
              (cl-incf i))))
        (push ch cur-arg)
        (cl-incf i))))
    (when cur-arg
      (setq args (cons (apply 'string (nreverse cur-arg)) args)))
    (nreverse args)))

(defun irony--get-buffer-path-for-server (&optional buffer)
  "Get the path of the current buffer to send to irony-server.

If no such file exists on the filesystem the special file '-' is
  returned instead."
  (let ((file (buffer-file-name buffer)))
    (if (and file (file-exists-p file))
        file
      "-")))


;;
;; Mode
;;

(defvar irony-mode-map (make-sparse-keymap)
  "Keymap used in `irony-mode' buffers.")

;;;###autoload
(define-minor-mode irony-mode
  "Minor mode for C, C++ and Objective-C, powered by libclang."
  nil
  irony-lighter
  irony-mode-map
  :group 'irony
  (if irony-mode
      (irony--mode-enter)
    (irony--mode-exit)))

(defun irony--mode-enter ()
  ;; warn the user about modes such as php-mode who inherits c-mode
  (when (not (memq major-mode irony-supported-major-modes))
    (display-warning 'irony "Major mode is unknown to Irony,\
 see `irony-supported-major-modes'."))
  ;; warn the user about Windows-specific issues
  (when (eq system-type 'windows-nt)
    (cond
     ((version< emacs-version "24.4")
      (display-warning 'irony "Emacs >= 24.4 expected on Windows."))
     ((and (boundp 'w32-pipe-read-delay) (> w32-pipe-read-delay 0))
      (display-warning 'irony "Performance will be bad because a\
 pipe delay is set for this platform (see variable\
 `w32-pipe-read-delay')."))))
  (irony-completion--enter))

(defun irony--mode-exit ()
  (irony-completion--exit))

;;;###autoload
(defun irony-version (&optional show-version)
  "Return the version number of the file irony.el.

If called interactively display the version in the echo area."
  (interactive (list t))
  ;; Shamelessly stolen from `company-mode'.
  (with-temp-buffer
    (insert-file-contents (find-library-name "irony"))
    (let ((v (lm-version)))
      (when show-version
        (message "irony version: %s" v))
      v)))


;;
;; Compile options handling
;;

(defun irony--lang-compile-option ()
  (irony--awhen (cdr-safe (assq major-mode irony-lang-compile-option-alist))
    (list "-x" it)))

(defun irony--extract-working-directory-option (flags)
  "Return working directory specified on the command line, if
any."
  (catch 'found
    (while flags
      (let ((flag (car flags)))
        (cond
         ((string= "-working-directory" flag)
          (throw 'found (cadr flags)))
         ((string-prefix-p "-working-directory=" flag)
          (throw 'found (substring flag (length "-working-directory="))))
         (t
          (setq flags (cdr flags))))))))

(defun irony--adjust-compile-options ()
  "The compile options to send to libclang."
  ;; TODO: if current buffer has no associated file (will be sent as '-') but is
  ;; in an existing directory, we will want to add -I (directory-file-name
  ;; buffer-file-name) to find the relative headers
  (append
   (irony--lang-compile-option)
   (irony--awhen irony--working-directory
     (unless (irony--extract-working-directory-option irony--compile-options)
       (list "-working-directory" it)))
   irony-additional-clang-options
   irony--compile-options))

(defun irony--extract-user-search-paths (compile-options work-dir)
  "Retrieve the user search paths present in COMPILE-OPTIONS.

Relative paths are expanded to be relative to WORK-DIR.

The returned paths are returned as
directory (`file-name-as-directory').

Note: WORK-DIR is not used when the compile option
'-working-directory=<directory>' is detected in COMPILE-OPTIONS."
  (setq work-dir (or (irony--extract-working-directory-option compile-options)
                     work-dir))
  (let (include-dirs opt)
    (while (setq opt (car compile-options))
      (cond
       ((string= "-I" opt)
        (add-to-list 'include-dirs (nth 1 compile-options) t)
        (setq compile-options (cddr compile-options)))
       ((string-prefix-p "-I" opt)
        (add-to-list 'include-dirs (substring opt 2) t)
        (setq compile-options (cdr compile-options)))
       (t
        (setq compile-options (cdr compile-options)))))
    (delete-dups (mapcar #'(lambda (path)
                             (file-name-as-directory
                              (expand-file-name path work-dir)))
                         include-dirs))))


;;
;; Irony-Server setup
;;

(defvar irony--server-install-command-history nil)
(defun irony--install-server-read-command (command)
  (read-shell-command
   "Install command: " command
   (if (equal (car irony--server-install-command-history) command)
       '(irony--server-install-command-history . 1)
     'irony--server-install-command-history)))

(defun irony-install-server (command)
  "Install or reinstall the Irony server.

The installation requires CMake and the libclang developpement package."
  (interactive
   (list (let ((command
                (format
                 (concat "%s %s %s %s && %s --build . "
                         "--use-stderr --config Release --target install")
                 (shell-quote-argument irony-cmake-executable)
                 (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                               (expand-file-name
                                                irony-server-install-prefix)))
                 (mapconcat 'shell-quote-argument irony-extra-cmake-args " ")
                 (shell-quote-argument
                  (or irony-server-source-dir
                      (expand-file-name "server"
                                        (file-name-directory
                                         (find-library-name "irony")))))
                 (shell-quote-argument irony-cmake-executable))))
           (irony--install-server-read-command command))))
  (let ((build-dir (or irony-server-build-dir
                       (concat
                        (file-name-as-directory temporary-file-directory)
                        (file-name-as-directory (format "build-irony-server-%s"
                                                        (irony-version)))))))
    (make-directory build-dir t)
    (let ((default-directory build-dir))
      ;; we need to kill the process to be able to install a new one,
      ;; at least on Windows
      (irony-server-kill)
      (with-current-buffer (compilation-start command nil
                                              #'(lambda (maj-mode)
                                                  "*irony-server build*"))
        (setq-local compilation-finish-functions
                    '(irony--server-install-finish-function))))))

(defun irony--server-install-finish-function (buffer msg)
  (if (string= "finished\n" msg)
      (message "irony-server installed successfully!")
    (message "Failed to build irony-server, you are on your own buddy!")))

(defun irony--find-server-executable ()
  "Return the path to the irony-server executable.

Throw an `irony-server-error' if a proper executable cannot be
found."
  (let* ((exec-path (cons (expand-file-name "bin" irony-server-install-prefix)
                          exec-path))
         (exe (executable-find "irony-server")))
    (condition-case err
        (let ((version (car (process-lines exe "--version"))))
          (if (and (string-match "^irony-server version " version)
                   (version= (irony-version)
                             (substring version
                                        (length "irony-server version "))))
              ;; irony-server is working and up-to-date!
              exe
            (signal 'irony-server-error
                    (list
                     (format "irony-server version mismatch: %s"
                             (substitute-command-keys
                              "type `\\[irony-install-server]' to reinstall"))))))
      (irony-server-error
       (signal (car err) (cdr err)))
      (error
       (signal 'irony-server-error
               (if (and exe
                        (file-executable-p exe))
                   ;; failed to execute due to a runtime problem, i.e:
                   ;; libclang.so isn't in the ld paths
                   (list (format "irony-server is broken! %s"
                                 (error-message-string err)))
                 ;; irony-server doesn't exists, first time irony-mode is used?
                 ;; inform the user about how to build the executable
                 (list
                  (format "irony-server not installed! %s"
                          (substitute-command-keys
                           "Type `\\[irony-install-server]' to install")))))))))


;;
;; irony-server process management.
;;

(defvar irony--server-executable nil)
(defvar irony--server-process nil)
(defvar irony--server-buffer " *Irony*"
  "The name of the buffer for the irony process to run in.

When using a leading space, the buffer is hidden from the buffer
list (and undo information is not kept).")

(defun irony--start-server-process ()
  (unless irony--server-executable
    ;; if not found, an `irony-server-error' error is signaled
    (setq irony--server-executable (irony--find-server-executable)))
  (let ((process-connection-type nil)
        (process-adaptive-read-buffering nil)
        (w32-pipe-buffer-size (when (boundp 'w32-pipe-buffer-size)
                                (or irony-server-w32-pipe-buffer-size
                                    w32-pipe-buffer-size)))
        process)
    (setq process
          (start-process-shell-command
           "Irony"                    ;process name
           irony--server-buffer       ;buffer
           (format "%s -i 2> %s"      ;command
                   (shell-quote-argument irony--server-executable)
                   (expand-file-name
                    (format-time-string "irony.%Y-%m-%d_%Hh-%Mm-%Ss.log")
                    temporary-file-directory))))
    (set-process-query-on-exit-flag process nil)
    (irony-iotask-setup-process process)
    process))

;;;###autoload
(defun irony-server-kill ()
  "Kill the running irony-server process, if any."
  (interactive)
  (when (process-live-p irony--server-process)
    (kill-process irony--server-process)
    (setq irony--server-process nil)))

(defun irony--get-server-process-create ()
  (unless (process-live-p irony--server-process)
    (setq irony--server-process (irony--start-server-process)))
  irony--server-process)

(defun irony--run-task (task)
  (irony-iotask-run (irony--get-server-process-create) task))

(defun irony--run-task-asynchronously (task callback)
  (irony-iotask-schedule (irony--get-server-process-create) task callback))

(defun irony--quote-strings (strings &optional separator)
  "Like `combine-and-quote-strings', but when the string is \"\" or nil,
`irony--quote-strings' will convert it to \"\" instead of <SPC>.
That is:

  (irony--quote-strings \'(\"a\" \"\" \"b\"))            => \"a \\\"\\\" b\"
  (combine-and-quote-strings \'(\"a\" \"\" \"b\"))       => \"a  b\"
"
  (let* ((sep (or separator " "))
         (re (concat "[\\\"]" "\\|" (regexp-quote sep))))
    (mapconcat
     (lambda (str)
       (cond
        ((or (not str) (string= str ""))
         "\"\"")
        ((string-match re str)
         (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" str) "\""))
        (t str)))
     strings sep)))

(defun irony--server-send-command (command &rest args)
  (let ((command-line (concat (irony--quote-strings
                               (mapcar (lambda (arg)
                                         (if (numberp arg)
                                             (number-to-string arg)
                                           arg))
                                       (cons command args)))
                              "\n")))
    (irony-iotask-send-string command-line)))

;; XXX: this code can run in very tight very sensitive on big inputs,
;; every change should be measured
(defun irony--server-command-update (&rest _args)
  (when (and (>= (buffer-size) (length irony--eot))
             (string-equal (buffer-substring-no-properties
                            (- (point-max) (length irony--eot)) (point-max))
                           irony--eot))
    (condition-case-unless-debug nil
        (let ((result (read (current-buffer))))
          (cl-case (car result)
            (success
             (irony-iotask-set-result (cdr result)))
            (error
             (apply #'irony-iotask-set-error 'irony-server-error
                    (cdr result)))))
      (error
       (throw 'invalid-msg t)))))

;; FIXME: code duplication with `irony--server-command-update'
;; XXX: this code can run in very tight very sensitive on big inputs,
;; every change should be measured
(defun irony--server-query-update (&rest _args)
  (when (and (>= (buffer-size) (length irony--eot))
             (string-equal (buffer-substring-no-properties
                            (- (point-max) (length irony--eot)) (point-max))
                           irony--eot))
    (condition-case-unless-debug nil
        (irony-iotask-set-result (read (current-buffer)))
      (error
       (throw 'invalid-msg t)))))


;;
;; Server commands
;;

(irony-iotask-define-task irony--t-get-compile-options
  "`get-compile-options' server command."
  :start (lambda (build-dir file)
           (irony--server-send-command "get-compile-options" build-dir file))
  :update irony--server-command-update)

(defun irony--get-compile-options-task (build-dir file)
  (irony-iotask-package-task irony--t-get-compile-options build-dir file))

(cl-defstruct (irony--buffer-state
               (:constructor irony--buffer-state-create-1))
  file
  exists
  modified
  tick)

(defun irony--buffer-state-create (buffer)
  (let ((file (buffer-file-name buffer)))
    (irony--buffer-state-create-1 :file file
                                  :exists (and file (file-exists-p file))
                                  :modified (buffer-modified-p buffer)
                                  :tick (buffer-chars-modified-tick buffer))))

(defun irony--buffer-state-compare (old new)
  (unless (equal old new)
    (cond
     ((irony--buffer-state-modified new) 'set-unsaved)
     ((null old) nil)                   ;noop
     ((and
       (irony--buffer-state-modified old)
       (irony--buffer-state-exists old)) 'reset-unsaved))))

(irony-iotask-define-task irony--t-set-unsaved
  "`set-unsaved' server command."
  :start (lambda (process buffer buf-state)
           (let ((elem (assq buffer (process-get process :unsaved-buffers)))
                 temp-file)
             (if (eq (cdr elem) buf-state)
                 ;; early exit if already cached
                 (irony-iotask-set-result t)
               (setq temp-file (make-temp-file "irony-unsaved-"))
               (irony-iotask-put :temp-file temp-file)
               (irony-iotask-put :buffer-state buf-state)
               (process-put process :buffer-state buf-state)
               (with-current-buffer buffer
                 (irony--without-narrowing
                   (let ((write-region-inhibit-fsync t))
                     (write-region nil nil temp-file nil 0)))
                 (irony--server-send-command "set-unsaved"
                                             (irony--get-buffer-path-for-server)
                                             temp-file)))))
  :update irony--server-command-update
  :finish (lambda (&rest _args)
            (delete-file (irony-iotask-get :temp-file)))
  :on-success
  (lambda (process buffer &rest _args)
    (let* ((unsaved-buffers (process-get process :unsaved-buffers))
           (elem (assq buffer unsaved-buffers))
           (buf-state (irony-iotask-get :buffer-state)))
      (if elem
          (setcdr elem buf-state)
        (process-put process :unsaved-buffers (cons (cons buffer buf-state)
                                                    unsaved-buffers))))))

(defun irony--set-unsaved-task (process buffer buf-state)
  (irony-iotask-package-task irony--t-set-unsaved process buffer buf-state))

(irony-iotask-define-task irony--t-reset-unsaved
  "`reset-unsaved' server command."
  :start (lambda (process buffer)
           (if (assq buffer (process-get process :unsaved-buffers))
               (irony--server-send-command "reset-unsaved"
                                           (irony--get-buffer-path-for-server
                                            buffer))
             ;; exit early if already reset
             (irony-iotask-set-result t)))
  :update irony--server-command-update
  :finish (lambda (process buffer)
            (process-put
             process
             :unsaved-buffers
             (assq-delete-all buffer (process-get process :unsaved-buffers)))))

(defun irony--reset-unsaved-task (process buffer)
  (irony-iotask-package-task irony--t-reset-unsaved process buffer))

(defun irony--list-unsaved-irony-mode-buffers (&optional ignore-list)
  (delq nil (mapcar (lambda (buf)
                      (unless (memq buf ignore-list)
                        (when (buffer-modified-p buf)
                          (with-current-buffer buf
                            (and irony-mode buf)))))
                    (buffer-list))))

(defun irony--get-buffer-change-alist (process)
  "Return a list of (buffer . old-state).

old-state can be nil if the old state isn't known."
  (let ((unsaved-list (process-get process :unsaved-buffers)))
    (append unsaved-list
            (mapcar (lambda (buf)
                      (cons buf nil))
                    (irony--list-unsaved-irony-mode-buffers
                     (mapcar #'car unsaved-list))))))

(defun irony--unsaved-buffers-tasks ()
  (let ((process (irony--get-server-process-create))
        result)
    (dolist (buffer-old-state-cons (irony--get-buffer-change-alist process)
                                   result)
      (let* ((buffer (car buffer-old-state-cons))
             (old-state (cdr buffer-old-state-cons))
             (new-state (irony--buffer-state-create buffer))
             (task
              (cl-case (irony--buffer-state-compare old-state new-state)
                (set-unsaved
                 (irony--set-unsaved-task process buffer new-state))
                (reset-unsaved
                 (irony--reset-unsaved-task process buffer)))))
        (when task
          (setq result (if result
                           (irony-iotask-chain result task)
                         task)))))))

(irony-iotask-define-task irony--t-parse
  "`parse' server command."
  :start (lambda (file compile-options)
           (apply #'irony--server-send-command "parse" file "--"
                  compile-options))
  :update irony--server-command-update)

(defun irony--parse-task-1 (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (irony-iotask-package-task irony--t-parse
                               (irony--get-buffer-path-for-server)
                               (irony--adjust-compile-options))))

(defun irony--parse-task (&optional buffer)
  (let ((unsaved-tasks (irony--unsaved-buffers-tasks))
        (parse-task (irony--parse-task-1 buffer)))
    (if unsaved-tasks
        (irony-iotask-chain unsaved-tasks parse-task)
      parse-task)))

(irony-iotask-define-task irony--t-diagnostics
  "`parse' server command."
  :start (lambda ()
           (irony--server-send-command "diagnostics"))
  :update irony--server-query-update)

(defun irony--diagnostics-task (&optional buffer)
  (irony-iotask-chain
   (irony--parse-task buffer)
   (irony-iotask-package-task irony--t-diagnostics)))

(irony-iotask-define-task irony--t-get-type
  "`get-type' server command."
  :start (lambda (line col)
           (irony--server-send-command "get-type" line col))
  :update irony--server-query-update)

(defun irony--get-type-task (&optional buffer pos)
  (let ((line-column (irony--completion-line-column pos)))
    (irony-iotask-chain
     (irony--parse-task buffer)
     (irony-iotask-package-task irony--t-get-type
                                (car line-column) (cdr line-column)))))

;;;###autoload
(defun irony-get-type ()
  "Get the type of symbol under cursor."
  (interactive)
  (let ((types (irony--run-task (irony--get-type-task))))
    (unless types
      (user-error "Type not found"))
    (if (and (cdr types) (not (string= (car types) (cadr types))))
        (message "%s (aka '%s')" (car types) (cadr types))
      (message "%s" (car types)))))

(defun irony-parse-buffer-async (&optional callback)
  "Parse the current buffer sending results to an optional
  CALLBACK function."
  (irony--run-task-asynchronously (irony--parse-task)
                                  (or callback #'ignore)))

(provide 'irony)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony.el ends here
