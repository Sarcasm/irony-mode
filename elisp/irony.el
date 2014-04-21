;;; irony.el --- C based language parsing facilities with Clang (libclang).

;; Copyright (C) 2011-2013  Guillaume Papin

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

;; This file provide `irony-mode' a minor mode for C, C++ (eventually
;; Objective C and Objective C++).  This minor mode does nothing alone
;; in buffers where it's activated.
;;
;; TODO:
;; - explain `irony-mode' in details.
;; - checkdoc ?
;; - when an irony-mode buffer is exited, inform the irony process ?
;;   (change-major-mode-hook)
;; - ...
;;

;;; Usage:

;;; Code:

(require 'json)

(autoload 'irony-compilation-db-setup "irony-cdb")

(eval-when-compile
  (require 'cl))

;;
;; Customizable variables
;;

(defgroup irony nil
  "C based language comprehension, completion, syntax checking
and more."
  :version "23.3"
  :group 'c)

(defcustom irony-compiler-executable
  (or (executable-find "clang")
      (executable-find "gcc"))
  "Location of the compiler executable. Use Clang by default,
otherwise try GCC."
  :group 'irony
  :type '(file :must-match t))

(defcustom irony-compile-flags nil
  "List of compiler flags to compile the current buffer.

    e.g: '(\"-std=c++11\" \"-DNDEBUG\")"
  :type '(repeat string)
  :require 'irony
  :group 'irony)
(make-variable-buffer-local 'irony-compile-flags)

(defcustom irony-compile-flags-work-dir nil
  "If non-nil, contains default directory used to expand
  relatives paths in the compile command for the current buffer."
  :type '(string :tag "compile command default directory")
  :require 'irony
  :group 'irony)
(make-variable-buffer-local 'irony-compile-flags-work-dir)

(defcustom irony-known-modes '(c++-mode c-mode objc-mode)
  "List of modes where `irony-mode' can be turn on without
  warnings."
  :type '(repeat symbol)
  :require 'irony
  :group 'irony)

(defcustom irony-lang-option-alist '((c++-mode . "c++")
                                     (c-mode   . "c")
                                     (objc-mode . "objective-c"))
  "Association list of major-mode -> -x <lang_option> to pass to
  the compiler ."
  :type '(alist :key-type symbol :value-type string)
  :require 'irony
  :group 'irony)

(defcustom irony-libclang-additional-flags nil
  "Additionnal flags to send to libclang.")

(defcustom irony-known-source-extensions '("c"   "cc"
                                           "C"   "CC"
                                           "cpp" "cxx" "c++"
                                           "m"   "mm")
  "Known file extensions used for source code in C/C++/Obj-C.

Header files extensions shouldn't take part of this list."
  :group 'irony
  :type '(choice (repeat string)))

(defcustom irony-server-executable (or (executable-find "irony-server")
                                       (let ((path (concat (file-name-directory
                                                            (locate-library "irony"))
                                                           "../bin/irony-server")))
                                         (if (file-exists-p path)
                                             (expand-file-name path))))
  "The path where the \"irony-server\" executable can be found."
  :type 'file
  :require 'irony
  :group 'irony)

;;;###autoload
(defcustom irony-mode-line " Irony"
  "Text to display in the mode line (actually an irony mark) when
irony mode is on."
  :type 'string
  :require 'irony
  :group 'irony)

(defcustom irony-cancel-process-hook nil
  "List of functions to be called after the irony process is
cancelled."
  :group 'irony
  :type 'hook)

;;
;; Internal variables
;;

(defvar irony-process nil
  "The current irony-server process.")

;;;###autoload
(defvar irony-request-mapping '((:status-code   . irony-handle-status-code)
                                (:compile-check . irony-handle-compile-check))
  "Alist of known request types associated to their handler. New
  server plugins must add their handlers in this list.")

(defconst irony-eot "\n;;EOT\n"
  "The string sent by the server to finish the transmission of a
  message.")

(defconst irony-server-eot "\nEOT\n"
  "The string to send to the server to finish a transmission.")

(make-variable-buffer-local
 (defvar irony-num-requests 0
   "The number of current request to the irony process made. When
the value reach 0 it means the temporary file can be deleted."))

(make-variable-buffer-local
 (defvar irony-flags-cache nil
   "Calculating the flags for a buffer can be costly, so after the
  first time we use this variable as value for flags."))

(make-variable-buffer-local
 (defvar irony-mode-enabled nil
   "Prevent double loading of irony-mode ({c,c++}-hooks are run
   twice apparently)."))

;;;###autoload
(define-minor-mode irony-mode
  ;; FIXME: describe the mode here
  ;; Check if turning off the mode with -1 works.
  "Toggle irony mode.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode."
  nil
  irony-mode-line
  '(
    ;; ([(control return)] . irony-complete)
    )
  :group 'irony

  (cond (irony-mode                     ;start irony mode
         (unless irony-mode-enabled
           (setq irony-mode-enabled t)
           ;; if not in a known mode, warn the user
           (unless (memq major-mode irony-known-modes)
             (display-warning 'irony "Irony mode is aimed to work \
with a major mode present in `irony-known-modes'."))
           ;; FIXME: if the process is not found, turn off `irony-mode'.
           (irony-start-process-maybe)
           (irony-compilation-db-setup)
           (irony-compile-check)))
        (t
         (setq irony-mode-enabled nil))))

(defun irony-cancel-process ()
  "Stop the irony process. `irony-cancel-process-hook' are
called when the process is cancelled."
  (if (not irony-process)
      (message "No irony process running...")
    (delete-process irony-process)
    (setq irony-process nil)
    (run-hooks 'irony-cancel-process-hook)))

(defun irony-restart-process ()
  "Restart the irony process."
  (irony-cancel-process)
  (irony-start-process-maybe))

(defun irony-start-process-maybe ()
  "Launch the `irony-process' if it's not already started."
  (cond
   ;; Already stated, nothing need to be done
   ((processp irony-process))
   ;; Executable not found or invalid
   ((or (null irony-server-executable)
        (null (file-executable-p irony-server-executable))
        (file-directory-p irony-server-executable))
    (error "Can't start the process `%s'. Please check \
the value of the variable `irony-server-executable'."
           irony-server-executable))
   ;; Try to start the process, let `start-process-shell-command'
   ;; throw an error if something went wrong.
   (t
    ;; Without this the server doesn't work as expected, line
    ;; buffering and cie...
    ;;                  |
    ;;                  V
    ;;     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    (let ((process-connection-type  nil))
      (setq irony-process (start-process-shell-command
                           "Irony"           ;process name
                           "*Irony*"         ;buffer
                           (irony-command))) ;command
      (set-process-query-on-exit-flag irony-process nil)
      (set-process-sentinel irony-process 'irony-sentinel)
      (set-process-filter irony-process 'irony-handle-output)))))

(defun irony-command ()
  "Shell command used to start the irony-server process."
  (format "\"%s\" 2>> %s/irony.$$.log"
          irony-server-executable
          temporary-file-directory))

(defun irony-sentinel (process event)
  "Watch the activity of irony process."
  ;; FIXME: turn off `irony-mode' in all buffer ?
  (let ((status (process-status process)))
    (when (memq status '(exit signal closed failed))
      (message "irony process stopped..."))))

(defun irony-handle-response (response)
  "Handle a response from the `irony-handle-output'.

If a complete response is present in the irony process buffer the
variable `irony-request-mapping' is used in order to find
the action to do with the :type key in the request."
  (when (stringp response)
    (let* ((sexp (read response))
           (type (plist-get sexp :type))
           (buffer-file (plist-get sexp :buffer))
           (buffer (if buffer-file (get-file-buffer buffer-file)))
           (handler (cdr (assq type irony-request-mapping))))
      (if buffer
          (irony-pop-request buffer))
      (if (null handler)
          (error "Irony process received an unknown request. \
Request was \"%s\"." response)
        (cond
         ((functionp handler)
          (funcall handler sexp))
         (handler
          (warn "The value of %s is not set correctly, function \
expected got: %s." (symbol-name handler) handler)))))))

(defun irony-handle-output (process output)
  "Handle output that come from the `irony-process'.

Output has multiple responses.  Each responses are
handled by `irony-handle-response'."
  ;; If with OUTPUT we get a complete answers, RESPONSES will be
  ;; non-nil list.
  (let ((pbuf (process-buffer process))
        responses)
    ;; Add to process buffer
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (save-excursion
          (goto-char (process-mark process))
          (insert output)
          (set-marker (process-mark process) (point))
          ;; Check if the message is complete based on `irony-eot'
          (goto-char (point-min))
          (while (search-forward irony-eot nil t)
            (let ((response (buffer-substring (point-min) (point))))
              (delete-region (point-min) (point))
              (let ((reason (unsafep response)))
                (when reason
                  (setq response nil)
                  (error "Unsafe data received by the irony process\
 (request skipped): %s." reason)))
              (setq responses (cons response responses)))))
        (goto-char (process-mark process))))
    ;; Handle all responses.
    (mapc #'irony-handle-response
          (reverse responses))))

(defun irony-push-request (buffer)
  "Increment the request count `irony-num-requests' in the given
buffer and write the buffer content in a temporary file."
  (with-current-buffer buffer
    (setq irony-num-requests (1+ irony-num-requests))
    (write-region nil nil (irony-temp-filename) nil -1)))

(defun irony-pop-request (buffer)
  "Decrement the request count `irony-num-requests' in the given
buffer, if the value of zero is reached delete the temporary file
associated to the buffer if any."
  (with-current-buffer buffer
    (when (zerop (setq irony-num-requests (1- irony-num-requests)))
      (let ((temp-file (irony-temp-filename)))
        (if (file-exists-p temp-file)
            ;; (delete-file temp-file nil)))))) ;;New in Emacs 24
            (delete-file temp-file))))))

(defun irony-buffer-identifier (&optional buffer)
  "Get the expanded buffer filename if any. If there is no
filename associated to this buffer return the name of the
buffer."
  (let ((buf-fname (buffer-file-name buffer)))
    (or
     (when buf-fname
       (expand-file-name buf-fname))
     (buffer-name buffer)
     "killed-buffer")))

(defun irony-temp-filename (&optional buffer)
  "Return the temporary filename associated to BUFFER (the
current buffer by default).

The file is created under `temporary-file-directory' in order to
avoid polluting the working directory."
  (let ((file (irony-buffer-identifier buffer)))
    ;; Partially stolen from files.el `make-backup-file-name-1'
    (concat
     temporary-file-directory
     (subst-char-in-string ?/ ?! (replace-regexp-in-string "!" "!!" file)))))

(defun irony-send-request (type data buffer)
  "Send a request of type TYPE to the irony process.

The argument DATA is a plist of data that complete the
request, example:
        (:file \"/tmp/foo.cpp\" :line 42 :column 4)

If non-nil BUFFER will be added to the target buffer, that mean
when the answer will be received we will be able to retrieve the
buffer who sent the request."
  (let ((request (list :request type :data data)))
    (when buffer
      (irony-push-request buffer)
      (setq request (plist-put request :buffer (irony-buffer-identifier
                                                buffer))))
    (process-send-string irony-process (concat (json-encode request)
                                               irony-server-eot))))

(defun irony-wait-request-answer (sym)
  "Loop until the SYM value became non nil, SYM value should
change in the irony process filter, when a request is completed
the flag is set to a non nil value (probably the request datas).

Once the symbol value is non nil it's value is returned."
  ;; Wait for the completion to be completed or the death of the
  ;; process (condition partially stolen from "network-stream.el".
  (with-local-quit ;Otherwise we can enter in an infinite loop,
                   ;without the possibility to exit
    (while (and (null (symbol-value sym))
                (memq (process-status irony-process) '(open run)))
      (accept-process-output irony-process 0.05)))
  (symbol-value sym))

(defun irony-current-directory ()
  "Return the directory of the current buffer or nil if the
current directory couldn't be found."
  (if buffer-file-name
      (file-name-directory (expand-file-name buffer-file-name))))

(defun irony-get-libclang-flags ()
  "Find the compiler flags required to parse the content of the
  current buffer by libclang."
  (or irony-flags-cache
      (let ((lang-flag (irony-language-option-flag))
            (cur-dir (irony-current-directory))
            (work-dir-flag (irony-working-directory-flag)))
        (setq irony-flags-cache
              (append
               (if lang-flag
                   lang-flag)
               irony-libclang-additional-flags
               (if cur-dir
                   (list (concat "-I" cur-dir)))
               (if work-dir-flag
                   work-dir-flag)
               irony-compile-flags)))))

(defun irony-user-search-paths-from-flags (compile-flags &optional work-dir)
  "If WORK-DIR is given, relative path are expanded to be
relative to WORK-DIR.

Note: WORK-DIR will not be used if the flag
\"-working-directory=<directory>\" is set in compile flags."
  (setq work-dir (or (irony-extract-working-dir-flag compile-flags)
		     work-dir))
  (let (arg include-dirs)
    (while compile-flags
      (setq arg (car compile-flags))
      (cond
       ((string= "-I" arg)
        (add-to-list 'include-dirs (nth 1 compile-flags) t)
        (setq compile-flags (cdr compile-flags))) ;skip next arg
       ((string-prefix-p "-I" arg)
        (add-to-list 'include-dirs (substring arg 2) t)))
      (setq compile-flags (cdr compile-flags)))
    (let ((res (delete-dups (if work-dir
                                (mapcar (lambda (path)
                                          (expand-file-name path work-dir))
                                        include-dirs)
                              include-dirs))))
      res)))

(defun irony-user-search-paths ()
  "Returns a list of header search paths for the current buffer."
  (irony-user-search-paths-from-flags irony-compile-flags
                                      irony-compile-flags-work-dir))

(defun irony-language-option-flag ()
  "Find the language for filename based on the major mode. (the
-x option of the compiler)."
  (let ((lang (cdr-safe (assq major-mode irony-lang-option-alist))))
    (when lang
      (list "-x" lang))))

(defun irony-extract-working-dir-flag (flags)
  "Get the clang \"-working-directory=<value>\" option value if
  any."
  (let (work-dir arg)
    (while (and flags
                (not work-dir))
      (setq arg (car flags))
      (cond
       ((string= "-working-directory" arg)
        (setq work-dir (cadr flags)))
       ((string-prefix-p "-working-directory=" arg)
        (setq work-dir (substring arg (length "-working-directory="))))
       (t
        (setq flags (cdr flags)))))
    work-dir))

(defun irony-working-directory-flag ()
  "Find the -working-directory flag to set for the current buffer."
  (when (and irony-compile-flags-work-dir
             (not (irony-extract-working-dir-flag irony-compile-flags)))
    (list "-working-directory"
          irony-compile-flags-work-dir)))

(defmacro irony-without-narrowing (&rest body)
  "Remove the effect of narrowing for the current buffer.

Note: If `save-excursion' is needed for body, it should be used
before calling that macro."
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))

(defun irony-point-location (point)
  "Return a cons of the following form: (line . column)
corresponding to POS. The narrowing is skipped temporarily.

`position-bytes' is used to handle multibyte and
'multiple-column' (i.e tabulations) characters properly."
  (save-excursion
    (goto-char point)
    (let ((col (- (position-bytes (point)) (position-bytes (point-at-bol)))))
      (irony-without-narrowing
        (cons (line-number-at-pos) (1+ col))))))


;; Irony utility functions
;;

(defun irony-handle-status-code (data)
  "Generic handler for requests that do not require an answer
from the irony process, such as for the command that reload the
cached flags on a file."
  (unless (eq (plist-get data :value) :success)
    (message "error: irony-handle-status-code")))

(defun irony-handle-compile-check (data)
  (let ((stats (plist-get data :stats)))
    (unless (eq stats t)
      (let ((num-errors (+ (or (plist-get stats :fatal-errors) 0)
                           (or (plist-get stats :errors) 0)))
            (num-warnings (or (plist-get stats :warnings) 0))
            (help-msg (substitute-command-keys
                       "Type `\\[irony-cdb-menu]' to configure project"))
            stats-strings)
        (unless stats
          (setq num-errors 1))
        (when (> num-warnings 0)
          (push (concat (number-to-string num-warnings)
                        " warning"
                        (unless (eq num-warnings 1) ;; plural
                          "s"))
                stats-strings))
        (when (> num-errors 0)
          (push (concat (number-to-string num-errors)
                        " error"
                        (unless (eq num-errors 1) ;; plural
                          "s"))
                stats-strings))
        (message "[%s] %s" (mapconcat 'identity stats-strings " | ")
                 help-msg)))))

(defun irony-compile-check (&optional buffer)
  (interactive)
  (let ((request-data (list (cons :file (irony-temp-filename))
                            (cons :flags (irony-get-libclang-flags)))))
    (irony-send-request :compile-check request-data
                        (or buffer (current-buffer)))))

;; TODO:
;; Interactive with completion (see `completion-read')
(defun irony-enable (modules)
  "Load one or more modules for Irony. (this is simply a helper
function for modules that respect the following contract:

- provide irony-MODULE-NAME
- defun irony-MODULE-NAME-enable
- defun irony-MODULE-NAME-disable"
  (dolist (module (if (listp modules) modules (list modules)))
    (require (intern (concat "irony/" (symbol-name module))))
    (funcall (intern (concat "irony-" (symbol-name module) "-enable")))))

(defun irony-disable (modules)
  "Unload one or more modules for Irony. (this is simply a helper
function for modules that respect the following contract:

- provide irony-MODULE-NAME
- defun irony-MODULE-NAME-enable
- defun irony-MODULE-NAME-disable"
  (dolist (module (if (listp modules) modules (list modules)))
    (funcall (intern (concat "irony-" (symbol-name module) "-disable")))))

(defun irony-find-traverse-for-subpath (subpath dir)
  "Look starting at DIR for and traverse the filesystem until
  SUBPATH is found.

Return the directory where dir/subpath exists, the ending slash
will always be here. Returns nil if nothing if found.

Example:

- we are looking for: build/compile_commands.json
- we start at: /home/jimmy/project/blah/src/server/

The function will look respectively for:
- /home/jimmy/project/blah/src/server/build/compile_commands.json -- not found
- /home/jimmy/project/blah/src/build/compile_commands.json -- not found
- /home/jimmy/project/blah/build/compile_commands.json -- found"
  (unless (string= (setq dir (file-truename dir)) "/")
    (if (file-exists-p (expand-file-name subpath dir))
        (file-name-as-directory dir)
      (irony-find-traverse-for-subpath
       subpath
       (directory-file-name (file-name-directory dir))))))

(defun irony-split-command-line-1 (quoted-str)
  "Remove the escaped quotes and backlash from a string.

Return a list of the final characters in the reverse order, only
to be consumed by `irony-split-command-line'."
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
            (incf i))))
      (push ch result)
      (incf i))
    result))

(defun irony-split-command-line (cmd-line)
  "Split the command line into a list of arguments.

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
            (error "ill formed command line"))
          (let ((quoted-str (substring cmd-line (1+ i) (1+ endq))))
            (setq cur-arg (append (irony-split-command-line-1 quoted-str)
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
              (incf i))))
        (push ch cur-arg)
        (incf i))))
    (when cur-arg
      (setq args (cons (apply 'string (nreverse cur-arg)) args)))
    (nreverse args)))


;; "built-in" commands
;;

(defun irony-reload-flags ()
  "Invalidate the cached flags on the current buffer. To be used
   when the compiler flags has changed while working on a file."
  (interactive)
  ;; invalidate the flags on the Emacs side first
  (setq irony-flags-cache nil)
  ;; then on the irony-server process
  (let* ((request-data (list (cons :file (irony-temp-filename)))))
    (irony-send-request :reload-flags
                        request-data
                        (current-buffer))))

(provide 'irony)
;;; irony.el ends here
