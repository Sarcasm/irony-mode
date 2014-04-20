;;; irony.el --- C/C++ minor mode powered by libclang

;; Copyright (C) 2011-2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Version: 0.1.0
;; URL: https://github.com/Sarcasm/irony-mode
;; Compatibility: GNU Emacs 23.x, GNU Emacs 24.x
;; Keywords: c, convenience, tools
;; Package-Requires: ((cl-lib "0.5"))

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

;;; Code:

(require 'cl-lib)

(autoload 'find-library-name "find-func" nil t)
(autoload 'lm-version "lisp-mnt" nil t)


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

(defcustom irony-user-dir (locate-user-emacs-file "irony/")
  "Directory containing the irony-mode generated files.

The slash is expected at the end."
  :type 'directory
  :risky t
  :group 'irony)

(defcustom irony-known-modes '(c++-mode c-mode objc-mode)
  "List of modes known to be compatible with `irony-mode'."
  :type '(repeat symbol)
  :group 'irony)

(defcustom irony-lang-option-alist '((c++-mode . "c++")
                                     (c-mode   . "c")
                                     (objc-mode . "objective-c"))
  "Association list of (major-mode . \"<compiler language option>\").

The compiler language options matches the ones used by Clang with
the -x <language> command line switch."
  :type '(alist :key-type symbol :value-type string)
  :group 'irony)

(defcustom irony-cmake-executable "cmake"
  "A name that can be found in `exec-path' or the full-path to
  the cmake executable."
  :type 'string
  :group 'irony)

(defcustom irony-server-source-dir
  (expand-file-name "server" (file-name-directory (find-library-name "irony")))
  "Points to the irony-server source directory.

This should point to the directory that contains the top-most
CMakeLists.txt used to build the server."
  :type 'directory
  :group 'irony)

(defcustom irony-server-build-dir (concat irony-user-dir "build/")
  "Build directory for irony-server."
  :type 'directory
  :group 'irony)

(defcustom irony-server-install-prefix irony-user-dir
  "Installation prefix used to install irony-server.

The irony-server executable is expected to be in
`irony-server-install-prefix'/bin/."
  :type 'directory
  :group 'irony)

(defcustom irony-check-compile-functions
  '(irony--process-initial-check-compile)
  "Abnormal hook run when new check-compile results are available.

Takes as an argument the number of fatal errors (usually 0 or 1),
the number of errors and the number of warnings.

Example function:

  (defun my-irony-check-compile-function (nfatals nerrors nwarnings)
    (message \"%d error(s) and %d warning(s)\" (+ nfatals nerrors)\
 nwarnings))

  (add-hook 'irony-check-compile-functions\
 'my-irony-check-compile-function)"
  :type 'hook
  :options '(irony--process-initial-check-compile)
  :group 'irony)


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
;; Internal variables
;;
;; Use the prefix `irony--' when something can completely change (or disappear)
;; from one release to the other.
;;
;; -- https://lists.gnu.org/archive/html/emacs-devel/2013-06/msg01129.html

(defconst irony--eot "\n;;EOT\n"
  "The string sent by the server to finish the transmission of a
  message.")

(defconst irony-server-eot "\nEOT\n"
  "The string to send to the server to finish a transmission.")

(defvar irony-install-server-initiating-buffer)

(defvar-local irony-server-executable-path nil
  "Path to the irony-server executable to use for the current buffer.

Set to nil if not found.")

(defvar-local irony--initial-compile-check-status nil
  "Non-nil when an initial compile check as already been requested.

Possible values are:
- nil
- 'requested, when the compile check for the current buffer has
  been requested.
- 'done, when the compile check has been received and processed")


;;
;; Utility functions & macros
;;

(defmacro irony-without-narrowing (&rest body)
  "Remove the effect of narrowing for the current buffer.

Note: If `save-excursion' is needed for body, it should be used
before calling this macro."
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))

(defun irony-buffer-size-in-bytes ()
  "Returns the buffer size, in bytes."
  (1- (position-bytes (point-max))))


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
      (irony-mode-enter)
    (irony-mode-exit)))

(defun irony-mode-enter ()
  ;; warn the user about suspicious modes such as php-mode that inherits c-mode
  (when (not (memq major-mode irony-known-modes))
    (display-warning 'irony "Irony mode is aimed to work with a\
 major mode present in `irony-known-modes'."))
  ;;c-mode-hook and c++-mode-hook appears to run twice, avoid unecessary call
  ;;to `irony-locate-server-executable' if it has already be done once.
  (unless irony-server-executable-path
    (irony-locate-server-executable))
  ;; TODO: init compilation database here
  (when irony-server-executable-path
    (irony--initial-check-compile)))

(defun irony-mode-exit ())

(defun irony-version (&optional show-version)
  "Returns the version number of the file irony.el.

If called interactively display the version in the echo area."
  (interactive "P")
  ;; Shamelessly stolen from `company-mode'.
  (with-temp-buffer
    (insert-file-contents (find-library-name "irony"))
    (let ((v (lm-version)))
      (when show-version
        (message "irony version: %s" v))
      v)))

(defun irony-install-server-finish-function (buffer msg)
  (if (string= "finished\n" msg)
      (progn
        (message "irony-server installed successfully!")
        (with-current-buffer buffer
          (if (buffer-live-p irony-install-server-initiating-buffer)
              (with-current-buffer irony-install-server-initiating-buffer
                (if irony-mode
                    (irony-locate-server-executable))))))
    ;; failed to build/install
    ;; TODO: detect common issues:
    ;; - cmake not installed, or installed in a specific place
    ;;   could be detected before compilation and the user can be prompted to
    ;;   customize `irony-cmake-executable'
    ;; - libclang not found
    ;; - ...
    (message "Failed to build irony-server, you are on your own buddy!")))

(defun irony-install-server ()
  (interactive)
  (let ((cur-buf (current-buffer))
        (default-directory irony-server-build-dir)
        (cmd (format
              (concat "%s -DCMAKE_INSTALL_PREFIX=%s %s && %s --build . "
                      "--use-stderr --config Release --target install")
              (shell-quote-argument irony-cmake-executable)
              (shell-quote-argument (expand-file-name
                                     irony-server-install-prefix))
              (shell-quote-argument irony-server-source-dir)
              (shell-quote-argument irony-cmake-executable))))
    (make-directory irony-server-build-dir t)
    (with-current-buffer (compilation-start cmd nil #'(lambda (maj-mode)
                                                        "*irony-server build*"))
      (setq-local irony-install-server-initiating-buffer cur-buf)
      (setq-local compilation-finish-functions
                  '(irony-install-server-finish-function)))))

(defun irony-locate-server-executable ()
  "Check if an irony-server exists for the current buffer."
  (let ((exe (expand-file-name "bin/irony-server" irony-server-install-prefix)))
    (condition-case err
        (let ((irony-server-version (car (process-lines exe "--version"))))
          (if (and (string-match "^irony-server version " irony-server-version)
                   (version= (irony-version)
                             (substring irony-server-version
                                        (length "irony-server version "))))
              ;; irony-server is working and up-to-date!
              (setq irony-server-executable-path exe)
            (message "irony versions mismatch, forgot to call\
 'M-x irony-install-server'?") ))
      (error
       (if (file-executable-p exe)
           ;; failed to execute due to a runtime problem, i.e: libclang.so isn't
           ;; in the ld paths
           (message "error: irony-server is broken, good luck buddy! %s"
                    (error-message-string err))
         ;; irony-server doesn't exists, first time irony-mode is used? inform
         ;; the user about how to build the executable
         (message "Please install irony-server: M-x irony-install-server"))))))

(defun irony--initial-check-compile ()
  "Check that the current buffer compiles, if not a hint will be
 displayed to the user.

Ideally this is done only once, when the buffer is first
opened (or irony-mode first started), just to inform the user if
he forgot to provide the flags for the current buffer."
  (unless irony--initial-compile-check-status
    (setq irony--initial-compile-check-status 'requested)
    (irony-request-check-compile)))

(defun irony--process-initial-check-compile (nfatals nerrors nwarnings)
  "See `irony-check-compile-functions'."
  (when (and (eq irony--initial-compile-check-status 'requested)
             (not (zerop (+ nfatals nerrors nwarnings))))
    (setq irony--initial-compile-check-status 'done)
    (let ((help-msg (substitute-command-keys
                     "Type `\\[irony-cdb-menu]' to configure project"))
          stats-strings)
      (unless (zerop nwarnings)
        (push (concat (number-to-string nwarnings)
                      " warning"
                      (unless (eq nwarnings 1) ;plural
                        "s"))
              stats-strings))
      (setq nerrors (+ nfatals nerrors))
      (unless (zerop nerrors)
        (push (concat (number-to-string nerrors)
                      " error"
                      (unless (eq nerrors 1) ;plural
                        "s"))
              stats-strings))
      (message "[%s] %s" (mapconcat 'identity stats-strings " | ")
               help-msg))))


;;
;; irony-server process management.
;;

(defun irony--server-command ()
  "Shell command used to start the irony-server process."
  (format "%s -i 2>> %s/irony.$$.log"
          (shell-quote-argument irony-server-executable-path)
          temporary-file-directory))

(defvar irony--server-process nil)

(defun irony--get-server-process ()
  (if (and irony--server-process
           (process-live-p irony--server-process))
      irony--server-process
    (let ((process-connection-type nil)
          process)
      (setq process (start-process-shell-command
                     "Irony"                   ;process name
                     "*Irony*"                 ;buffer
                     (irony--server-command))) ;command
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel process 'irony--server-process-sentinel)
      (set-process-filter process 'irony--server-process-filter)
      (setq irony--server-process process))))

(defun irony--server-process-sentinel (process event)
  (unless (process-live-p process)
    (setq irony--server-process nil)
    (message "irony process stopped!")))

(defun irony--process-server-response (process response)
  (let ((sexp (read response))
        (callback (irony--server-process-pop-callback process)))
    (apply (car callback) sexp (cdr callback))))

(defun irony--server-process-filter (process output)
  "Handle output that come from an irony-server process."
  (let ((pbuf (process-buffer process))
        responses)
    ;; append output to process buffer
    (when (buffer-live-p pbuf)
      (with-current-buffer pbuf
        (save-excursion
          (goto-char (process-mark process))
          (insert output)
          (set-marker (process-mark process) (point))
          ;; check if the message is complete based on `irony--eot'
          (goto-char (point-min))
          (while (search-forward irony--eot nil t)
            (let* ((response (buffer-substring-no-properties (point-min) (point)))
                   (reason (unsafep response)))
              (delete-region (point-min) (point))
              (if (not reason)
                  (setq responses (cons response responses))
                (error "Irony: got unsafe data, skip request: '%s'" reason))
              (goto-char (process-mark process)))))))
    ;; Handle all responses.
    (mapc #'(lambda (r)
              (irony--process-server-response process r))
          (nreverse responses))))

(defun irony--server-process-push-callback (p cb)
  (let ((callbacks (process-get p 'irony-callback-stack)))
    (if callbacks
        (nconc callbacks (list cb))
      (process-put p 'irony-callback-stack (list cb)))))

(defun irony--server-process-pop-callback (p)
  (let ((callbacks (process-get p 'irony-callback-stack)))
    (process-put p 'irony-callback-stack (cdr callbacks))
    (car callbacks)))

(defun irony--server-process-callback-count (p)
  (length (process-get p 'irony-callback-stack)))


;;
;; Server commands
;;

(defun irony--get-buffer-path-for-server ()
  "Get the path of the current buffer to send to irony-server.

If no such file exists on the filesystem the special file '-' is
  returned instead."
  (if (and buffer-file-name (file-exists-p buffer-file-name))
      buffer-file-name
    "-"))

(defun irony--send-file-request (request callback &rest args)
  "Send a request that act on a file (the current buffer) to irony-server.

This concerns mainly irony-server commands that do some work on a
translation unit for libclang, the unsaved buffer data are taken
care of."
  (let ((process (irony--get-server-process))
        (argv (append (list request
                            "--num-unsaved=1"
                            (irony--get-buffer-path-for-server))
                      args)))
    (irony--server-process-push-callback process callback)
    ;; skip narrowing to compute buffer size and content
    (irony-without-narrowing
      (process-send-string process
                           (format "%s\n%s\n%d\n"
                                   (mapconcat 'shell-quote-argument argv " ")
                                   (shell-quote-argument buffer-file-name)
                                   (irony-buffer-size-in-bytes)))
      (process-send-region process (point-min) (point-max))
      ;; always make sure to finis with a newline (required by irony-server to
      ;; play nice with line buffering even when the file doesn't end with a
      ;; newline)
      (process-send-string process "\n"))))

(defun irony-request-check-compile ()
  (irony--send-file-request "check-compile"
                            (list 'irony-check-compile-handler)))

(defun irony-check-compile-handler (errors)
  (run-hook-with-args 'irony-check-compile-functions
                      (or (plist-get errors :fatals) 0)
                      (or (plist-get errors :errors) 0)
                      (or (plist-get errors :warnings) 0)))

(provide 'irony)
;;; irony.el ends here
