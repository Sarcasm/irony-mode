;;; irony.el --- C/C++ minor mode powered by libclang

;; Copyright (C) 2011-2014  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Version: 0.1.0
;; URL: https://github.com/Sarcasm/irony-mode
;; Compatibility: GNU Emacs 23.x, GNU Emacs 24.x
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
;; This file provide `irony-mode' a minor mode for C, C++ and Objective-C.

;;; Code:

(require 'cl-lib)


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

(defcustom irony-server-executable "irony-server"
  "The path where the 'irony-server' executable or simply the
executable name if it is in your PATH."
  :type 'file
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


;;
;; Internal variables
;;

(defvar irony-process nil)

(defconst irony-eot "\n;;EOT\n"
  "The string sent by the server to finish the transmission of a
  message.")

(defconst irony-server-eot "\nEOT\n"
  "The string to send to the server to finish a transmission.")


;;
;; Mode
;;

(defvar irony-mode-map (make-sparse-keymap)
  "Keymap used in `irony-mode' buffers.")

;;;###autoload
(define-minor-mode irony-mode
  "TODO"
  nil
  irony-lighter
  irony-mode-map
  :group 'irony

  (cond
   (irony-mode
    ;; warn the user about suspicious modes, e.g: php-mode that inherits c-mode
    (unless (memq major-mode irony-known-modes)
      (display-warning 'irony "Irony mode is aimed to work \
with a major mode present in `irony-known-modes'.")))
   (t
    nil;;TODO: remove hooks
    )))

(autoload 'find-library-name "find-func" nil t)
(autoload 'lm-version "lisp-mnt" nil t)
(defun irony-version (&optional show-version)
  "Returns the version number of the file irony.el.

If called interactively display the version in the echo area."
  (interactive "P")
  ;; Shamelessly stolen from `company-mode'.
  (with-temp-buffer
    (insert-file-contents (find-library-name "irony"))
    (let ((v (lm-version)))
      (when show-version
        (message "Irony version: %s" v))
      v)))

(provide 'irony)
;;; irony.el ends here
