;;; irony-diagnostics.el --- irony-mode diagnostic reporting

;; Copyright (C) 2014  Guillaume Papin

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

;; Interface libclang's "Diagnostic Reporting", see
;; http://clang.llvm.org/doxygen/group__CINDEX__DIAG.html

;;; Code:

(require 'irony)


;;
;; Internal variables
;;

(defvar-local irony-diagnostics--diagnostics nil)
(defvar-local irony-diagnostics--request-context nil)
(defvar-local irony-diagnostics--available-context nil)
(defvar-local irony-diagnostics--callbacks nil)


;;
;; Functions
;;

(defun irony-diagnostics--context ()
  (buffer-chars-modified-tick))


;;
;; Interface with irony-server
;;

(defun irony-diagnostics--send-request ()
  (setq irony-diagnostics--request-context (irony-diagnostics--context))
  (irony--send-file-request "diagnostics"
                            (list 'irony-diagnostics--request-handler
                                  (irony-diagnostics--context))))

(defun irony-diagnostics--request-handler (diagnostics context)
  (when (equal (list irony-diagnostics--request-context
                     (irony-diagnostics--context))
               (make-list 2 context))
    (setq irony-diagnostics--diagnostics diagnostics
          irony-diagnostics--available-context context)
    (mapc 'funcall irony-diagnostics--callbacks)))

(defun irony-diagnostics--in-progress-p ()
  (unless (irony-diagnostics-available-p)
    (equal irony-diagnostics--request-context (irony-diagnostics--context))))


;;
;; Irony Diagnostics Interface
;;

(defun irony-diagnostics-file (diagnostic)
  (nth 0 diagnostic))

(defun irony-diagnostics-line (diagnostic)
  (nth 1 diagnostic))

(defun irony-diagnostics-column (diagnostic)
  (nth 2 diagnostic))

(defun irony-diagnostics-severity (diagnostic)
  (nth 4 diagnostic))

(defun irony-diagnostics-message (diagnostic)
  (nth 5 diagnostic))

(defun irony-diagnostics-available-p ()
  (equal irony-diagnostics--available-context (irony-diagnostics--context)))

(defun irony-diagnostics ()
  "Return the list of diagnostics for the current buffer, if available."
  (when (irony-diagnostics-available-p)
    irony-diagnostics--diagnostics))

(defun irony-diagnostics--async (callback)
  (if (irony-diagnostics-available-p)
      (funcall callback)
    (unless (irony-diagnostics--in-progress-p)
      (irony-diagnostics--send-request))
    (push callback irony-diagnostics--callbacks)))

(provide 'irony-diagnostics)

;;; irony-diagnostics.el ends here
