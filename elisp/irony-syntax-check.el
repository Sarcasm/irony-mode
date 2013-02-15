;;; irony-syntax-check.el --- irony-mode syntax checking definitions

;; Copyright (C) 2012  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: c, irony-mode

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

;; Handle the syntax checking feature. Handle the build of the request
;; and the reception of the response. It only provides the necessary
;; structure to build a 'plugin' around (for example something like
;; flymake).

;;; Code:

(require 'irony)

;;
;; Public, customizable variables
;;


;;
;; Privates variables
;;

(defvar irony-last-syntax-check nil
  "Contain the last syntax checking made by the Irony
  server (internal variable).")

;;
;; Register the syntax checking callback(s) in the
;; `irony-request-mapping' alist.
;;

;;;###autoload
(add-to-list 'irony-request-mapping '(:syntax-checking . irony-handle-syntax-check))

;;
;; Functions
;;

(defun irony-handle-syntax-check (data)
  "Function called when a syntax checking answer is received."
  (setq irony-last-syntax-check data))

(defun irony-syntax-check (&optional buffer)
  "Return a list of diagnostics found during the parsing of the
BUFFER translation unit."
  (let ((request-data (list (cons :file (irony-temp-filename))
                            (cons :flags (irony-get-libclang-flags)))))
    (setq irony-last-syntax-check nil)
    (irony-send-request :syntax-check request-data (or buffer (current-buffer))))
  (irony-wait-request-answer 'irony-last-syntax-check))


;; -- Working... --
;;
;; (defcustom irony-syntax-checking-function nil
;;   "Function to call when a syntax results are received."
;;   :type 'function
;;   :require 'irony
;;   :group 'irony)

(provide 'irony-syntax-check)
;;; irony-syntax-check.el ends here
