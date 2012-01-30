;;; irony-completion.el --- irony-mode completion specific definitions

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

;; Handle the search of completion points, the triggering of the
;; completion when needed, the "parsing" of completion results.
;;
;; The completion can be auto feature is automated but can also be
;; triggered by the user.

;;; Code:

(require 'irony)

(defcustom irony-completion-async-function nil
  "Function to call when new completion results are received. The
function will be called with the point of completion, and the
list of completion strings available (NO filtering is made to
remove completion with non matching prefix)."
  :type 'function
  :require 'irony
  :group 'irony)

(defcustom irony-priority-limit 74
  "The Clang priority threshold to keep a candidate in the
completion list. Smaller values indicate higher-priority (more
likely) completions."
  :type 'integer
  :require 'irony
  :group 'irony)

(defvar irony-last-completion nil
  "If non nil contain the last completion answer received by the
  server (internal variable).")

;;;###autoload
(add-to-list 'irony-request-mapping '(:completion . irony-handle-completion))

;;;###autoload
(add-to-list 'irony-request-mapping '(:completion-simple . irony-handle-completion-simple))

(defun irony-handle-completion (data)
  "Handle a completion request from the irony process,
actually because the code completion is not 'asynchronous' this
function only set the variable `irony-last-completion'."
  (setq irony-last-completion (cons :detailed data)))

(defun irony-handle-completion-simple (data)
  "See `irony-handle-completion'."
  (setq irony-last-completion (cons :simple data)))

(defun irony-complete (kind &optional pos)
  "Return a list of completions available at POS (the current
position if not given). The completion KIND can be
either :detailed or :simple."
  (let* ((location (irony-point-location (or pos (point))))
         (request-data (list (cons :file (irony-temp-filename))
                             (cons :flags (irony-get-flags))
                             (cons :line (car location))
                             (cons :column (cdr location)))))
    (setq irony-last-completion nil)
    (irony-send-request (if (eq kind :detailed)
                            :complete
                          :complete-simple)
                        request-data
                        (current-buffer)))
  (loop with answer = (cdr (irony-wait-request-answer 'irony-last-completion))
        for completion-cell in (plist-get answer :results)
        for kind = (car completion-cell)
        for result = (cdr completion-cell)
        for priority = (or (plist-get result :priority) irony-priority-limit)
        when (and (< priority irony-priority-limit)
                  (not (memq kind blacklist-kind)))
        collect result))

(defun irony-get-completion-point ()
  "Return the point where the completion should start from the
current point. If no completion can be used in the current
context return NIL.

Note: This function try to return the point only in case where it
seems to be interesting and not too slow to show the completion
under point. If you want to have the completion *explicitly* you
should use `irony-get-completion-point-anywhere'."
  ;; Try different possibilities...
  (or
   ;; - Object member access: '.'
   ;; - Pointer member access: '->'
   ;; - Scope operator: '::'
   (if (re-search-backward "\\(?:\\.\\|->\\|::\\)\\(\\(?:[_a-zA-Z][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
       (let ((point (match-beginning 1)))
         ;; fix floating number literals (the prefix tried to complete
         ;; the following "3.[COMPLETE]")
         (unless (re-search-backward "[^_a-zA-Z0-9][[:digit:]]+\\.[[:digit:]]*\\=" nil t)
           point)))
   ;; Initialization list (use the syntactic informations partially
   ;; stolen from `c-show-syntactic-information')
   ;; A::A() : [complete], [complete]
   (if (re-search-backward "[,:]\\s-*\\(\\(?:[_a-zA-Z][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
       (let* ((point (match-beginning 1))
              (c-parsing-error nil)
              (syntax (if (boundp 'c-syntactic-context)
                          c-syntactic-context
                        (c-save-buffer-state nil (c-guess-basic-syntax)))))
         (if (or (assoc 'member-init-intro (c-guess-basic-syntax))
                 (assoc 'member-init-cont (c-guess-basic-syntax)))
             ;; Check if were are in an argument list
             ;; without this when we have:
             ;;  A::A() : foo(bar, []
             ;; the completion is triggered.
             (if (eq (car (syntax-ppss)) 0) ;see [[info:elisp#Parser State]]
                 point))))
   ;; switch/case statements, complete after the case
   (if (re-search-backward "[ \n\t\v\r\f;{]case\\s-+\\(\\(?:[_a-zA-Z][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
       (match-beginning 1))))

(defun irony-get-completion-point-anywhere ()
  "Return the completion point for the current context, contrary
to `irony-get-completion' a point will be returned every times."
  (or
   (if (re-search-backward "[^_a-zA-Z0-9]\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\=" nil t)
       (match-beginning 1))
   (point)))

(provide 'irony-completion)
;;; irony-completion.el ends here
