;;; irony-completion.el --- irony-mode completion interface

;; Copyright (C) 2012-2014  Guillaume Papin

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

;; Handle the search of completion points, the triggering of the
;; completion when needed and the "parsing" of completion results.

;;; Code:

(require 'irony)
(require 'irony-snippet)

(require 'cl-lib)

(eval-when-compile
  (require 'cc-defs))                   ;for `c-save-buffer-state'


;;
;; Customizable variables
;;

(defcustom irony-completion-trigger-commands '(self-insert-command
                                               newline-and-indent
                                               c-context-line-break
                                               c-scope-operator)
  "List of commands to watch for asynchronous completion triggering.

There are actually some hard-coded regexp as well in
`irony-completion-trigger-command-p', if it causes any trouble
please report a bug."
  :type '(repeat function)
  :group 'irony)

;;;###autoload
(defcustom irony-completion-hook nil
  ;; TODO: proper documentation
  "Function called when new completion data are available."
  :type 'hook
  :group 'irony)


;;
;; Internal variables
;;

(defvar-local irony-completion--context-pos nil)
(defvar-local irony-completion--context-tick 0)
(defvar-local irony-completion--context-candidates nil)


;;
;; Utility functions
;;

(defun irony-completion--symbol-bounds ()
  (let ((pt (point)))
    (save-excursion
      (skip-chars-backward "_a-zA-Z0-9")
      (let ((ch (char-after)))
        (if (and (>= ch ?0) (<= ch ?9)) ;symbols can't start with a digit
            (cons pt pt)
          (setq pt (point))
          (skip-chars-forward "_a-zA-Z0-9")
          (cons pt (point)))))))

(defun irony-completion--beginning-of-symbol ()
  (car (irony-completion--symbol-bounds)))

(defun irony-completion--end-of-symbol ()
  (cdr (irony-completion--symbol-bounds)))

(defun irony-completion--context-pos ()
  (let ((syntax (syntax-ppss)))
    ;; no context in strings and comments
    ;; TODO: Use fontlock faces instead? at least
    ;;     #warning In the middle of a warning|
    ;; will be handled properly but things like the link will be messed-up
    ;; (`goto-address-prog-mode' feature):
    ;;     #error see bug report XY: http://example.com/XY
    (unless (or (nth 3 syntax)          ;strings
                (nth 4 syntax))         ;comments
      (save-excursion
        (goto-char (irony-completion--beginning-of-symbol))
        (skip-chars-backward " \t\n\r")
        (point)))))


;;
;; Functions
;;

(defun irony-completion--enter ()
  (add-hook 'post-command-hook 'irony-completion-post-command nil t)
  (add-hook 'completion-at-point-functions 'irony-completion-at-point nil t))

(defun irony-completion--exit ()
  (remove-hook 'post-command-hook 'irony-completion-post-command)
  (remove-hook 'completion-at-point-functions 'irony-completion-at-point t)
  (setq irony-completion--context-pos nil
        irony-completion--context-candidates nil
        irony-completion--context-tick 0))

(defun irony-completion-post-command ()
  (when (and (irony-completion-trigger-command-p this-command)
             (irony-completion--update-context)
             (irony-completion--trigger-context-p))
    (irony-completion--send-request)))

(defun irony-completion-trigger-command-p (command)
  "Whether or not COMMAND is a completion trigger command.

Stolen from `auto-complete` package."
  (and (symbolp command)
       (or (memq command irony-completion-trigger-commands)
           (string-match-p "^c-electric-" (symbol-name command)))))

(defun irony-completion--update-context ()
  "Update the completion context variables based on the current position.

Return t if the context has been updated, nil otherwise."
  (let ((ctx-pos (irony-completion--context-pos)))
    (if (eq ctx-pos irony-completion--context-pos)
        nil
      (setq irony-completion--context-pos ctx-pos
            irony-completion--context-candidates nil
            irony-completion--context-tick (1+ irony-completion--context-tick))
      t)))

(defun irony-completion--trigger-context-p ()
  "Whether or not completion is expected to be triggered for the
the current context."
  (when irony-completion--context-pos
    (save-excursion
      (goto-char irony-completion--context-pos)
      (re-search-backward
       (format "%s\\="                 ;see Info node `(elisp) Regexp-Backslash'
               (regexp-opt '("."       ;object member access
                             "->"      ;pointer member access
                             "::")))   ;scope operator
       nil t))))


;;
;; Interface with irony-server
;;

(defun irony-completion--send-request (&optional async-cb)
  (let (line column)
    (save-excursion
      (goto-char (irony-completion--beginning-of-symbol))
      ;; `position-bytes' to handle multibytes and 'multicolumns' (i.e
      ;; tabulations) characters properly
      (irony-without-narrowing
        (setq line (line-number-at-pos)
              column (1+ (- (position-bytes (point))
                            (position-bytes (point-at-bol)))))))
    (irony--send-file-request
     "complete"
     (list 'irony-completion--request-handler
           irony-completion--context-tick
           async-cb)
     (number-to-string line)
     (number-to-string column))))

(defun irony-completion--request-handler (candidates tick &optional async-cb)
  (when (eq tick irony-completion--context-tick)
    (setq irony-completion--context-candidates candidates)
    (run-hooks 'irony-completion-hook)
    (when async-cb
      (funcall async-cb))))


;;
;; Irony Completion Interface
;;

(defun irony-completion-at-point ()
  (irony-completion--update-context)
  (when irony-completion--context-candidates
    (let ((symbol-bounds (irony-completion--symbol-bounds)))
      (list (car symbol-bounds) (cdr symbol-bounds)
            (mapcar 'car irony-completion--context-candidates)))))

(defun irony-completion-at-point-async ()
  (interactive)
  (irony-completion--update-context)
  (if irony-completion--context-candidates
      (completion-at-point)
    (when irony-completion--context-pos
      (irony-completion--send-request 'completion-at-point))))

(provide 'irony-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-completion.el ends here
