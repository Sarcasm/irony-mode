;;; irony-completion.el --- irony-mode completion specific definitions

;; Copyright (C) 2012-2013  Guillaume Papin

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
;; completion when needed and the "parsing" of completion results.

;;; Code:

(require 'irony)
(require 'irony-header-comp)

(eval-when-compile
  (require 'cc-defs)                    ;for `c-save-buffer-state'
  (require 'cl))

;;
;; Public, customizable variables
;;

(defcustom irony-priority-limit 74
  "The Clang priority threshold to keep a candidate in the
completion list. Smaller values indicate higher-priority (more
 likely) completions."
  :type 'integer
  :require 'irony
  :group 'irony)

(defcustom irony-completion-trigger-commands '(self-insert-command
                                               newline-and-indent
                                               c-context-line-break)
  "List of commands used to trigger a look for smart/semantic
completion.

There are actually some hard-coded regexp as well in
`irony-completion-trigger-command-p`, if it causes any trouble
please report a bug."
  :type '(repeat function)
  :require 'irony
  :group 'irony)

(defcustom irony-on-completion-hook nil
  "Function called when new completion data are available.

TODO:
Completion results are available by the function starting with
`irony-completion-*'."
  :type '(repeat function)
  :require 'irony
  :group 'irony)

(defcustom irony-complete-typed-text-only nil
  "Do not complete arguments, parenthesis or anything else than
the typed text.

Typed text is often an identifier. So for a function

   void foo(int a, int b = 42)

only 'foo' will be returned by the irony server. Without this
option every information such as optional parameters, return
type, etc will be returned and processed by the completions
plugins.

This can eventually lead to performance improvement but more
limited usefulness."
  :type 'boolean
  :require 'irony
  :group 'irony)


;; Register completion callback(s) in the `irony-request-mapping'
;; alist.
;;

;;;###autoload
(add-to-list 'irony-request-mapping '(:completion . irony-handle-server-completion))

;;;###autoload
(add-to-list 'irony-request-mapping
             '(:completion-simple . irony-handle-server-completion))

;;;###autoload
(add-hook 'irony-mode-hook 'irony-setup-completion)



;; Privates variables
;;

(defvar irony-completion-marker (make-marker))

(defvar irony-completion-last-marker (make-marker))

(defvar irony-completion-request-running-count 0)

(defvar irony-last-completion nil
  "Cons of (COMPLETION-DATA . MARKER)")



;; Functions
;;
(defun irony-setup-completion ()
  "Initialize completion module for irony-mode."
  (add-hook 'post-command-hook 'irony-completion-post-command nil t))

(defun irony-completion-trigger-command-p (command)
  "Return non-nil if `COMMAND' is a trigger command.

Stolen from `auto-complete` package."
  (and (symbolp command)
       (or (memq command irony-completion-trigger-commands)
           (string-match-p "^c-electric-" (symbol-name command)))))

(defun irony-completion-marker-position ()
  (when (eq (marker-buffer irony-completion-marker) (current-buffer))
    (marker-position irony-completion-marker)))

(defun irony-request-completion (&optional pos)
  (let* ((location (irony-point-location (or pos (point))))
         (request-data (list (cons :file (irony-temp-filename))
                             (cons :flags (irony-get-libclang-flags))
                             (cons :line (car location))
                             (cons :column (cdr location)))))
    (irony-send-request (if irony-complete-typed-text-only
                            :complete-simple
                          :complete)
                        request-data
                        (current-buffer))))

(defun irony-completion-post-command ()
  (when (irony-completion-trigger-command-p this-command)
    (irony-trigger-completion-maybe)))

(defun irony-trigger-completion-maybe ()
  (save-excursion
    (let* ((stats (irony-completion-stats-at-point))
           (ctx-pos (car stats)))
      (unless (eq ctx-pos (irony-completion-marker-position))
        (incf irony-completion-request-running-count)
        (set-marker irony-completion-last-marker ctx-pos)
        (if (nth 2 stats)             ;header-comp-p -> t
            (run-with-timer 0.1 nil 'irony-request-header-comp)
          (irony-request-completion (nth 1 stats))))
      (set-marker irony-completion-marker ctx-pos))))

(defun irony-completion-stats-at-point ()
  "Return a list such as:

    (context-pos completion-pos header-completion-p)

completion-pos is the point where the typed text starts on a
completion while context-pos is the position that decides of the
kind of completion has to be performed. I don't think it's clear
at all, here is an example:


    1. std::string s(\"hey\");
    2.
    3. s.size();
    4. s.   size();

For #3: s.[CONTEXT-POS][COMPLETION-POS]size
For #4: s.[CONTEXT-POS]   [COMPLETION-POS]size

Nil is returned if there is no completion context found.

Note: This function try to return the point only in case where it
seems to be interesting and not too slow to show the completion
under point. If you want to have the completion *explicitly* you
should use `irony-get-completion-point-anywhere'."
  ;; Try different possibilities...
  (or
   ;; - Object member access: '.'
   ;; - Pointer member access: '->'
   ;; - Scope operator: '::'
   (when (re-search-backward "\\(\\.\\|->\\|::\\)[ \t\n\r]*\\(\\(?:[_a-zA-Z][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
     (let ((res (list (match-end 1) (match-beginning 2))))
       ;; fix floating number literals (the prefix tried to complete
       ;; the following "3.[COMPLETE]")
       (unless (re-search-backward "[^_a-zA-Z0-9][[:digit:]]+\\.[[:digit:]]*\\=" nil t)
         res)))
   ;; Initialization list (use the syntactic informations partially
   ;; stolen from `c-show-syntactic-information')
   ;; A::A() : [complete], [complete]
   (when (re-search-backward "\\([,:]\\)[ \t\n\r]*\\(\\(?:[_a-zA-Z][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
     (let* ((res (list (match-end 1) (match-beginning 2)))
            (c-parsing-error nil)
            (syntax (if (boundp 'c-syntactic-context)
                        c-syntactic-context
                      (c-save-buffer-state nil (c-guess-basic-syntax)))))
       (if (or (assoc 'member-init-intro syntax)
               (assoc 'member-init-cont syntax))
           ;; Check if were are in an argument list
           ;; without this when we have:
           ;;  A::A() : foo(bar, []
           ;; the completion is triggered.
           (when (eq (car (syntax-ppss)) 0) ;see [[info:elisp#Parser State]]
             res))))
   ;; switch/case statements, complete after the case
   (when (re-search-backward "\\Sw\\(case\\)\\s-+\\(\\(?:[_a-zA-Z][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
     ;; FIXME: Why a match is given after the colon?
     ;;
     ;;            case blah:[POINT-STILL-INDICATES-CASE-COMPLETION]
     ;;
     (list (match-end 1) (match-beginning 2)))
   ;; preprocessor #define, #include, ...
   (when (re-search-backward "^\\s-*\\(#\\)\\s-*\\([[:alpha:]]*\\)\\=" nil t)
     (list (match-end 1) (match-beginning 2)))
   (let ((header-comp-point (irony-header-comp-point)))
     (when header-comp-point
       (list header-comp-point header-comp-point t)))))

(defun irony-get-completion-point-anywhere ()
  "Return the completion point for the current context, contrary
to `irony-get-completion' a point will be returned every times."
  (or
   (if (re-search-backward "[^_a-zA-Z0-9]\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\=" nil t)
       (match-beginning 1))
   (point)))

(defun irony-handle-completion (data)
  (decf irony-completion-request-running-count)
  ;; ignore requests when there is some still pending b/c the
  ;; completion context is already "expired".
  (when (irony-last-completion-available-p)
    (setq irony-last-completion (cons data irony-completion-last-marker))
    (run-hooks 'irony-on-completion-hook)))

(defun irony-handle-server-completion (data)
  (irony-handle-completion (plist-get data :results)))

(defun irony-request-header-comp ()
  (irony-handle-completion (irony-header-comp-complete-at)))



;;
;; Irony completion results "API"
;; Functions useful for completion handlers.
;;

(defun irony-last-completion-data ()
  (car irony-last-completion))

(defun irony-last-completion-available-p ()
  (zerop irony-completion-request-running-count))

(defun irony-last-completion-position ()
  (let ((marker (cdr irony-last-completion)))
    (when (and marker
               (eq (marker-buffer marker) (current-buffer)))
      (marker-position marker))))

(defun irony-get-last-completion-point ()
  "Return the beginning of the completion for the latest completion.

Return nil if no completion point is available, for example if
the latest completion is not valid anymore for the current cursor
position."
  (when (irony-last-completion-available-p)
    (let* ((stats (irony-completion-stats-at-point))
           (ctx-pos (car stats))
           (comp-point (nth 1 stats)))
      (when (eq ctx-pos (irony-last-completion-position))
          comp-point))))

(defun irony-last-completion-results ()
  (if (stringp (car (irony-last-completion-data)))
      (irony-last-completion-data)
    (loop for completion-cell in (irony-last-completion-data)
          for kind = (car completion-cell)
          for result = (cdr completion-cell)
          for priority = (or (plist-get result :priority) irony-priority-limit)
          when (and (< priority irony-priority-limit)
                    (not (memq kind irony-blacklist-kind)))
          collect result)))

(provide 'irony-completion)
;;; irony-completion.el ends here
