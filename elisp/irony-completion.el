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
(require 'irony-snippet)
(require 'irony-pp)

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

(defvar irony-completion-user-triggered nil)

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
  "Return nil if no completion context has been found.

Note: It doesn't mean a completion has been requested to the
irony-server since it might already be in cache. Check the
`irony-completion-request-running-count' to know if there are
some requests still pending."
  (let* ((stats (irony-completion-stats-at-point))
         (ctx-pos (car stats))
         (comp-pos (nth 1 stats)))
    (unless (eq ctx-pos (irony-completion-marker-position))
      (incf irony-completion-request-running-count)
      (setq irony-completion-user-triggered nil)
      (set-marker irony-completion-last-marker ctx-pos)
      (if (nth 2 stats)             ;pp-comp-p -> t
          (run-with-timer 0.2 nil 'irony-request-pp-complete)
        (irony-request-completion comp-pos)))
    (set-marker irony-completion-marker ctx-pos)
    ctx-pos))

(defun irony-trigger-completion ()
  "Request completion at point."
  (if (irony-trigger-completion-maybe)
      (when (irony-last-completion-available-p)
        (run-with-timer 0.2 nil 'run-hooks 'irony-on-completion-hook))
    (let ((comp-point (irony-get-completion-point-anywhere)))
      ;; if user triggered completion and completion point hasn't
      ;; change, just return the results, otherwise make a completion
      ;; request.
      (if (and irony-completion-user-triggered
               (eq comp-point (irony-last-completion-position)))
          (when (irony-last-completion-available-p)
            (run-with-timer 0.2 nil 'run-hooks 'irony-on-completion-hook))
        (incf irony-completion-request-running-count)
        (setq irony-completion-user-triggered t)
        (set-marker irony-completion-last-marker comp-point)
        (irony-request-completion comp-point)))))

(defun irony-completion-stats-at-point ()
  "Return a list such as:

    (context-pos completion-pos pp-completion-p)

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
  (save-excursion
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
     (let ((pp-comp-point (irony-pp-completion-point)))
       (when pp-comp-point
         (list pp-comp-point pp-comp-point t))))))

(defun irony-get-completion-point-anywhere ()
  "Return the completion point for the current context, contrary
to `irony-get-completion' a point will be returned every times."
  (save-excursion
    (or
     (if (re-search-backward "[^_a-zA-Z0-9]\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\=" nil t)
         (match-beginning 1))
     (point))))

(defun irony-handle-completion (data)
  (decf irony-completion-request-running-count)
  ;; ignore requests when there is some still pending b/c the
  ;; completion context is already "expired".
  (when (irony-last-completion-available-p)
    (setq irony-last-completion (cons data irony-completion-last-marker))
    (run-hooks 'irony-on-completion-hook)))

(defun irony-handle-server-completion (data)
  (irony-handle-completion (plist-get data :results)))

(defun irony-request-pp-complete ()
  (irony-handle-completion (irony-pp-complete-at)))

(defun irony-pp-complete-action ()
  "After the completion is complete, add the closing
character (double quote or angle-bracket) if needed."
  ;; do not add closing '>' or '"' when the completed item was a
  ;; directory.
  (unless (eq (char-before) ?/)
    (let ((ch (char-after)))
      (when (not (or (eq ch ?\")
                     (eq ch ?>)))
        (let ((line (buffer-substring-no-properties (point-at-bol) (point))))
          (when (string-match "#\\s-*include\\s-+\\([<\"]\\)" line)
            (insert (if (eq (string-to-char (match-string 1 line)) ?<)
                        ">"
                      "\""))))))))

(defun irony-comp-main-chunk-p (element)
  "Return t if ELEMENT is either the typed text or the current
  parameter.

The name is shit, I know..."
  (if (consp element)
      (eq (car element) 'p)
    (stringp element)))

(defun irony-comp-dynamic-snippet (candidate-data)
  "Return a cons of the for (SNIPPET-STR . HAS-PLACEHOLDER-P)
where SNIPPET-STR is a string as follow:

 - \"()\"
 - \"(${1:int x}, ${2:int y})$0\"

if HAS-PLACEHOLDER-P is nil then it's not a snippet and doesn't
need special expansion, otherwise it's a snippet and it will end
with the $0 string to the represent the final position after
expansion."
  (let ((dynamic-snippet "")
        (num-placeholders 0))
    ;; find the typed text or current parameter
    (while (and candidate-data
                (not (irony-comp-main-chunk-p (car candidate-data))))
      (setq candidate-data (cdr candidate-data)))
    ;; build snippet
    (dolist (e (cdr candidate-data))
      (cond
       ((characterp e)
        (setq dynamic-snippet (concat dynamic-snippet (list e)
                                      (when (eq e ?,) ;prettify commas
                                        " "))))
       ((stringp e)
        (setq dynamic-snippet (concat dynamic-snippet e)))
       ((and (consp e) (eq (car e) 't))
        (setq dynamic-snippet (concat dynamic-snippet (cdr e))))
       ((and (consp e) (eq (car e) 'ph))
        (incf num-placeholders)
        (setq dynamic-snippet
              (concat dynamic-snippet
                      (format "${%d:%s}" num-placeholders (cdr e)))))))
    (unless (zerop num-placeholders)
      (setq dynamic-snippet (concat dynamic-snippet "$0")))
    (cons dynamic-snippet
          (> num-placeholders 0))))


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
    (if irony-completion-user-triggered
        (let ((comp-point (irony-get-completion-point-anywhere)))
          (when (eq comp-point (irony-last-completion-position))
            comp-point))
      (let* ((stats (irony-completion-stats-at-point))
             (ctx-pos (car stats))
             (comp-point (nth 1 stats)))
        (when (eq ctx-pos (irony-last-completion-position))
          comp-point)))))

(defun irony-last-completion-results ()
  (if (stringp (car (irony-last-completion-data)))
      (irony-last-completion-data)
    (loop for result in (irony-last-completion-data)
          for priority = (cdr (assq 'p (cdr result)))
          unless (and priority
                      (> priority irony-priority-limit))
          collect result)))

(defun irony-post-completion-action (&optional candidate-data)
  "Post-completion action to execute.

CANDIDATE-DATA if provided contains informations about the
candidate that was expanded, in the format of a car of an element
returned by `irony-last-completion-results'."
  (if (irony-pp-inside-include-stmt-p)
      (irony-pp-complete-action)
    (unless (stringp candidate-data)
      (let ((dyn-snip (irony-comp-dynamic-snippet candidate-data)))
        (if (cdr dyn-snip)              ;has placeholder(s)?
            (when (irony-snippet-available-p)
              (irony-snippet-expand (car dyn-snip)))
          ;; no placeholder, just insert the string
          (insert (car dyn-snip))))))
  (irony-trigger-completion-maybe))

(provide 'irony-completion)
;;; irony-completion.el ends here
