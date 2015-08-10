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


;;
;; Customizable variables
;;

(defgroup irony-completion nil
  "Irony's completion interface."
  :group 'irony)

(defcustom irony-completion-trigger-commands '(self-insert-command
                                               newline-and-indent
                                               c-context-line-break
                                               c-scope-operator
                                               ;; electric commands
                                               c-electric-backspace
                                               c-electric-brace
                                               c-electric-colon
                                               c-electric-lt-gt
                                               c-electric-paren
                                               c-electric-pound
                                               c-electric-semi&comma
                                               c-electric-slash
                                               c-electric-star)
  "List of commands to watch for asynchronous completion triggering."
  :type '(repeat function)
  :group 'irony-completion)


;;
;; Internal variables
;;

(defvar-local irony-completion--context nil)
(defvar-local irony-completion--context-tick 0)
(defvar-local irony-completion--request-callbacks nil)
(defvar-local irony-completion--request-tick 0)
(defvar-local irony-completion--candidates nil)
(defvar-local irony-completion--candidates-tick 0)


;;
;; Utility functions
;;

(defun irony-completion-symbol-bounds ()
  (let ((pt (point))
        (syntax (syntax-ppss)))
    ;; no prefix for strings or comments
    ;; TODO: Use fontlock faces instead? at least
    ;;     #warning In the middle of a warning|
    ;; will be handled properly but things like links when
    ;; `goto-address-prog-mode' is enabled will mess up things:
    ;;     #error see bug report XY: http://example.com/XY
    (unless (or (nth 3 syntax)          ;skip strings
                (nth 4 syntax))         ;skip comments
      (save-excursion
        (skip-chars-backward "_a-zA-Z0-9")
        (let ((ch (char-after)))
          (unless (and ch (>= ch ?0) (<= ch ?9)) ;skip numbers
            (when (eq (char-before) ?~)
              (backward-char))
            (setq pt (point))
            (skip-chars-forward "_a-zA-Z0-9~")
            (cons pt (point))))))))

(defun irony-completion-beginning-of-symbol ()
  (car (irony-completion-symbol-bounds)))

(defun irony-completion-end-of-symbol ()
  (cdr (irony-completion-symbol-bounds)))

(defsubst irony-completion--skip-whitespace-backwards ()
  ;;(skip-syntax-backward "-") doesn't seem to care about newlines
  (skip-chars-backward " \t\n\r"))

(defun irony-completion--context-pos ()
  (irony--awhen (irony-completion-beginning-of-symbol)
    (save-excursion
      (goto-char it)
      (irony-completion--skip-whitespace-backwards)
      (point))))


;;
;; Functions
;;

(defun irony-completion--enter ()
  (add-hook 'post-command-hook 'irony-completion--post-command nil t)
  (add-hook 'completion-at-point-functions 'irony-completion-at-point nil t))

(defun irony-completion--exit ()
  (remove-hook 'post-command-hook 'irony-completion--post-command t)
  (remove-hook 'completion-at-point-functions 'irony-completion-at-point t)
  (setq irony-completion--context nil
        irony-completion--candidates nil
        irony-completion--context-tick 0
        irony-completion--request-tick 0
        irony-completion--request-callbacks nil
        irony-completion--candidates-tick 0))

(defun irony-completion--post-command ()
  (when (and (memq this-command irony-completion-trigger-commands)
             (irony-completion--update-context)
             (irony-completion-at-trigger-point-p))
    (irony-completion--send-request)))

(defun irony-completion--update-context ()
  "Update the completion context variables based on the current position.

Return t if the context has been updated, nil otherwise."
  (let ((ctx (irony-completion--context-pos)))
    (if (eq ctx irony-completion--context)
        nil
      (setq irony-completion--context ctx
            irony-completion--candidates nil
            irony-completion--context-tick (1+ irony-completion--context-tick))
      (unless irony-completion--context
        ;; when there is no context, assume that the candidates are available
        ;; even though they are nil
        irony-completion--request-tick irony-completion--context-tick
        irony-completion--request-callbacks nil
        irony-completion--candidates nil
        irony-completion--candidates-tick irony-completion--context-tick)
      t)))

(defun irony-completion--post-complete-yas-snippet (str placeholders)
  (let ((ph-count 0)
        (from 0)
        to snippet)
    (while
        (setq to (car placeholders)
              snippet (concat
                       snippet
                       (substring str from to)
                       (format "${%d:%s}"
                               (cl-incf ph-count)
                               (substring str
                                          (car placeholders)
                                          (cadr placeholders))))
              from (cadr placeholders)
              placeholders (cddr placeholders)))
    ;; handle the remaining non-snippet string, if any.
    (concat snippet (substring str from) "$0")))


;;
;; Interface with irony-server
;;

(defun irony-completion--send-request ()
  (let (line column)
    (save-excursion
      (goto-char (irony-completion-beginning-of-symbol))
      ;; `position-bytes' to handle multibytes and 'multicolumns' (i.e
      ;; tabulations) characters properly
      (irony--without-narrowing
        (setq line (line-number-at-pos)
              column (1+ (- (position-bytes (point))
                            (position-bytes (point-at-bol)))))))
    (setq irony-completion--request-callbacks nil
          irony-completion--request-tick irony-completion--context-tick)
    (irony--send-parse-request
     "complete"
     (list 'irony-completion--request-handler irony-completion--context-tick)
     (number-to-string line)
     (number-to-string column))))

(defun irony-completion--request-handler (candidates tick)
  (when (eq tick irony-completion--context-tick)
    (setq
     irony-completion--candidates-tick tick
     irony-completion--candidates candidates)
    (mapc 'funcall irony-completion--request-callbacks)))

(defun irony-completion--still-completing-p ()
  (unless (irony-completion-candidates-available-p)
    (eq irony-completion--request-tick irony-completion--context-tick)))


;;
;; Irony Completion Interface
;;

(defun irony-completion-typed-text (candidate)
  (nth 0 candidate))

(defun irony-completion-priority (candidate)
  (nth 1 candidate))

(defun irony-completion-type (candidate)
  (nth 2 candidate))

(defun irony-completion-brief (candidate)
  (nth 3 candidate))

(defun irony-completion-annotation (candidate)
  (substring (nth 4 candidate) (nth 5 candidate)))

(defun irony-completion-post-comp-str (candidate)
  (car (nth 6 candidate)))

(defun irony-completion-post-comp-placeholders (candidate)
  (cdr (nth 6 candidate)))

(defun irony-completion-candidates-available-p ()
  (and (eq (irony-completion--context-pos) irony-completion--context)
       (eq irony-completion--candidates-tick irony-completion--context-tick)))

(defun irony-completion-candidates ()
  "Return the list of candidates at point, if available.

Use the function `irony-completion-candidates-available-p' to
know if the candidate list is available.

A candidate is composed of the following elements:
 0. The typed text. Multiple candidates can share the same string
    because of overloaded functions, default arguments, etc.
 1. The priority.
 2. The [result-]type of the candidate, if any.
 3. If non-nil, contains the Doxygen brief documentation of the
    candidate.
 4. The signature of the candidate excluding the result-type
    which is available separately.
    Example: \"foo(int a, int b) const\"
 5. The annotation start, a 0-based index in the prototype string.
 6. Post-completion data. The text to insert followed by 0 or
    more indices. These indices work by pairs and describe ranges
    of placeholder text.
    Example: (\"(int a, int b)\" 1 6 8 13)"
  (and (irony-completion-candidates-available-p)
       irony-completion--candidates))

(defun irony-completion-candidates-async (callback)
  "Call CALLBACK when asynchronous completion is available.

Note that:
 - If the candidates are already available, CALLBACK is called
   immediately.
 - In some circumstances, CALLBACK may not be called. i.e:
   irony-server crashes, ..."
  (irony-completion--update-context)
  (if (irony-completion-candidates-available-p)
      (funcall callback)
    (when irony-completion--context
      (unless (irony-completion--still-completing-p)
        (irony-completion--send-request))
      (setq irony-completion--request-callbacks
            (cons callback irony-completion--request-callbacks)))))

(defun irony-completion-post-complete (candidate)
  (let ((str (irony-completion-post-comp-str candidate))
        (placeholders (irony-completion-post-comp-placeholders candidate)))
    (if (and placeholders (irony-snippet-available-p))
        (irony-snippet-expand
         (irony-completion--post-complete-yas-snippet str placeholders))
      (insert (substring str 0 (car placeholders))))))

(defun irony-completion-at-trigger-point-p ()
  (when (eq (point) (irony-completion-beginning-of-symbol))
    (save-excursion
      (cond
       ;; use `re-search-backward' so that the cursor is moved just before the
       ;; member access, if any
       ((re-search-backward
         (format "%s\\=" (regexp-opt '("."     ;object member access
                                       "->"    ;pointer member access
                                       "::"))) ;scope operator
         (point-at-bol) t)
        (unless
            ;; ignore most common uses of '.' where it's not a member access
            (and (eq (char-after) ?.)
                 (or
                  ;; include statements: #include <foo.|>
                  (looking-back  "^#\\s-*include\\s-+[<\"][^>\"]*"
                                 (point-at-bol))
                  ;; floating point numbers (not thorough, see:
                  ;; http://en.cppreference.com/w/cpp/language/floating_literal)
                  (looking-back "[^_a-zA-Z0-9][[:digit:]]+" (point-at-bol))))
          ;; except the above exceptions we use a "whitelist" for the places
          ;; where it looks like a member access
          (irony-completion--skip-whitespace-backwards)
          (or
           ;; after brackets consider it's a member access so things like
           ;; 'getFoo().|' match
           (memq (char-before) (list ?\) ?\] ?} ?>))
           ;; identifiers but ignoring some keywords
           ;;
           ;; handle only a subset of template parameter packs, where the
           ;; ellipsis is preceded by a keyword, in situation like:
           ;;     template<typename ... Args> class X {...};
           ;;     template<typename .|
           ;; or just look if the face is: font-lock-keyword-face?
           (save-excursion
             (and (re-search-backward "\\b\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\="
                                      (point-at-bol) t)
                  (not (member (match-string 0)
                               '("class" "sizeof" "typename"))))))))))))


;;
;; Irony CAPF
;;

(defun irony-completion--at-point-annotate (candidate)
  (irony-completion-annotation
   (get-text-property 0 'irony-capf candidate)))

;;;###autoload
(defun irony-completion-at-point ()
  (when (and irony-mode (irony-completion-candidates-available-p))
    (let ((symbol-bounds (irony-completion-symbol-bounds)))
      (list
       (car symbol-bounds)              ;start
       (cdr symbol-bounds)              ;end
       (mapcar #'(lambda (candidate)    ;completion table
                   (propertize (car candidate) 'irony-capf candidate))
               (irony-completion-candidates))
       :annotation-function 'irony-completion--at-point-annotate))))

;;;###autoload
(defun irony-completion-at-point-async ()
  (interactive)
  (irony-completion-candidates-async 'completion-at-point))

(provide 'irony-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-completion.el ends here
