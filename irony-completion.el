;;; irony-completion.el --- irony-mode completion interface  -*- lexical-binding: t -*-

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

(defcustom irony-completion-availability-filter '(available deprecated)
 "For completion, only accept candidates whose availability is in the list.

Maps to libclang's CXAvailabilityKind:
- https://clang.llvm.org/doxygen/group__CINDEX.html#gada331ea0195e952c8f181ecf15e83d71

Due to a bug in
Clang (https://bugs.llvm.org//show_bug.cgi?id=24329), candidates
that can be validly accessed are deemed not-accessible."
 :type '(repeat symbol)
 :options '(available deprecated not-accessible)
 :group 'irony-completion)


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

(defsubst irony-completion--skip-whitespaces-backward ()
  ;;(skip-syntax-backward "-") doesn't seem to care about newlines
  (skip-chars-backward " \t\n\r"))

(defun irony-completion--parse-context-position (&optional pos)
  (save-excursion
    (when pos
      (goto-char pos))
    (irony-completion--skip-whitespaces-backward)
    (point)))

(defun irony--completion-line-column (&optional pos)
  (save-excursion
    (when pos
      (goto-char pos))
    ;; `position-bytes' to handle multibytes and 'multicolumns' (i.e
    ;; tabulations) characters properly
    (irony--without-narrowing
      (cons
       (line-number-at-pos)
       (1+ (- (position-bytes (point))
              (position-bytes (point-at-bol))))))))


;;
;; Functions
;;

(defun irony-completion--enter ()
  (add-hook 'completion-at-point-functions 'irony-completion-at-point nil t))

(defun irony-completion--exit ()
  (remove-hook 'completion-at-point-functions 'irony-completion-at-point t))

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

(irony-iotask-define-task irony--t-complete
  "`complete' server command."
  :start (lambda (file line column compile-options)
           (apply #'irony--server-send-command "complete" file line column "--"
                  compile-options))
  :update irony--server-command-update)

(defun irony--complete-task-1 (&optional buffer pos)
  (with-current-buffer (or buffer (current-buffer))
    (let ((line-column (irony--completion-line-column pos)))
      (irony-iotask-package-task irony--t-complete
                                 (irony--get-buffer-path-for-server)
                                 (car line-column)
                                 (cdr line-column)
                                 (irony--adjust-compile-options)))))

(defun irony--complete-task (&optional buffer pos)
  (let ((unsaved-tasks (irony--unsaved-buffers-tasks))
        (complete-task (irony--complete-task-1 buffer pos)))
    (if unsaved-tasks
        (irony-iotask-chain unsaved-tasks complete-task)
      complete-task)))

(irony-iotask-define-task irony--t-candidates
  "`candidates' server command."
  :start (lambda (prefix style)
           (irony--server-send-command
            "candidates" prefix
            (cl-case style
              (case-insensitive "case-insensitive")
              (smart-case "smart-case")
              (t "exact"))))
  :update irony--server-query-update)

(defun irony--candidates-task (&optional buffer pos prefix style)
  (irony-iotask-chain
   (irony--complete-task buffer pos)
   (irony-iotask-package-task irony--t-candidates prefix style)))


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

(defun irony-completion-availability (candidate)
  "See `irony-completion-availability-filter'"
  (nth 7 candidate))

(defun irony-completion--filter-candidates (candidates)
  (cl-remove-if-not
   (lambda (candidate)
     (memq (irony-completion-availability candidate)
           irony-completion-availability-filter))
   candidates))

(defun irony-completion-candidates (&optional prefix style)
  "Return the list of candidates at point.

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
    Example: (\"(int a, int b)\" 1 6 8 13)
 7. The availability of the candidate."
  (irony--awhen (irony-completion-symbol-bounds)
    (irony-completion--filter-candidates
     (irony--run-task
      (irony--candidates-task nil (car it) prefix style)))))

(defun irony-completion-candidates-async (callback &optional prefix style)
  (irony--aif (irony-completion-symbol-bounds)
      (irony--run-task-asynchronously
       (irony--candidates-task nil (car it) prefix style)
       (lambda (candidates-result)
         (funcall callback (irony-completion--filter-candidates
                            (irony-iotask-result-get candidates-result)))))
    (funcall callback nil)))

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
          (irony-completion--skip-whitespaces-backward)
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

(defsubst irony-completion--capf-candidate (candidate)
  (get-text-property 0 'irony-capf candidate))

(defun irony-completion--capf-annotate (candidate)
  (irony-completion-annotation
   (irony-completion--capf-candidate candidate)))

(defun irony-completion--capf-postcomp-commonprefix (candidates)
  (let ((prefixes (mapcar
                   (lambda (candidate)
                     (let ((str (irony-completion-post-comp-str candidate))
                           (phs (irony-completion-post-comp-placeholders
                                 candidate)))
                       (substring str 0 (car phs))))
                   candidates)))
    (cl-loop for i from 0 below (apply #'min (mapcar #'length prefixes))
             while (apply #'= (mapcar (lambda (string) (aref string i))
                                          prefixes))
             finally (return (cl-subseq (first prefixes) 0 i)))))

(defun irony-completion--capf-postcomp-all-equal-p (candidates)
  (when (cdr candidates)
    (let ((expected-str (irony-completion-post-comp-str (car candidates)))
          (expected-phs (irony-completion-post-comp-placeholders
                         (car candidates))))
      (while (and (setq candidates (cdr candidates))
                  (string= expected-str (irony-completion-post-comp-str (car candidates)))
                  (equal expected-phs (irony-completion-post-comp-placeholders (car candidates))))))
    (null candidates)))

(defun irony-completion--capf-exit-function (candidates str status)
  "Insert post completion string or snippet after STR has been completed."
  ;; according to `pcomplete-completions-at-point',
  ;; react on `finished' but not `sole',
  ;; because it does not work properly when cycling completions
  (when (eq status 'finished)
    (let ((candidate (irony-completion--capf-candidate str))
          matches)
      ;; `completion-at-point' doesn't provides the propertized string created
      ;; with the `irony-capf' text property
      ;; but `company-capf' does, and maybe `completion-at-point' some day.
      ;; So if the candidate data is found use it,
      ;; otherwise try to find the candidate in the completion list
      ;; at the risk of dealing with overloaded functions and not being able to
      ;; make the right decision
      (setq matches
            (if candidate
                (list candidate)
              (cl-remove-if-not (lambda (candidate)
                                  (string= (car candidate) str))
                                candidates)))
      ;; all equals can happen with when the difference in the annotation
      ;; is the return type and constness attributes,
      ;; for example `std::string' `at(n)' function is overloaded that way:
      ;;     const char & at(size_type n) const;
      ;;           char & at(size_type n);
      (if (or (= (length matches) 1)
              (irony-completion--capf-postcomp-all-equal-p matches))
          ;; with a perfect match we are able to give complete post-completion
          (irony-completion-post-complete (car matches))
        ;; more than one candidates possible,
        ;; provide the beginning of the post-completion data as a best effort.
        ;; For overloaded functions this inserts the opening parenthesis.
        (irony--awhen (irony-completion--capf-postcomp-commonprefix matches)
                      (insert it))))))

;;;###autoload
(defun irony-completion-at-point ()
  (irony--awhen (and irony-mode (irony-completion-symbol-bounds))
    (let ((candidates (irony-completion--filter-candidates
                       (irony--run-task
                        (irony--candidates-task
                         nil
                         (car it)
                         (buffer-substring-no-properties
                          (car it) (cdr it))
                         (if completion-ignore-case
                             'case-insensitive
                           'exact))))))
      (list
       (car it)                           ;start
       (cdr it)                           ;end
       (mapcar (lambda (candidate)        ;completion table
                 (propertize (car candidate) 'irony-capf candidate))
               candidates)
       :annotation-function #'irony-completion--capf-annotate
       :exit-function
       (lambda (str status)
         (irony-completion--capf-exit-function candidates str status))))))

(provide 'irony-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-completion.el ends here
