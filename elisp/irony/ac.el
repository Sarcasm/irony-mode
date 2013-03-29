;;; irony/ac.el --- Irony completion with auto-complete

;; Copyright (C) 2011-2013  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: c, convenience

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

;; `irony-mode' sources for `auto-complete'. Information displayed in
;; the completion try to be detailed (use the completion provided by
;; `irony-complete', see function documentation for more information).
;;
;; The functionalities provided are:
;; - functions with optional parameters are distinguished in order to
;;   choose the correct function directly when possible. ATM of this
;;   writting the only known possible way to get this is to use a
;;   special fork of auto-complete as specified in the README.md.
;; - if yasnippet is installed on the system, snippet expansion is
;;   performed after the completion a completion has been entered by
;;   the user.

;;; Usage:
;;      (irony-enable 'ac)
;;
;; Note: Call `ac-complete-irony' to trigger the completion manually.
;; It's binded to `C-RET' by default when `irony-ac-setup' is used.

;;; Code:

(require 'irony-completion)
(require 'auto-complete)
(require 'popup)

(defcustom irony-ac-show-priority nil
  "Non-nil means the priority of the result will be shown in the
completion menu.

This can help to set `irony-priority-limit'. Works only with
detailed completion."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Never" nil))
  :group 'irony
  :group 'auto-complete)

(defface ac-irony-candidate-face
  '((((class color) (min-colors 88))
     :background "LightSteelBlue1" :foreground "dark slate gray")
    (t
     :background "blue" :foreground "white"))
  "Face for (non-selected) irony candidates in auto-complete."
  :group 'irony
  :group 'auto-complete)

(defface ac-irony-selection-face
  '((((class color) (min-colors 88))
     :background "LightSteelBlue3" :foreground "dark slate gray" :bold t)
    (t
     :background "blue" :foreground "white"))
  "Face for *selected* irony candidates in auto-complete."
  :group 'irony
  :group 'auto-complete)


;; Internal variables
;;

(defvar irony-ac-expand-snippet-function nil
  "Function to expand a snippet at a given point (by default to
  the current position).

  e.g: (defun my-expand-snippet (snippet-str &optional pos)
         (do-stuff (or pos (point)))

Will be set to nil if no snippet expansion function is found.")

(defvar ac-source-irony
  '((prefix         . irony-ac-prefix)
    (candidates     . irony-ac-candidates)
    (requires       . -1)
    (candidate-face . ac-irony-candidate-face)
    (selection-face . ac-irony-selection-face)
    (action         . irony-ac-action)
    (limit          . nil)
    (cache)))

(defun irony-ac-support-detailed-display-p ()
  "Return non-nil if the completion system can (and should)
  displayed detailed results."
  ;; ATM only my auto-complete fork support this feature. The fork
  ;; defines `ac-sarcasm-fork-version'.
  (boundp 'ac-sarcasm-fork-version))

(defun irony-ac-setup ()
  "Hook to run for `auto-complete-mode' when `irony-mode' is
activated."
  (interactive)
  (add-hook 'irony-on-completion-hook 'irony-ac-complete)
  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map [(control return)] 'ac-complete-irony))

(defun ac-complete-irony ()
  (interactive)
  (irony-trigger-completion))

(defun irony-ac-complete ()
  (ac-update t)
  (ac-start))

(defun irony-ac-yas-disabled-p ()
  "If the current yasnippet version offers a minor-mode, check if
this mode is disable by returning t, otherwise returns nil and
it's partially safe to assume that yasnippet expansion can be
used."
  ;; XXX: work only when yasnippet is enabled, otherwise
  ;;      some variables use for the snippet expansion are
  ;;      not set and it causes some errors.
  (if (boundp 'yas-minor-mode)
      (not yas-minor-mode)
    (if (boundp 'yas/minor-mode)
        (not yas/minor-mode))))

(defun irony-ac-expand-yas-1 (snippet-str &optional pos)
  "Expand snippets for YASnippet version <= 0.6.0c."
  (unless (irony-ac-yas-disabled-p)
    (yas/expand-snippet (or pos (point))
                        (or pos (point))
                        snippet-str)))

(defun irony-ac-expand-yas-2 (snippet-str &optional pos)
  "Expand snippets for YASnippet version < 0.8.

See also `irony-ac-expand-yas-1'."
  (unless (irony-ac-yas-disabled-p)
    (when pos
        (goto-char pos))
    (yas/expand-snippet snippet-str)))

(defun irony-ac-expand-yas-3 (snippet-str &optional pos)
  "Expand snippets for YASnippet version >= 0.8.

See also `irony-ac-expand-yas-2'."
  (unless (irony-ac-yas-disabled-p)
    (when pos
        (goto-char pos))
    (yas-expand-snippet snippet-str)))

(defun irony-ac-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (when (irony-ac-support-detailed-display-p)
    (add-to-list 'ac-source-irony '(allow-dups) t))
  ;; find the snippet expand function
  (when (and (not irony-ac-expand-snippet-function)
             (require 'yasnippet nil t))
    (let ((yas-version (or (and (boundp 'yas--version) yas--version)
                           (and (boundp 'yas/version) yas/version)))) ;for old versions
      (when (stringp yas-version)
        (setq yas-version (replace-regexp-in-string "(\\|)" "" yas-version))
        (setq irony-ac-expand-snippet-function
              (cond ((version<= yas-version "0.6.0c")
                     'irony-ac-expand-yas-1)
                    ;; `version<' thinks "0.8beta" < "0.8", we want to
                    ;; consider anything starting with "0.8" as "0.8"
                    ;; and more.
                    ((and (version< yas-version "0.8")
                          (not (string-prefix-p "0.8" yas-version)))
                     'irony-ac-expand-yas-2)
                    (t
                     'irony-ac-expand-yas-3))))))
  ;; Enable ac even if we are in a C string. Allow the completion to
  ;; work for the following case:
  ;;
  ;;    for #include "head[COMP]..
  ;;
  (setq ac-disable-faces (delq 'font-lock-string-face ac-disable-faces))
  (add-hook 'irony-mode-hook 'irony-ac-setup))

(defun irony-ac-disable ()
  "Disable `auto-complete-mode' handling of `irony-mode'
completion results."
  (remove-hook 'irony-mode-hook 'irony-ac-setup))

(defun irony-ac-candidates ()
  "Generate detailed candidates."
  (let ((window-width (- (window-width) (popup-current-physical-column)))
        (show-priority irony-ac-show-priority)
        candidates)
    (dolist (result (irony-last-completion-results) candidates)
      (cond
       ((listp result)
        (let ((r (car result))
              (priority (if show-priority (cdr (assq 'p (cdr result))))))
          (if (and (irony-ac-support-detailed-display-p)
                   (cdr (assq 'opt (cdr result))))
              (mapc (lambda (opt-r)
                      (setq candidates (cons
                                        (irony-ac-new-item opt-r window-width priority)
                                        candidates)))
                    ;; XXX: nreverse shouldn't be necessary, it just
                    ;;      seems to produced more please results
                    ;;      like in the order:
                    ;;          foo(int a)
                    ;;          foo(int a, int b)
                    ;;          ...
                    (nreverse (irony-ac-expand-optionals r)))
            (setq candidates (cons
                              (irony-ac-new-item r window-width priority)
                              candidates)))))
       ((stringp result)
        (setq candidates (cons result candidates)))))))

(defun irony-ac-new-item (result window-width &optional priority)
  "Return a new item of a result element.

Here is 4 differents RESULT to get an idea of the representation:

    ((\"ptrdiff_t\") (p . 50))
    ((\"basic_ios\" ?< (ph . \"typename _CharT\")
                         (opt ?, (ph . \"typename_Traits\"))
                       ?>)  (p . 50) (opt . t))
    (((r . \"bool\") \"uncaught_exception\" ?( ?)) (p . 50))
    ((\"std\" (t . \"::\")) (p . 75))

Note that a string, the typed text is mandatory, this is the
element/identifier that need to be completed.

The WINDOW-WITH is for the case the candidate string is too long,
the summary is truncated in order to not span on multiple lines.
"
  (let ((view "")
        typed-text result-type summary)
    (dolist (e result)
      (cond
       ((stringp e)
        (setq typed-text e
              view (concat view typed-text)))

       ((consp e)
        (if (eq (car e) 'r)
            (setq result-type (cdr e))
          (when (memq (car e) '(ph t i p))
            (setq view (concat view (cdr e))))))

       ((characterp e)
        (unless (eq e ?\n)              ;VIEW should be one line only
          (setq view (concat view (list e)
                             (when (eq e ?,)
                               " "))))))) ;prettify commas
    ;; Set the summary, reduce is size of summary if view + summary
    ;; are longer than the window-width and the summary is too long
    ;; (view is automatically truncated by the popup library).
    (when (or result-type priority)
      (let ((result-type-width (string-width (or result-type "")))
            (max-result-type-width (/ window-width 3)))
        (when (and (> result-type-width  ;result-type too long
                      max-result-type-width)
                   (< window-width         ;not enough space
                      (+ (string-width view) result-type-width 2)))
          (setq result-type (concat (substring result-type
                                               0
                                               (- max-result-type-width 3))
                                    "..."))))
      (setq summary (cond
                     ((and priority result-type)
                      (format "[%s]%3d" result-type priority))
                     (result-type
                      (format "[%s]" result-type))
                     (t
                      (format ":%3d" priority)))))
    (popup-make-item typed-text :view view :value result :summary summary)))

(defun irony-ac-expand-optionals (data)
  (let ((results (list nil)))
    (dolist (cell data)
      (cond
       ((and (consp cell)
             (eq (car cell) 'opt))
        (let (new-results)
          (dolist (opt-chunks (irony-ac-expand-optionals (cdr cell)))
            (let ((new-elems (mapcar (lambda (r)
                                       (nconc (nreverse opt-chunks) r))
                                     (copy-tree results))))
              (if new-results
                  (nconc new-results new-elems)
                (setq new-results new-elems))))
          (if results
              (nconc results new-results)
            (setq results new-results))))

       (t
        ;; Add the cell to each results
        (setq results (mapcar (lambda (res)
                                (cons cell res))
                              results)))))
    (mapcar (lambda (res)
              (nreverse res))
            results)))

(defun irony-ac-dynamic-snippet (completion-result)
  "Return a cons of the for (SNIPPET-STR . HAS-PLACEHOLDER-P)
where SNIPPET-STR is a string as follow:

 - \"()\"
 - \"(${1:int x}, ${2:int y})$0\"

if HAS-PLACEHOLDER-P is nil then it's not a snippet and doesn't
need special expansion, otherwise it's a snippet and it will end
with the $0 string to the represent the final position after
expansion.

See `irony-ac-new-item' for a description of COMPLETION-RESULT
format."
  (let ((result (popup-item-value completion-result))
        (dynamic-snippet "")
        (num-placeholders 0))
    ;; skip after the typed text
    (while (not (stringp (car result)))
      (setq result (cdr result)))
    ;; build snippet
    (dolist (e result)
      (if (characterp e)
          (setq dynamic-snippet (concat dynamic-snippet (list e)
                                        (when (eq e ?,) ;prettify commas
                                          " ")))
        (when (consp e)
          (cond
           ((or (eq (car e) 't)
                (eq (car e) 'p))    ;TODO: find a way to test this one
            (setq dynamic-snippet (concat dynamic-snippet (cdr e))))

           ((eq (car e) 'ph)
            (incf num-placeholders)
            (setq dynamic-snippet
                  (concat dynamic-snippet
                          (format "${%d:%s}" num-placeholders (cdr e)))))))))
    (unless (zerop num-placeholders)
      (setq dynamic-snippet (concat dynamic-snippet "$0")))
    (cons dynamic-snippet
          (> num-placeholders 0))))

(defun irony-ac-action-detailed ()
  "Action to execute after a detailed completion is done."
  (let ((dyn-snip (irony-ac-dynamic-snippet (cdr ac-last-completion))))
    (if (cdr dyn-snip)                ;has placeholder(s)?
        (when irony-ac-expand-snippet-function
          (funcall irony-ac-expand-snippet-function (car dyn-snip)))
      ;; no placeholder, just insert the string
      (insert (car dyn-snip)))))

(defun irony-ac-header-comp-action ()
  "After the completion is complete, add the closing
character (double quote or angle-bracket) if needed."
  ;; do not add closing '>' or '"' when the completed item was a
  ;; directory.
  (if (string-match-p "/$" (cdr ac-last-completion))
      (irony-trigger-completion-maybe)
    (let ((ch (char-after)))
      (when (not (or (eq ch ?\")
                     (eq ch ?>)))
        (let ((line (buffer-substring-no-properties (point-at-bol) (point))))
          (when (string-match "#\\s-*include\\s-+\\([<\"]\\)" line)
            (insert (if (eq (string-to-char (match-string 1 line)) ?<)
                        ">"
                      "\""))))))))

(defun irony-ac-action ()
  "Action to execute after a completion is done."
  (if (irony-header-comp-inside-include-stmt-p)
      (irony-ac-header-comp-action)
    (when (and (not irony-complete-typed-text-only)
               (irony-ac-support-detailed-display-p))
      (irony-ac-action-detailed))))

(defun irony-ac-prefix ()
  "Return the point of completion either for a header or a
standard identifier."
  (irony-get-last-completion-point))

  ;; (or (irony-header-comp-point)
  ;;     (irony-get-completion-point)))

(provide 'irony/ac)

;; Local variables:
;; generated-autoload-load-name: "irony/ac"
;; End:

;;; irony/ac.el ends here
