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
(require 'irony-header-comp)
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

;; XXX: Not sure how to make this option for the moment. I think AC
;;      caches the sources and it might be a bad idea to change the
;;      definition of a source dynamically.
;;
;; (defcustom irony-ac-prefer-simple-results nil
;;   "Non-nil means that, even if detailed completion is available
;;   the plugin should stick to simple candidates display.
;;
;; Note: ATM it is not possible to change this value dynamically, it
;; should be set before the plugin is loaded."
;;   :type '(choice (const :tag "Yes" t)
;;                  (const :tag "No" nil))
;;   :group 'irony
;;   :group 'auto-complete)

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

(defconst irony-ac-symbol-to-str-alist
  '((:left-paren       . "(")
    (:right-paren      . ")")
    (:left-bracket     . "[")
    (:right-bracket    . "]")
    (:left-brace       . "{")
    (:right-brace      . "}")
    (:left-angle       . "<")
    (:right-angle      . ">")
    (:comma            . ", ")          ;prettier like this
    (:colon            . ":")
    (:semi-colon       . ";")
    (:equal            . "=")
    (:horizontal-space . " ")
    (:vertical-space   . "\n"))
  "Alist of symbol names and their string representation.")

(defconst ac-source-irony-base
  '((candidates     . (irony-ac-candidates ac-point))
    (requires       . 0)
    (candidate-face . ac-irony-candidate-face)
    (selection-face . ac-irony-selection-face)
    (action         . irony-ac-action)
    (limit          . nil)
    (cache))
  "Auto-complete source for `irony-mode'.")

(defvar ac-source-irony (cons '(prefix . irony-ac-completion-point)
                              ac-source-irony-base)
  "Auto-complete source for `irony-mode'.")

(defvar ac-source-irony-anywhere (cons '(prefix . irony-ac-completion-point-anywhere)
                                       ac-source-irony-base)
  "Auto-complete source for `irony-mode'.")

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
  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map [(control return)] 'ac-complete-irony))

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
    (add-to-list 'ac-source-irony '(allow-dups) t)
    (add-to-list 'ac-source-irony-anywhere '(allow-dups) t))
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

(defun ac-complete-irony ()
  "Display available completions on demand."
  (interactive)
  (auto-complete '(ac-source-irony-anywhere)))

(defun irony-ac-simple-candidates (pos)
  "Generate a list of candidates identifier."
  (delete-dups (mapcar (lambda (result)
                         (plist-get result :result))
                       (irony-complete :simple pos))))

(defun irony-ac-detailed-candidates (pos)
  "Generate detailed candidates."
  (loop with window-width = (- (window-width) (popup-current-physical-column))
        with show-priority = irony-ac-show-priority
        for result-plist in (irony-complete :detailed pos)
        for result = (plist-get result-plist :result)
        for priority = (if show-priority (plist-get result-plist :priority))
        if (plist-get result-plist :optional)
        append (mapcar (lambda (r) (irony-ac-new-item r window-width priority))
                       (irony-ac-expand-optionals result))
        into candidates
        else collect (irony-ac-new-item result window-width priority)
        into candidates
        finally return candidates))

(defun irony-ac-candidates (pos)
  "Generate candidates for `auto-complete' (a list of strings).
POS is the position of the beginning of the completion in the
current buffer."
  (if (irony-header-comp-inside-include-stmt-p)
      (irony-header-comp-complete-at pos)
    (if (irony-ac-support-detailed-display-p)
        (irony-ac-detailed-candidates pos)
      (irony-ac-simple-candidates pos))))

(defun irony-ac-new-item (result window-width &optional priority)
  "Return a new item of a result element. RESULT has the
following form (:typed-text is mandatory to each item, this is
the element that need to be completed):

       ((:result-type  . \"bool\")
        (:typed-text   . \"getFoo\")
        (:symbol       . :left-paren)
        (:symbol       . :right-paren))

The WINDOW-WITH is for the case the candidate string is too long,
the summary is truncated in order to not span on multiple lines."
  (let (typed-text result-type summary
                   (view ""))
    (dolist (cell result)
      (let ((identifier (car cell))
            (value (cdr cell)))
        (when (eq identifier :typed-text)
          (setq typed-text value))
        (if (eq identifier :result-type)
            (setq result-type value)
          (setq view (concat view
                             (cond
                              ((eq identifier :symbol)
                               (unless (eq value :vertical-space) ;view should be one-line
                                 (cdr (assq value irony-ac-symbol-to-str-alist))))
                              (t
                               value))))))) ;!dolist
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
  "Expand a DATA into a list of results, where optional chunks
are expanded."
  (let ((results (list nil)))
    (dolist (cell data)
      (cond
       ((eq (car cell) :optional)
        (let (new-results)
          (dolist (opt-chunks (irony-ac-expand-optionals (plist-get (cdr cell) :result)))
            (let* ((res (copy-tree results)) ;FIXME: copy alist ?
                   (new-elems (mapcar (lambda (r)
                                        (nconc (nreverse opt-chunks) r))
                                      res)))
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
where SNIPPET-STR is a string of the form:

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
    (while (not (eq (caar result) :typed-text))
      (setq result (cdr result)))
    ;; build snippet
    (dolist (cell result)
      (let ((identifier (car cell))
            (value (cdr cell)))
        (setq dynamic-snippet
              (concat dynamic-snippet
                      (cond
                       ((eq identifier :symbol)
                        (cdr (assq value irony-ac-symbol-to-str-alist)))
                       ((or (eq identifier :place-holder)
                            (eq identifier :current-place-holder)) ;FIXME: "can't test"
                        (setq num-placeholders (1+ num-placeholders))
                        (format "${%d:%s}" num-placeholders value))
                       ((eq identifier :text)
                        value))))))     ;!dolist
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
      (ac-start)
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
    (when (irony-ac-support-detailed-display-p)
      (irony-ac-action-detailed))))

(defun irony-ac-completion-point ()
  "Return the point of completion either for a header or a
standard identifier."
  (or (irony-header-comp-point)
      (irony-get-completion-point)))

(defun irony-ac-completion-point-anywhere ()
  "See `irony-ac-completion-point-anywhere'."
  (or (irony-header-comp-point)
      (irony-get-completion-point-anywhere)))

(provide 'irony/ac)

;; Local variables:
;; generated-autoload-load-name: "irony/ac"
;; End:

;;; irony/ac.el ends here
