;;; irony-ac.el --- Irony completion with auto-complete

;; Copyright (C) 2011  Guillaume Papin

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
;; `irony-complete-detailed', see function documentation for more
;; information).
;;
;; The functionalities provided are:
;; - functions with optional parameters are distinguished in order to
;;   choose the correct function directly.
;; - Snippet (with `yasnippet') expansion are made after the
;;   completion is performed

;;; Usage:
;;      (add-to-list 'load-path "/path/to/irony-ac-directory")
;;      (irony-load '(ac))
;;
;; note: `ac-complete-irony' to trigger the completion on demand
;; (binded to C-RET by default when `irony-ac-setup' is used)

;;; Code:

(require 'irony)
(require 'auto-complete)
(require 'popup)
(require 'yasnippet)

(defcustom irony-ac-show-priority nil
  "Non-nil means the priority of the result will be shown in the
completion menu. This can help to set `irony-priority-limit'."
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

;;
;; Internal variables
;;

(defconst irony-symbol-to-str-alist
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
  "Alist of symbols and their string representation.")

;; TODO: factoring the `ac-source-irony' and
;; `ac-source-irony-anywhere'
;; FIXME: find how to check the availability of the completion
;; (available . irony-mode-on-p) ?
(defconst ac-source-irony
  '((candidates     . (irony-ac-candidates ac-point))
    (prefix         . irony-get-completion-point)
    (requires       . 0)
    (candidate-face . ac-irony-candidate-face)
    (selection-face . ac-irony-selection-face)
    (action         . irony-ac-action)
    (allow-dups)
    (cache))
  "Auto-complete source for `irony-mode'.")

(defconst ac-source-irony-anywhere
  '((candidates     . (irony-ac-candidates ac-point))
    (prefix         . irony-get-completion-point-anywhere)
    (requires       . 0)
    (candidate-face . ac-irony-candidate-face)
    (selection-face . ac-irony-selection-face)
    (action         . irony-ac-action)
    (allow-dups)
    (cache))
  "Auto-complete source for `irony-mode'.")

(defun irony-ac-setup ()
  "Hook to run for `auto-complete-mode' when `irony-mode' is
activated."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map [(control return)] 'ac-complete-irony))

(defun irony-ac-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (add-hook 'irony-mode-hook 'irony-ac-setup))

(defun irony-ac-disable ()
  "Disable `auto-complete-mode' handling of `irony-mode'
completion results."
  (remove-hook 'irony-mode-hook 'irony-ac-setup))

(defun ac-complete-irony ()
  "Display available completions on demand."
  (interactive)
  (auto-complete '(ac-source-irony-anywhere)))

(defun irony-ac-candidates (pos)
  "Generate candidates for `auto-complete' (a list of strings).
POS is the position of the beginning of the completion in the
current buffer."
  ;; FIXME: should results be sorted by priority ?
  (loop with window-width = (- (window-width) (popup-current-physical-column))
        with show-priority = irony-ac-show-priority
        for result-plist in (irony-complete-detailed pos)
        for result = (plist-get result-plist :result)
        for priority = (if show-priority (plist-get result-plist :priority))
        if (plist-get result-plist :optional)
        append (mapcar (lambda (r) (irony-new-item r window-width priority))
                       (irony-expand-optionals result))
        into candidates
        else collect (irony-new-item result window-width priority)
        into candidates
        finally return candidates))

(defun irony-new-item (result window-width &optional priority)
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
                                 (cdr (assq value irony-symbol-to-str-alist))))
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

(defun irony-expand-optionals (data)
  "Expand a DATA into a list of results, where optional chunks
are expanded."
  (let ((results (list nil)))
    (dolist (cell data)
      (cond
       ((eq (car cell) :optional)
        (let (new-results)
          (dolist (opt-chunks (irony-expand-optionals (plist-get (cdr cell) :result)))
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

(defun irony-ac-action ()
  "Action to execute after a completion was done."
  (let ((result (popup-item-value (cdr ac-last-completion)))
        (text "")
        (snippet 0))
    ;; skip after the typed text
    (while (not (eq (caar result) :typed-text))
      (setq result (cdr result)))
    (dolist (cell result)
      (let ((identifier (car cell))
            (value (cdr cell)))
        (setq text
              (concat text (cond
                            ((eq identifier :symbol)
                             (cdr (assq value irony-symbol-to-str-alist)))
                            ((or (eq identifier :place-holder)
                                 (eq identifier :current-place-holder)) ;FIXME: "couldn't test"
                             (setq snippet (1+ snippet))
                             (format "${%d:%s}" snippet value))
                            ((eq identifier :text)
                             value)))))) ;!dolist
    (if (eq snippet 0)
        (insert text)
      (yas/expand-snippet (concat text "$0")))))

(provide 'irony-ac)
;;; irony-ac.el ends here
