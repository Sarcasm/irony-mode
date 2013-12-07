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
  (add-hook 'irony-on-completion-hook 'irony-ac-complete)
  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map [(control return)] 'ac-complete-irony))

(defun ac-complete-irony ()
  (interactive)
  (irony-trigger-completion))

(defun irony-ac-complete ()
  (ac-update t)
  (ac-start))

(defun irony-ac-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (when (irony-ac-support-detailed-display-p)
    (add-to-list 'ac-source-irony '(allow-dups) t))
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

(defun irony-ac-detailed-candidates (results)
  (let ((window-width (- (window-width) (popup-current-physical-column)))
        (show-priority irony-ac-show-priority)
        candidates)
    (dolist (result results candidates)
      (let ((r (car result))
            (priority (if show-priority (cdr (assq 'p (cdr result)))))
            (brief (cdr (assq 'b (cdr result)))))
        (if (cdr (assq 'opt (cdr result)))
            (mapc (lambda (opt-r)
                    (setq candidates (cons
                                      (irony-ac-new-item opt-r window-width priority brief)
                                      candidates)))
                  ;; XXX: nreverse shouldn't be necessary, it just
                  ;;      seems to produced more please results
                  ;;      like in the order:
                  ;;          foo(int a)
                  ;;          foo(int a, int b)
                  ;;          ...
                  (nreverse (irony-ac-expand-optionals r)))
          (setq candidates (cons
                            (irony-ac-new-item r window-width priority brief)
                            candidates)))))))

(defun irony-ac-make-simplified-candidate (result)
  ;; first move the pointer to the typed-text
  (let ((typed-text (car result))
        (brief (cdr (assq 'b (cdr result))))
        result-type)
    (setq result (car result))         ;ignore priority
    ;; find typed-text (and result optionaly)
    (while (and typed-text (not (stringp (car typed-text))))
      (let ((elem (car typed-text)))
        (when (and (consp elem) (eq (car elem) 'r))
          (setq result-type (cdr elem))))
      (setq typed-text (cdr typed-text)))
    ;; make the item
    (when typed-text
      (setq result-type (if result-type (format "[%s]" result-type)))
      (popup-make-item (car typed-text) :value result :summary result-type
                       :document brief))))

(defun irony-ac-simplified-candidates (results)
  (mapcar 'irony-ac-make-simplified-candidate results))

(defun irony-ac-candidates ()
  "Generate detailed candidates."
  (let ((results (irony-last-completion-results)))
    (cond
     ((stringp (car results))
      results)

     ((consp (car results))
      (if (irony-ac-support-detailed-display-p)
          (irony-ac-detailed-candidates results)
        (irony-ac-simplified-candidates results))))))

(defun irony-ac-new-item (result window-width &optional priority brief)
  "Return a new item of a result element.

Here is 4 differents RESULT to get an idea of the representation:

    ((\"ptrdiff_t\") (p . 50))
    ((\"basic_ios\" ?< (ph . \"typename _CharT\")
                         (opt ?, (ph . \"typename_Traits\"))
                       ?>)  (p . 50) (opt . t))
    (((r . \"bool\") \"uncaught_exception\" ?( ?)) (p . 50))
    ((\"std\" (t . \"::\")) (p . 75))

The WINDOW-WITH is for the case the candidate string is too long,
the summary is truncated in order to not span on multiple lines.
"
  (let ((typed-text "")
        (view "")
        result-type summary)
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
    (popup-make-item typed-text :view view :value result :summary summary
                     :document brief)))

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

(defun irony-ac-action ()
  "Action to execute after a completion is done."
  (let ((last-comp (cdr ac-last-completion)))
    (irony-post-completion-action (when last-comp
                                    (or (popup-item-value last-comp)
                                        last-comp)))))

(defun irony-ac-prefix ()
  "Return the point of completion either for a header or a
standard identifier."
  (irony-get-last-completion-point))

(provide 'irony/ac)

;; Local variables:
;; generated-autoload-load-name: "irony/ac"
;; End:

;;; irony/ac.el ends here
