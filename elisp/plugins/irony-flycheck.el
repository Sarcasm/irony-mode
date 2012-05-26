;;; irony-flycheck.el --- Syntax checking on the Fly a la flymake with Irony mode.

;; Copyright (C) 2011  Guillaume Papin

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

;; Syntax checking à la `flymake-mode'.

;;; Usage:
;; TODO: ...

;;; Code:

(require 'irony-syntax-check)

;;
;; Public / customizable variables
;;

(defcustom irony-flycheck-idle-time 1
  "Number of seconds in idle time before a syntax checking
request should be made.

By default after 1 second of 'idling' a syntax check is made."
  :type 'number
  :group 'irony)

(defface irony-flycheck-error
  '((((class color) (background dark)) (:foreground "white smoke" :underline "dark red"))
    ;; (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'flymake)

(defface irony-flycheck-warning
  '((((class color) (background dark)) (:underline "#4477aa"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'flymake)

;; (defface irony-note
;;   '((((class color) (background dark)) (:background "DarkBlue"))
;;     (((class color) (background light)) (:background "LightBlue2"))
;;     (t (:bold t)))
;;   "Face used for marking note lines."
;;   :group 'flymake)

(defcustom irony-flycheck-overlay-priority 1000
  "Priority of `irony-check-mode' highlighting overlays."
  :type 'integer
  :group 'irony)

;;
;; Private variables
;;

(defvar irony-flycheck-timer nil
  "Internal timer used by `irony-flycheck' to run a syntax check
  when Emacs is idle for `irony-flycheck-idle-time' seconds.")
(make-variable-buffer-local 'irony-flycheck-timer)

;;
;; Functions
;;

(defun irony-flycheck-setup ()
  "Hook to run for `auto-complete-mode' when `irony-mode' is
activated."
  (unless irony-flycheck-timer
    (setq irony-flycheck-timer
          (run-with-idle-timer irony-flycheck-idle-time t 'irony-flycheck-on-timer-event))
    (add-hook 'irony-cancel-process-hook 'irony-flycheck-cancel-timer)
    (define-key irony-mode-map [(control return)] 'ac-complete-irony)))

(defun irony-flycheck-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (add-hook 'irony-mode-hook 'irony-flycheck-setup))

(defun irony-flycheck-disable ()
  "Disable `auto-complete-mode' handling of `irony-mode'
completion results."
  (remove-hook 'irony-mode-hook 'irony-flycheck-setup))

(defun irony-flycheck-cancel-timer ()
  "Cancel the internal idle timer set it's value to nil."
  (when irony-flycheck-timer
    (cancel-timer irony-flycheck-timer)
    (setq irony-flycheck-timer nil)))

(defun irony-flycheck-on-timer-event ()
  "Call when `irony-flycheck-timer' (idle) seconds have passed."
  (when irony-mode
    (message "Syntax checking...")
    (let* ((syntax-check-data (irony-syntax-check))
           (diagnostics (plist-get syntax-check-data :diagnostics)))
      (irony-flycheck-remove-overlays)
      (when diagnostics
        (irony-flycheck-generate-overlays diagnostics)))))

(defun irony-flycheck-remove-overlays ()
  "Remove overlays produced by `irony-flycheck'."
  (remove-overlays nil nil 'irony-flycheck t))

(defun irony-flycheck-generate-overlays (diagnostics)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; note (elisp info node *Managing Overlays*):
      ;; A loop that scans the buffer forwards, creating overlays, can
      ;; run faster if you do `(overlay-recenter (point-max))' first.
      (overlay-recenter (point-max))
      (mapc 'irony-flycheck-make-diagnostic-overlay diagnostics))))

(defun irony-flycheck-make-diagnostic-overlay (diagnostic)
  (let ((severity   (plist-get diagnostic :severity))
        (diagnostic (plist-get diagnostic :diagnostic))
        (location   (plist-get diagnostic :location))
        (ranges     (plist-get diagnostic :ranges))
        (notes      (plist-get diagnostic :notes))
        (fix-its    (plist-get diagnostic :fix-its))
        ;; (flags      (plist-get diagnostic :flags))
        )
    (when location
      (if (string= (irony-temp-filename) (car location))
          (irony-flycheck-make-local-diagnostic (current-buffer)
                                                severity
                                                diagnostic
                                                (cdr location)
                                                ranges
                                                notes
                                                fix-its)
        (message "other buffer syntax error (maybe a header ?)")))))

(defun irony-flycheck-make-local-diagnostic (buffer severity
                                                    diagnostic
                                                    location
                                                    ranges notes
                                                    fix-its)
  ;; :severity :error
  ;; :location (/tmp/!tmp!lol.cpp 91 (5 . 36))
  ;; :diagnostic "expected ';' after expression"
  ;; :flags ( . )
  ;; :ranges (((/tmp/!tmp!lol.cpp 82 (5 . 27)) /tmp/!tmp!lol.cpp 87 (5 . 32)))
  ;; :fix-its ((";" (/tmp/!tmp!lol.cpp 91 (5 . 36)) /tmp/!tmp!lol.cpp 91 (5 . 36)))
  ;; :notes nil
  (let* ((diag-offset (car location))
         (overlay (make-overlay (+ diag-offset 1) (+ diag-offset 2) buffer t t)))
    (message "Woot: %s %s"  (1- diag-offset) (1+ diag-offset))
    (overlay-put overlay 'irony-flycheck t)
    ;; (overlay-put overlay 'category '(CATEGORIE_PROPERTIES))
    ;; (overlay-put overlay 'face 'irony-flycheck-error)
    (overlay-put overlay 'face 'show-paren-match)
    (overlay-put overlay 'priority irony-flycheck-overlay-priority)
    ;; (overlay-put overlay 'mouse-face 'highlight)
    ;; (overlay-put overlay 'rfclink-section (match-string 1))
    ;; (overlay-put overlay 'help-echo (format "This word has a size of %d." rfclink-word-size))
    ;; (overlay-put overlay 'before-string " > ")
    ;; (overlay-put overlay 'after-string " < ")
    ;; (overlay-put overlay 'invisible t)
    ;; (overlay-put overlay 'local-map rfclink-mode-overlay-keymap)
    ))

(provide 'irony-flycheck)
;;; irony-flycheck.el ends here
