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

;; TODO: explain the mode

;;; Usage:
;; TODO: ...

;;; Code:

(defcustom irony-flycheck-idle-time 1
  "Number of seconds in idle time before a syntax checking
request should be made.

By default after 1 second of 'idling' a syntax check is made."
  :type 'number
  :group 'irony)

(defface irony-error-face
  '((((class color) (background dark)) (:foreground "white smoke" :underline "dark red"))
    ;; (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'flymake)

(defface irony-warning-face
  '((((class color) (background dark)) (:underline "#4477aa"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'flymake)

;; (defface irony-note-face
;;   '((((class color) (background dark)) (:background "DarkBlue"))
;;     (((class color) (background light)) (:background "LightBlue2"))
;;     (t (:bold t)))
;;   "Face used for marking warning lines."
;;   :group 'flymake)

;; Private variables
(defvar irony-flycheck-timer nil
  "Internal timer used by `irony-flycheck' to run a syntax check
  when Emacs is idle for `irony-flycheck-idle-time' seconds.")
(make-variable-buffer-local 'irony-flycheck-timer)

(defvar irony-flycheck-watched-buffer nil
  "")
(make-variable-buffer-local 'irony-flycheck-watched-buffer)

(defun irony-flycheck-hook ()
  "Hook to run for `auto-complete-mode' when `irony-mode' is
activated."
  (setq irony-flycheck-timer
        (run-with-idle-timer irony-flycheck-idle-time t (lambda ()
                                                          (message "foo"))))
  ;; (add-hook 'kill-buffer-hook CANCEL LE TIMER)

  (define-key irony-mode-map [(control return)] 'ac-complete-irony))

(defun irony-flycheck-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (add-hook 'irony-mode-hook 'irony-flycheck-hook))

(defun irony-flycheck-disable ()
  "Disable `auto-complete-mode' handling of `irony-mode'
completion results."
  (remove-hook 'irony-mode-hook 'irony-flycheck-hook))

(provide 'irony-flycheck)
;;; irony-flycheck.el ends here
