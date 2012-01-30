;;; irony-ac-header-comp.el --- C/C++ #include completion

;; Copyright (C) 2012  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: c

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

;;

;;; Code:

(require 'irony-header-comp)
(require 'auto-complete)

(defface irony-ac-header-comp-candidate-face
  '((((class color) (min-colors 88))
     :background "LightSteelBlue1" :foreground "dark slate gray")
    (t
     :background "blue" :foreground "white"))
  "Face for (non-selected) irony candidates in auto-complete."
  :group 'irony
  :group 'auto-complete)

(defface irony-ac-header-comp-selection-face
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

(defconst ac-source-irony-header-comp
  '((candidates     . (irony-header-comp-complete-at ac-point))
    (prefix         . irony-header-comp-point)
    (requires       . 0)
    (candidate-face . irony-ac-header-comp-candidate-face)
    (selection-face . irony-ac-header-comp-selection-face)
    (cache))
  "Auto-complete source for `irony-mode'.")

(defun irony-ac-header-comp-setup ()
  "Hook to run for `auto-complete-mode' when `irony-mode' is
activated."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-irony-header-comp)
  (define-key irony-mode-map [(control return)] 'ac-complete-irony-header-comp))

(defun irony-ac-header-comp-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (setq ac-disable-faces (delq 'font-lock-string-face ac-disable-faces))
  (add-hook 'irony-mode-hook 'irony-ac-header-comp-setup))

(defun irony-ac-header-comp-disable ()
  "Disable `auto-complete-mode' handling of `irony-mode'
completion results."
  (remove-hook 'irony-mode-hook 'irony-ac-header-comp-setup))

(defun ac-complete-irony-header-comp ()
  "Display available completions on demand."
  (interactive)
  (auto-complete '(ac-source-irony-header-comp)))

(provide 'irony-ac-header-comp)
;;; irony-ac-header-comp.el ends here
