;;; irony-ac-simple.el --- Irony completion with auto-complete

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

;; `irony-mode' sources for `auto-complete'. The completion strings
;; provided are simple (use the completion provided by
;; `irony-complete', see function documentation for more
;; information).

;;; Usage:
;;      (add-to-list 'load-path "/path/to/irony-ac-simple-directory")
;;      (irony-load '(ac-simple))
;;
;; note: `ac-complete-irony-simple' to trigger the completion on
;; demand (binded to C-RET by default when `irony-ac-simple-setup' is
;; used)

;;; Code:

(require 'irony)
(require 'auto-complete)

(defface irony-ac-simple-candidate-face
  '((((class color) (min-colors 88))
     :background "LightSteelBlue1" :foreground "dark slate gray")
    (t
     :background "blue" :foreground "white"))
  "Face for (non-selected) irony candidates in auto-complete."
  :group 'irony
  :group 'auto-complete)

(defface irony-ac-simple-selection-face
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

(defconst ac-source-irony-simple
  '((candidates     . (irony-ac-simple-candidates ac-point))
    (prefix         . irony-get-completion-point)
    (requires       . 0)
    (candidate-face . irony-ac-simple-candidate-face)
    (selection-face . irony-ac-simple-selection-face)
    (cache))
  "Auto-complete source for `irony-mode'.")

(defconst ac-source-irony-simple-anywhere
  '((candidates     . (irony-ac-simple-candidates ac-point))
    (prefix         . irony-get-completion-point-anywhere)
    (requires       . 0)
    (candidate-face . irony-ac-simple-candidate-face)
    (selection-face . irony-ac-simple-selection-face)
    (cache))
  "Auto-complete source for `irony-mode'.")

(defun irony-ac-simple-setup ()
  "Hook to run for `auto-complete-mode' when `irony-mode' is
activated."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-irony-simple)
  (define-key irony-mode-map [(control return)] 'ac-complete-irony-simple))

(defun irony-ac-simple-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (add-hook 'irony-mode-hook 'irony-ac-simple-setup))

(defun irony-ac-simple-disable ()
  "Disable `auto-complete-mode' handling of `irony-mode'
completion results."
  (remove-hook 'irony-mode-hook 'irony-ac-simple-setup))

(defun ac-complete-irony-simple ()
  "Display available completions on demand."
  (interactive)
  (auto-complete '(ac-source-irony-simple)))

(defun irony-ac-simple-candidates (pos)
  "Generate candidates for `auto-complete' (a list of strings).
POS is the position of the beginning of the completion in the
current buffer."
  (delete-dups
   (mapcar (lambda (result)
             (plist-get result :result))
           (irony-complete :simple pos))))

(provide 'irony-ac-simple)
;;; irony-ac-simple.el ends here
