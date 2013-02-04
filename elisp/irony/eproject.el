;;; irony/eproject.el --- `eproject-mode' integration in `irony-mode'.

;; Copyright (C) 2011-2013  Guillaume Papin

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

;;; Usage:
;;      (irony-enable 'eproject)

;;; Code:

(require 'irony)
(require 'eproject)

(defun irony-eproject-setup ()
  "Hook to run for `eproject-mode' when `irony-mode' is
activated."
  (eproject-maybe-turn-on)
  (when eproject-mode
    (setq irony-header-directories-root 'eproject-root
          irony-header-directories (lambda ()
                                     (eproject-attribute :includes))
          irony-config-commands (lambda ()
                                  (eproject-attribute :config-commands))
          irony-extra-flags  (lambda ()
                               (eproject-attribute :extra-flags)))))

(defun irony-eproject-enable ()
  "Enable eproject settings for `irony-mode'."
  (add-hook 'irony-mode-hook 'irony-eproject-setup))

(defun irony-eproject-disable ()
  "Disable eproject settings for `irony-mode'."
  (remove-hook 'irony-mode-hook 'irony-eproject-setup))

(provide 'irony/eproject)

;; Local variables:
;; generated-autoload-load-name: "irony/eproject"
;; End:

;;; irony/eproject.el ends here
