;;; irony-cdb.el --- `irony-mode` compilation database

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

;;; Code:

(require 'irony)

(require 'cl-lib)


;;
;; Internal variables
;;

;; TODO: doc
;; Modeled after company-mode backends.
(defvar irony-cdb--compilation-databases '(irony-cdb-customize
                                           irony-cdb-clang-complete))


;;
;; CDB
;;
;; TODO: add support accross sessions by restoring/saving ok values in
;; ~/.emacs.d/irony/cdb.el add a per-compilation db a load-cache/save-cache /
;; serialization methods
;; see ido-{save,wash,load}-history
;;
;; Note: entries that look for a specific file, such as .clang_complete,
;; compile_commands.json, ...should preferrably use `locate-dominating-file' as
;; it may be configured by the user to do the right thing, see
;; `locate-dominating-stop-dir-regexp' for example.

;;;###autoload
(defun irony-cdb-load-compile-options ()
  (catch 'found
    (mapc #'(lambda (compilation-database)
              (irony--awhen (funcall compilation-database 'autoload)
                (irony-update-command-line-options (car it) (cdr it))
                (throw 'found t)))
          irony-cdb--compilation-databases)))

;;;###autoload
(defun irony-cdb-menu ()
  "Open the compilation database menu."
  (interactive)
  (let* ((items (irony-cdb--menu-entries))
         (items-str (mapcar 'irony-cdb-menu-make-item-str items))
         (keys (irony-cdb-menu-list-keys items))
         k cmd)
    (save-excursion
      (save-window-excursion
        (delete-other-windows)
        (let ((buffer (get-buffer-create "*Irony/Compilation DB Menu*")))
          (with-current-buffer buffer
            (erase-buffer)
            (mapc (lambda (str)
                    (insert (concat str "\n")))
                  items-str)
            (insert "\n[q] to quit"))
          (let ((pop-up-windows t))
            (display-buffer buffer t))
          (fit-window-to-buffer (get-buffer-window buffer))
          (let ((chars (sort (cons ?q (mapcar 'car keys)) '<)))
            (setq k (irony--read-char-choice "Select Compilation DB" chars))))))
    (message "") ;; clear `read-char-choice' prompt
    (unless (eq ?q k)
      (setq cmd (cdr (assoc k keys))))
    (when cmd
      (apply 'funcall cmd))))

(defun irony-cdb--menu-entries ()
  "Generate the menu items for the current buffer."
  (mapcar #'(lambda (compilation-database)
              (funcall compilation-database 'menu-entry))
          irony-cdb--compilation-databases))

(defun irony-cdb-menu-make-item-str (item)
  (let ((keys (plist-get item :keys))
        (desc (plist-get item :desc))
        (disabled (plist-get item :disabled))
        (info (plist-get item :info)))
    ;; more than 3 keys will mess up the alignment, see the format string below
    (when (> (length keys) 3)
      (error "Irony-CDB: too many keys for one menu item"))
    (when (> (length desc) 70)
      (error "Irony-CDB: description too long for a menu item"))
    (mapc (lambda (key)
            (if (member (car key) '(?q ?Q))
                (error "Irony-CDB: reserved key detected in menu item")))
          keys)
    (let ((item-str
           (format "%-7s %s"
                   (format "[%s]"
                           (mapconcat 'identity
                                      (mapcar (lambda (k)
                                                (char-to-string (car k)))
                                              keys)
                                      "/"))
                   desc)))
      (when disabled
        (setq item-str (propertize item-str 'face 'shadow)))
      (when info
        (setq item-str (propertize item-str 'help-echo info)))
      item-str)))

(defun irony-cdb-menu-list-keys (items)
  "Return an assoc list of key . action for the active menu items."
  (cl-loop for item in items
           unless (plist-get item :disabled)
           append (plist-get item :keys) into keys
           finally return keys))


;;
;; Utility functions
;;

(defun irony-cdb--truncate-path (path occupied-len)
  "Shorten and truncate if necessary PATH to fit in the window.

Assumes a fixed size window of ~80 colums.
Keep some space to show the keys.
Removes OCCUPIED-LEN from the "
  (let ((p (irony--shorten-path path))
        ;;limit to 70 columns (leaving some space for the keys)
        (max-col 70))
    (if (< (+ (length p) occupied-len) max-col)
        p
      (concat "..." (substring p (- (- max-col 3)))))))


;;
;; Preferences
;;
(defun irony-cdb-customize (command &rest args)
  ;; Modeled after company-mode backends.
  (cl-case command
    ;; (autoload (cons '("-std=c++11") nil))
    (menu-entry
     (list :desc "Preferences"
           :keys '((?p customize-group "irony"))))))


;;
;; .clang_complete
;;

(defvar irony-cdb--clang-complete-cached-directories nil
  "List of already loaded .clang_complete files.")

(defun irony-cdb-clang-complete (command &rest args)
  (cl-case command
    (menu-entry (irony-cdb--clang-complete-entry))
    (autoload (irony-cdb--try-load-clang-complete))))

(defun irony-cdb--clang-complete-entry ()
  (let ((cc-file (when buffer-file-name
                   (irony--awhen (locate-dominating-file buffer-file-name
                                                         ".clang_complete")
                     (concat (file-name-as-directory it) ".clang_complete")))))
    (if cc-file
        (list
         :desc (format "Load %s"
                       (irony-cdb--truncate-path cc-file
                                                 (length "Load ")))
         :keys `((?l irony-cdb--clang-complete-load-file ,cc-file)))
      (list
       :desc "Load .clang_complete"
       ;; ugly, but show the key even if disabled
       :keys '((?l nil))
       :disabled t
       :info "Create a .clang_complete file in any subdirectory,\
 with one flag per line."))))

(defun irony-cdb--try-load-clang-complete ()
  (irony--awhen (cl-loop for dir in irony-cdb--clang-complete-cached-directories
                         with found = nil
                         ;; keep the closest directory
                         if (and (string-prefix-p dir (or buffer-file-name ""))
                                 (file-exists-p dir)
                                 (> (length dir) (length found)))
                         do (setq found dir)
                         finally return found)
    (irony-cdb--clang-complete-load-file-1 (concat it ".clang_complete"))))

(defun irony-cdb--clang-complete-load-file-1 (cc-file)
  "Load the flags from CC-FILE, one flag per line."
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      (setq compile-flags
            ;; remove whitespaces at the end of each line, if any
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    ;; keep track of the directory where the .clang_complete has been found to
    ;; load it automatically in other files/sessions.
    (add-to-list 'irony-cdb--clang-complete-cached-directories invocation-dir)
    (cons compile-flags invocation-dir)))

(defun irony-cdb--clang-complete-load-file (cc-file)
  (let ((flags-dir (irony-cdb--clang-complete-load-file-1 cc-file)))
    (irony-update-command-line-options (car flags-dir) (cdr flags-dir))))

(provide 'irony-cdb)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-cdb.el ends here
