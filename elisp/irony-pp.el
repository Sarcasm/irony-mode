;;; irony-pp.el --- Preprocessor specific facilities

;; Copyright (C) 2012-2013  Guillaume Papin

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

;; Provide some functions that helps with the preprocessor specific stuff, such
;; as header search paths and preprocessor directives.

;;; Code:

(require 'irony)

(eval-when-compile
  (require 'cl))

(defcustom irony-pp-compiler-args
  '("-v" "-E" "/dev/null")
  "The lang option will be automatically added (-x c++ / -x c)."
  :group 'irony
  :type '(choice (repeat string)))

(defcustom irony-pp-header-allowed-extensions
  '("h" "hh"
    "hpp" "hxx" "h++"
    "H" "HH")
  "Allowed extensions for user include directories completion."
  :group 'irony
  :type '(choice (repeat string)))


;; Internal variables
;;

(defvar irony-pp-include-re
  "^#\\s-*include\\s-+[<\"]\\(\\(?:[^>\"]*\\)\\)\\="
  "Regexp used to search backward if we are inside an include
  statement.")

(defvar irony-pp-comp-subdir nil
  "*internal variable* Contains the last subdirectory (or nil when
  there is no subdirectory) after a call to `irony-pp-completion-point'.")

(defvar irony-pp-system-searth-paths-cache (make-hash-table :test 'equal)
  "*internal variable* Memoize compiler search paths.")

;; What's parsed:
;; #include "..." search starts here:
;;  includes...
;; #include <...> search starts here:
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.6.2/../../../../include/c++/4.6.2/x86_64-unknown-linux-gnu
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.6.2/../../../../include/c++/4.6.2/backward
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.6.2/include
;;  /usr/local/include
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.6.2/include-fixed
;;  /usr/include
;; End of search list.
(defun irony-pp-system-search-paths-1 (lang-flag)
  "*Internal function* Really retrieve compiler search paths.
Please use `irony-pp-system-search-paths'."
  (when irony-compiler-executable
    (with-temp-buffer
      (apply 'call-process irony-compiler-executable nil t nil
             (append lang-flag irony-pp-compiler-args))
      (goto-char (point-min))
      (let (directories
            (start "#include \"...\" search starts here:")
            (second-start "#include <...> search starts here:")
            (stop "End of search list."))
        (when (search-forward start nil t)
          (forward-line 1)
          (while (not (looking-at-p second-start))
            ;; skip whitespace at the begining of the line
            (skip-chars-forward "[:blank:]" (point-at-eol))
            (push (buffer-substring (point) (point-at-eol)) directories)
            (forward-line 1))
          (forward-line 1)
          (while (not (or (looking-at-p stop)
                          (eolp)))
            ;; skip whitespace at the begining of the line
            (skip-chars-forward "[:blank:]" (point-at-eol))
            (push (buffer-substring (point) (point-at-eol)) directories)
            (forward-line 1)))
        directories))))

(defun irony-pp-system-search-paths ()
  "Retrieve compiler search paths for header files. Memoize the
search in a hash table."
  (let ((lang-flag (irony-language-option-flag)))
    (or (gethash lang-flag irony-pp-system-searth-paths-cache)
        (puthash lang-flag
                 (irony-pp-system-search-paths-1 lang-flag) ;value is returned
                 irony-pp-system-searth-paths-cache))))

(defun irony-pp-inside-include-stmt-p ()
  "Return t if the cursor is inside an include statement, such
  as:

    #include <[POINT]           -> t
    #include <iostream[POINT]>  -> t
    #include <toto>[POINT]      -> nil
    int i[POINT]                -> nil"
  (looking-back irony-pp-include-re (point-at-bol)))

(defun irony-pp-completion-point ()
  "Return the completion prefix for point, if the current context
  of point is an #include C/C++ directive. Return the position of
  the prefix or null if not in a context where an #include
  directive can be completed.

Example:

    #include <str[]
              ^~~~ Completion point returned."
  (save-excursion
    (when (re-search-backward irony-pp-include-re nil t)
      (let* ((slash-offset (position ?/ (string-to-vector (match-string 1))
                                     :from-end t))
             (begin (match-beginning 1))
             (end (+ begin (or (if slash-offset
                                   (+ slash-offset 1)) 0))))
        (setq irony-pp-comp-subdir
              (if (not (= begin end))
                  (buffer-substring-no-properties begin end)))
        end))))

(defun irony-pp-list-dir (base-dir sub-dir &optional filter-extensions)
  "List the content of SUB-DIR in BASE-DIR. If SUB-DIR is nil
list BASE-DIR.

A slash will be added for directory entry.

If FILTER-EXTENSIONS is non-nil, then
`irony-pp-header-allowed-extensions' will be use to filter
files and directories."
  (let (filelist is-dir
                 (include-dir (file-name-as-directory
                               (concat (file-name-as-directory base-dir)
                                       (or sub-dir "")))))
    (ignore-errors ;; "safer" this way than (file-directory-p include-dir)
      (loop for file in (directory-files include-dir nil "^[^.]" t)
            if (file-directory-p (concat include-dir file))
            collect (concat file "/")
            else if (or (not filter-extensions)
                        (member (file-name-extension file)
                                irony-pp-header-allowed-extensions))
            collect file))))

;; FIXME: POS ignored/useless but given
(defun irony-pp-complete-at (&optional pos)
  "Return the list of headers and directories available at
POS (the current position if not given). Headers extension is
filtered according to `irony-pp-header-allowed-extensions'."
  (let ((cwd (irony-current-directory))
        (header-directories (irony-pp-system-search-paths)))
    (delete-dups
     (append
      (irony-pp-list-dir cwd irony-pp-comp-subdir t)
      (loop for dir in header-directories
            append (irony-pp-list-dir dir irony-pp-comp-subdir) into completions
            finally return completions)
      (loop for dir in (irony-user-search-paths)
            with done = (append '(cwd) header-directories)
            unless (member dir done)
            append (irony-pp-list-dir dir irony-pp-comp-subdir t)
            into completions
            finally
            return completions)))))

(provide 'irony-pp)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-pp.el ends here
