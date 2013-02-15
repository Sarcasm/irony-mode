;;; irony-header-comp.el --- autocompletion for #include directives

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

;; Provide few functions that say when the cursor can complete an
;; #include directive and give the completion list of the includes.

;;; Code:

(require 'irony)

(eval-when-compile
  (require 'cl))

(defcustom irony-header-comp-compiler-args
  '("-v" "-E" "/dev/null")
  "The lang option will be automatically added (-x c++ / -x c)."
  :group 'irony
  :type 'file)

(defcustom irony-header-comp-allowed-extensions
  '("h" "hh"
    "hpp" "hxx" "h++"
    "H" "HH")
  "Allowed extensions for user include directories completion."
  :group 'irony
  :type '(choice (repeat string)))


;; Internal variables
;;

(defvar irony-header-comp-include-re
  "^#\\s-*include\\s-+[<\"]\\(\\(?:[^>\"]*\\)\\)\\="
  "Regexp used to search backward if we are inside an include
  statement.")

(defvar irony-header-comp-subdir nil
  "*internal variable* Contain the last subdirectory (or nil when
  there is no subdirectory) after a call to `irony-header-comp-point'.")

(defvar irony-compiler-header-directories-cache (make-hash-table :test 'equal)
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
(defun irony-get-compiler-header-directories-1 (lang-flag)
  "*Internal function* Really retrieve compiler search paths.
Please use `irony-get-compiler-header-directories'."
  (when irony-compiler-executable
    (with-temp-buffer
      (apply 'call-process irony-compiler-executable nil t nil
             (append lang-flag irony-header-comp-compiler-args))
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

(defun irony-get-compiler-header-directories ()
  "Retrieve compiler search paths for header files. Memoize the
search in a hash table."
  (let ((lang-flag (irony-language-option-flag)))
    (or (gethash lang-flag irony-compiler-header-directories-cache)
        (puthash lang-flag
                 (irony-get-compiler-header-directories-1 lang-flag) ;value is returned
                 irony-compiler-header-directories-cache))))

(defun irony-header-comp-inside-include-stmt-p ()
  "Return t if the cursor is inside an include statement, such
  as:

    #include <[POINT]           -> t
    #include <iostream[POINT]>  -> t
    #include <toto>[POINT]      -> nil
    int i[POINT]                -> nil"
  (looking-back irony-header-comp-include-re (point-at-bol)))

(defun irony-header-comp-point ()
  "Return the completion prefix for point, if the current context
  of point is an #include C/C++ directive. Return the position of
  the prefix or null if not in a context where an #include
  directive can be completed.

Example:

    #include <str[]
              ^~~~ Completion point returned."
  (when (re-search-backward irony-header-comp-include-re nil t)
    (let* ((slash-offset (position ?/ (string-to-vector (match-string 1))
                                   :from-end t))
           (begin (match-beginning 1))
           (end (+ begin (or (if slash-offset
                                 (+ slash-offset 1)) 0))))
      (setq irony-header-comp-subdir
            (if (not (= begin end))
                (buffer-substring-no-properties begin end)))
      end)))

(defun irony-header-comp-list-dir (base-dir sub-dir &optional filter-extensions)
  "List the content of SUB-DIR in BASE-DIR. If SUB-DIR is nil
list BASE-DIR.

A slash will be added for directory entry.

If FILTER-EXTENSIONS is non-nil, then
`irony-header-comp-allowed-extensions' will be use to filter
files and directories."
  (let (filelist is-dir
        (include-dir (file-name-as-directory
                      (concat (file-name-as-directory base-dir)
                              (or sub-dir "")))))
    (ignore-errors ;; "safer" this way than(file-directory-p include-dir)
      (loop for file in (directory-files include-dir nil "^[^.]" t)
            if (file-directory-p (concat include-dir file))
              collect (concat file "/")
            else if (or (not filter-extensions)
                        (member (file-name-extension file)
                                irony-header-comp-allowed-extensions))
            collect file))))

;; FIXME: POS ignored/useless but given
(defun irony-header-comp-complete-at (&optional pos)
  "Return the list of headers and directories available at
POS (the current position if not given). Headers extension is
filtered according to `irony-header-comp-allowed-extensions'."
  (let ((cwd (irony-current-directory))
        (header-directories (irony-get-compiler-header-directories)))
    (delete-dups
     (append
      (irony-header-comp-list-dir cwd
                                  irony-header-comp-subdir
                                  t)
      (loop for dir in header-directories
            append (irony-header-comp-list-dir dir irony-header-comp-subdir) into completions
            finally return completions)
      (loop for dir in (irony-header-search-paths)
            with done = (append '(cwd) header-directories)
            unless (member dir done)
            append (irony-header-comp-list-dir dir
                                               irony-header-comp-subdir
                                               t)
            into completions
            finally
            return completions)))))

(provide 'irony-header-comp)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; irony-header-comp.el ends here
