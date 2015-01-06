;;; irony-ast.el --- generate AST with libclang -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(require 'irony)
(require 'cl-lib)
(require 'cl)


;;
;; Internal variables and structures
;;

(cl-defstruct irony-ast
  "Roughly corresponds to a value of a CXCursor.

KIND is clang_getCursorKind

START, END give the cursor range. They may be invalid if the file
was edited since this cursor was generated.

PARENT-KIND, PARENT-OFFSET specify parent cursor. Cursors are not
necessarily nested within their parent (for example, comment
cursors). The offset is parent's start relative to START. This
way editing a buffer is less likely to invalidate old cursors."
  ;; FIXME There is an assumption that there will not be two ast nodes
  ;; of the same kind starting at the same location.
  kind
  start
  end
  parent
  child
  prev
  next
  hash
  type
  name
  comment)


;;
;; Functions
;;

(defun irony-ast-toplevel ()
  "Return the top-level AST node at point."
  (let* (o parent)
    (setq o (car (cl-remove-if-not
                  (lambda (o) (eq (overlay-get o 'category) 'irony-ast))
                  (overlays-at (point))))
          parent (and o (irony-ast-parent (overlay-get o 'irony-ast))))
    (while (and o (irony-ast-p parent))
      (setq o (irony-ast--overlay-of-node parent)
            parent (irony-ast-parent (overlay-get o 'irony-ast))))
    (when o (overlay-get o 'irony-ast))))

(defun irony-ast-current-nodes ()
  "List of all AST nodes that overlap with point."
  (mapcar (lambda (o) (overlay-get o 'irony-ast))
          (cl-remove-if-not
           (lambda (o) (eq (overlay-get o 'category) 'irony-ast))
           (overlays-at (point)))))

(defun irony-ast-current-node ()
  "Return the inner-most AST node at point."
  (car
   (sort (irony-ast-current-nodes)
         (lambda (n1 n2)
           (> (irony-ast-node-depth n1) (irony-ast-node-depth n2))))))

(defun irony-ast-node-depth (node)
  (when (irony-ast-p node)
    (let ((count 0))
      (while (and (irony-ast-p node) (setq node (irony-ast-parent node)))
        (cl-incf count))
      count)))

(defun irony-ast-async (callback &optional force)
  "Ask for AST nodes to be regenerated if necessary, then call CALLBACK.

If FORCE is non-nil, regenerate AST nodes in any case."
  (if (unless force (irony-ast-current-node))
      (funcall callback)
    (irony-ast--send-request callback)))


;;
;; Implementation
;;

(defvar irony-ast--debug t)

(defmacro irony-ast--debug (&rest message-args)
  `(when irony-ast--debug (message ,@message-args)))

;; Delete overlays on modification. irony-ast is the overlay category.
(put 'irony-ast 'modification-hooks (list #'irony-ast--overlay-hook))
(put 'irony-ast 'insert-in-front-hooks (list #'irony-ast--overlay-hook))
(put 'irony-ast 'insert-behind-hooks (list #'irony-ast--overlay-hook))

(defun irony-ast--overlay-hook (overlay &rest _)
  (let ((node (overlay-get overlay 'irony-ast)))
    (irony-ast--remove-overlays (irony-ast-start node) (irony-ast-end node))))

(defun irony-ast--remove-overlays (begin end)
  ;; We need to remove all overlays that either overlap with
  ;; begin-end, or whose AST overlaps with begin-end.
  (let ((min-begin begin) (max-end end))
    (dolist (o (overlays-in begin end))
      (when (and (eq (overlay-get o 'category) 'irony-ast) (overlay-start o))
        ;; (irony-ast--debug "Delete overlay %s" o)
        (delete-overlay o)
        (let* ((node (overlay-get o 'irony-ast))
               (parent (irony-ast-parent node)))
          (cl-callf min min-begin (irony-ast-start node))
          (cl-callf max max-end (irony-ast-end node))
          (when (irony-ast-p parent)
            (cl-callf min min-begin (irony-ast-start parent))
            (cl-callf max max-end (irony-ast-end parent))))))
    (when (< min-begin begin) (irony-ast--remove-overlays min-begin begin))
    (when (> max-end end) (irony-ast--remove-overlays end max-end))))

(defun irony-ast--overlay-of-node (node)
  (let ((overlays
         (cl-remove-if-not (lambda (o) (eq node (overlay-get o 'irony-ast)))
                           (overlays-at (irony-ast-start node)))))
    (when overlays
      (cl-assert (eq (length overlays) 1))
      (car overlays))))


;;
;; Interface with irony-server
;;

(defun irony-ast--context ()
  (buffer-chars-modified-tick))

(defun irony-ast--send-request (&optional callback)
  "Send AST-generation request to irony-server."
  (let (line column)
    ;; FIXME Pass byte offset instead of line/col to avoid this
    (save-excursion
      ;; `position-bytes' to handle multibytes and 'multicolumns' (i.e
      ;; tabulations) characters properly
      (irony--without-narrowing
        (setq line (line-number-at-pos)
              column (1+ (- (position-bytes (point))
                            (position-bytes (point-at-bol)))))))
    ;; FIXME This should be ordinary let, but the rest of code needs
    ;; to have lexical-binding then.
    (lexical-let ((request-context (irony-ast--context))
                  (user-callback callback))
      (irony--send-file-request
       "toplevelAST"
       ;; FIXME There is some weirdness in how this function gets called
       ;; The function called is the CAR of the callback
       (list
        (lambda (&rest args)
          (when (equal request-context (irony-ast--context))
            (apply #'irony-ast--request-handler (car args))
            (when user-callback (funcall user-callback)))))
       (number-to-string line)
       (number-to-string column)))))

(defun irony-ast--request-handler (&rest args)
  ;; The first node is the top-level node.
  (irony-ast--remove-overlays
   (irony-ast-start (car args)) (irony-ast-end (car args)))
  ;; Adjust all cursor references.
  (dolist (node args)
    (irony-ast--adjust-cursor-ref args (gv-ref (irony-ast-parent node)))
    (irony-ast--adjust-cursor-ref args (gv-ref (irony-ast-child node)))
    (irony-ast--adjust-cursor-ref args (gv-ref (irony-ast-prev node)))
    (irony-ast--adjust-cursor-ref args (gv-ref (irony-ast-next node)))
    (irony-ast--adjust-cursor-ref-list args (irony-ast-type node))
    (cl-assert (not (eq node (irony-ast-parent node))))
    (cl-assert (not (eq node (irony-ast-child node))))
    (cl-assert (not (eq node (irony-ast-prev node))))
    (cl-assert (not (eq node (irony-ast-next node)))))
  (dolist (node args)
    (unless (irony-ast-p node)
      (irony-ast--debug "Bad node: %s" (prin1-to-string node))
      (error "Unexpected output in irony-ast"))
    (cl-callf copy-marker (irony-ast-start node))
    (cl-callf copy-marker (irony-ast-end node))
    (let ((o (make-overlay (irony-ast-start node) (irony-ast-end node))))
      (overlay-put o 'category 'irony-ast)
      (overlay-put o 'irony-ast node))))

(defun irony-ast--adjust-cursor-ref (all-nodes cursor-ref)
  (let ((ref (gv-deref cursor-ref)))
    (when (eq (car ref) 'cursor-ref)
      (setf (gv-deref cursor-ref)
            (cl-find-if (lambda (node) (equal (irony-ast-hash node) (cadr ref)))
                        all-nodes)))))

(defun irony-ast--adjust-cursor-ref-list (all-nodes elems)
  "Update all cursor-refs inside ELEMS"
  (if (and (consp (car-safe elems)) (eq (caar elems) 'cursor-ref))
      (irony-ast--adjust-cursor-ref all-nodes (gv-ref (car elems)))
    (when (car-safe elems)
      (irony-ast--adjust-cursor-ref-list all-nodes (car-safe elems))))
  (when (cdr-safe elems)
    (irony-ast--adjust-cursor-ref-list all-nodes (cdr-safe elems)))
  nil)

(when irony-ast--debug
  (trace-function #'irony-ast--send-request)
  (trace-function #'process-send-string)
  (trace-function #'start-process)
  (trace-function #'irony--send-file-request)
  (trace-function #'irony--server-process-filter)
  (trace-function #'irony--start-server-process)
  (trace-function #'irony-ast--remove-overlays))


;;
;; Debug AST with highlighting overlays
;;

(defface irony-ast-face-0
  '((t (:background "red")))
  "irony-ast-face-0"
  :group 'irony)
(defface irony-ast-face-1
  '((t (:background "gold")))
  "irony-ast-face-1"
  :group 'irony)
(defface irony-ast-face-2
  '((t (:background "yellow")))
  "irony-ast-face-2"
  :group 'irony)
(defface irony-ast-face-3
  '((t (:background "green")))
  "irony-ast-face-3"
  :group 'irony)
(defface irony-ast-face-4
  '((t (:background "light blue")))
  "irony-ast-face-4"
  :group 'irony)
(defface irony-ast-face-5
  '((t (:background "blue")))
  "irony-ast-face-5"
  :group 'irony)
(defface irony-ast-face-6
  '((t (:background "purple")))
  "irony-ast-face-6"
  :group 'irony)

(defun irony-ast--level-face (level)
  (pcase level
    ((guard (< level 0)) 'irony-ast-face-0)
    (0 'irony-ast-face-0)
    (1 'irony-ast-face-1)
    (2 'irony-ast-face-2)
    (3 'irony-ast-face-3)
    (4 'irony-ast-face-4)
    (5 'irony-ast-face-5)
    (6 'irony-ast-face-6)
    (_ 'irony-ast-face-6)))

(defun irony-ast--hl-node (current-depth node)
  (let ((o (make-overlay (irony-ast-start node) (irony-ast-end node)))
        (level (- current-depth (irony-ast-node-depth node))))
    (overlay-put o 'category 'irony-ast-highlight-mode)
    (overlay-put o 'face (irony-ast--level-face level))
    (overlay-put o 'priority (- 60 level))))

(defun irony-ast--hl ()
  (with-demoted-errors "Error(irony-ast--hl): %S"
    (remove-overlays nil nil 'category 'irony-ast-highlight-mode)
    (let* ((current-node (irony-ast-current-node))
           (current-depth (irony-ast-node-depth current-node)))
      (dolist (node (irony-ast-current-nodes))
        (irony-ast--hl-node current-depth node))
      (let ((sibling current-node))
        (while (and sibling (setq sibling (irony-ast-next sibling)))
          (irony-ast--hl-node current-depth sibling)))
      (let ((sibling current-node))
        (while (and sibling (setq sibling (irony-ast-prev sibling)))
          (irony-ast--hl-node current-depth sibling))))))

(define-minor-mode irony-ast-debug-highlight-mode
  "Highlight AST tree nodes."
  :group 'irony
  (cond
   (irony-ast-debug-highlight-mode
    (add-hook 'post-command-hook #'irony-ast--hl nil t))
   (t
    (remove-hook 'post-command-hook #'irony-ast--hl))))

(provide 'irony-ast)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
;;; irony-ast.el ends here
