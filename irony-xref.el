;;; irony-xref.el --- Irony xref interface
;;
;;; Commentary:
;;
;; To enable, run
;;
;;     (add-hook 'irony-mode-hook (lambda () (add-hook 'xref-backend-functions #'irony--xref-backend nil t)))
;;
;; Features:
;; - will jump into system headers
;; - with overloaded functions, will jump to the right function definition
;;   (it's not just string matching on the function name)
;;
;; Missing commands:
;; - ‘xref-find-apropos’
;;
;;; Code:

(require 'irony)
(require 'irony-completion)

(require 'xref)
(require 'dash)


;;
;; IO tasks
;;

(irony-iotask-define-task irony--t-xref-definitions
  "`xref-definitions' server command."
  :start (lambda (line col)
           (irony--server-send-command "xref-definitions" line col))
  :update irony--server-query-update)

(irony-iotask-define-task irony--t-xref-references
  "`xref-references' server command."
  :start (lambda (line col)
           (irony--server-send-command "xref-references" line col))
  :update irony--server-query-update)

;; (irony-iotask-define-task irony--t-find-apropos
;;   "`xref-apropos' server command."
;;   :start (lambda (what) (irony--server-send-command "xref-apropos" what))
;;   :update irony--server-query-update)


;;
;; Functions
;;

;;;###autoload
(defun irony-find-definitions (&optional pos)
  (interactive)
  (let* ((line-column (irony--completion-line-column pos))
         (line (car line-column))
         (column (cdr line-column)))
    (irony--run-task
     (irony-iotask-chain
      (irony--parse-task)      ; FIXME Is parsing even necessary here?
      (irony-iotask-package-task irony--t-xref-definitions line column)))))

;;;###autoload
(defun irony-find-references (&optional pos)
  (interactive)
  (let* ((line-column (irony--completion-line-column pos))
         (result (irony--run-task
                  (irony-iotask-chain
                   (irony--parse-task) ; FIXME Is parsing necessary here?
                   (irony-iotask-package-task irony--t-xref-references
                                              (car line-column) (cdr line-column))))))
    (cl-loop for item in result
             do (message "%S" item))))


;;
;; Xref backend
;;

;;;###autoload
(defun irony--xref-backend () 'irony)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql irony)))
  "A dummy completion table."
  nil)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql irony)))
  ;; FIXME These propertized strings are not suitable for completion
  ;; FIXME which xref asks for
  (let* ((bounds (irony-completion-symbol-bounds))
         (start (car bounds))
         (end (cdr bounds))
         ;; FIXME Is (buffer-file-name) the right thing here?
         (file (buffer-file-name))
         (buffer (current-buffer))
         (line-column (irony--completion-line-column start))
         (thing (buffer-substring-no-properties start end)))
    (put-text-property 0 (length thing) 'irony-xref (list file buffer start end) thing)
    ;; (message "xref-backend-identifier-at-point: %S" thing)
    thing))

(cl-defmethod xref-backend-definitions ((_backend (eql irony)) identifier)
  (-when-let*
      ((thing (get-text-property 0 'irony-xref identifier))
       (buffer (nth 1 thing))
       (line-column (irony--completion-line-column (nth 2 thing)))
       (result
        ;; FIXME Must this be synchronous
        (irony--run-task
         (irony-iotask-chain
          (irony--parse-task buffer)
          (irony-iotask-package-task irony--t-xref-definitions
                                     (car line-column) (cdr line-column))))))
    (cl-loop
     for (kind name filename line column start end) in result
     collect
     (xref-make (concat name "(" (symbol-name kind) ")") (xref-make-file-location filename line column))
     ;; do (message "result: %S" (list kind name filename line column start end))
     )))

(cl-defmethod xref-backend-references ((_backend (eql irony)) identifier)
  (-when-let*
      ((thing (get-text-property 0 'irony-xref identifier))
       (buffer (nth 1 thing))
       (line-column (irony--completion-line-column (nth 2 thing)))
       (result
        ;; FIXME Must this be synchronous
        (irony--run-task
         (irony-iotask-chain
          (irony--parse-task buffer)
          (irony-iotask-package-task irony--t-xref-references
                                     (car line-column) (cdr line-column))))))
    (cl-loop
     for (kind name filename line column start end) in result
     collect
     (xref-make name (xref-make-file-location filename line column))
     ;; do (message "result: %S" (list kind name filename line column start end))
     )))

;; (cl-defmethod xref-backend-apropos ((_backend (eql irony)) pattern)
;;   (let ((result
;;         ;; FIXME Must this be synchronous
;;          (irony--run-task
;;           (irony-iotask-package-task irony--t-find-apropos pattern))))
;;     (dolist (item result) (message "xref-backend-apropos: %S" item))
;;     (cl-loop
;;      for (kind name filename line column start end) in result
;;      collect
;;      (xref-make name (xref-make-file-location filename line column)))))


;; Setting up xref

(defun irony-xref--enter ()
  (add-hook 'xref-backend-functions #'irony--xref-backend nil t))

(defun irony-xref--exit ()
  (remove-hook 'xref-backend-functions #'irony--xref-backend t))

(provide 'irony-xref)
;;; irony-xref.el ends here
