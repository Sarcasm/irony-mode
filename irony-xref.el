;;; irony-xref.el --- Irony xref interface
;;
;;; Commentary:
;;
;;; Code:

(require 'irony)
(require 'irony-completion)

(require 'xref)
(require 'dash)


;;
;; IO tasks
;;

(irony-iotask-define-task irony--t-find-definitions
  "`xref' server command."
  :start (lambda (line col)
           (irony--server-send-command "xref" line col))
  :update irony--server-query-update)

(irony-iotask-define-task irony--t-find-references
  "`grep' server command."
  :start (lambda (line col)
           (irony--server-send-command "grep" line col))
  :update irony--server-query-update)


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
      (irony--parse-task)
      (irony-iotask-package-task irony--t-find-definitions line column)))))

;;;###autoload
(defun irony-find-references (&optional pos)
  (interactive)
  (let* ((line-column (irony--completion-line-column pos))
         (result (irony--run-task
                  (irony-iotask-chain
                   (irony--parse-task)
                   (irony-iotask-package-task irony--t-find-references
                                              (car line-column) (cdr line-column))))))
    (cl-loop for item in result
             do (message "%S" item))))


;;
;; Xref backend
;;

;;;###autoload
(defun irony--xref-backend () 'irony)

;; (add-hook 'irony-mode (lambda () (add-hook 'xref-backend-functions #'irony--xref-backend nil t)))

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
    thing))

(cl-defmethod xref-backend-definitions ((_backend (eql irony)) identifier)
  (-when-let*
      ((thing (get-text-property 0 'irony-xref identifier))
       (buffer (nth 1 thing))
       (line-column (irony--completion-line-column (nth 2 thing)))
       (result
        ;; Must be synchronous
        (irony--run-task
         (irony-iotask-chain
          (irony--parse-task buffer)
          (irony-iotask-package-task irony--t-find-definitions
                                     (car line-column) (cdr line-column))))))
    (cl-loop
     for (kind name filename line column start end) in result
     collect
     (xref-make name (xref-make-file-location filename line column))
     do (message "result: %S" (list kind name filename line column start end)))))

(cl-defmethod xref-backend-references ((_backend (eql irony)) identifier)
  (-when-let*
      ((thing (get-text-property 0 'irony-xref identifier))
       (buffer (nth 1 thing))
       (line-column (irony--completion-line-column (nth 2 thing)))
       (result
        ;; Must be synchronous
        (irony--run-task
         (irony-iotask-chain
          (irony--parse-task buffer)
          (irony-iotask-package-task irony--t-find-references
                                     (car line-column) (cdr line-column))))))
    (cl-loop
     for (kind name filename line column start end) in result
     collect
     (xref-make name (xref-make-file-location filename line column))
     do (message "result: %S" (list kind name filename line column start end)))))

(provide 'irony-xref)
;;; irony-xref.el ends here
