;;; irony-exprtype.el --- Type information at point. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; See Irony::exprtype in Irony.cpp.
;;
;;; Code:

(require 'irony)
(require 'pcase)

(defun irony-exprtype (&optional callback)
  "Return type information string at point.

If region is active, will use the region instead."
  (interactive)
  (if (use-region-p)
      (irony-exprtype-region (region-beginning) (region-end) callback)
    (irony-exprtype-region (point) (point) callback)))

(defun irony-exprtype-region (start end callback)
  (irony--send-file-request
   "exprtype"
    ;; FIXME Why "list" of a callback?
   (list (apply-partially #'irony-exprtype--handler callback))
   (number-to-string (1- (position-bytes start)))
   (number-to-string (1- (position-bytes end)))))

(defun irony-exprtype--fix-offsets (thing)
  ;; Adjust all values of the form (bound byte-offset byte-offset)
  ;; to use char offsets instead.
  (when (consp thing)
    (pcase (car thing)
      (`(bounds . (,start-bytes . (,end-bytes . nil)))
       (let ((start (byte-to-position (1+ start-bytes)))
             (end (byte-to-position (1+ end-bytes))))
         (setcar thing (list 'bounds start end))))
      (_ (irony-exprtype--fix-offsets (car thing))))
    (irony-exprtype--fix-offsets (cdr thing))))

(defun irony-exprtype--handler (callback exprtype)
  (irony-exprtype--fix-offsets exprtype)
  (if callback (funcall callback exprtype) (message "%S" exprtype)))

;; ;; FIXME Remove when no longer helpful
;; (trace-function #'irony-exprtype)
;; (trace-function #'irony-exprtype-region)
;; (trace-function #'irony-exprtype--handler)
;; (trace-function #'process-send-string)
;; (trace-function #'start-process)
;; (trace-function #'irony--send-file-request)
;; (trace-function #'irony--server-process-filter)
;; (trace-function #'irony--start-server-process)

(provide 'irony-exprtype)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
;;; irony-exprtype.el ends here
