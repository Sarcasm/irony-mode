;;; irony-iotask.el --- Abstraction for I/O-based tasks

;; Copyright (C) 2015  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords: processes, convenience

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

;; Purpose is to work with both processes and network streams.
;;
;; Unlike tq.el we want tasks to be composed of 0, 1-1, or n-n communications.
;; tq.el only supports 1-1.
;;
;; 0 is useful if a server command has been cached, it means there is no need
;; for communication. We may only know if a task is still cached when the
;; callback is called (maybe after some other asynchronous tasks, so the context
;; may have changed since the task was initially posted)
;;
;; n-n is useful if the number of request depends on the answer to a previous
;; request but we still want to be able to push new tasks.

;;; Code:

(eval-when-compile
  (require 'cl))                        ;for lexical-let macro

(require 'cl-lib)


;;
;; Error conditions
;;

;; `define-error' breaks backward compatibility with Emacs < 24.4
(defun irony-iotask--define-error (name message &optional parent)
  "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
  (unless parent (setq parent 'error))
  (let ((conditions
         (if (consp parent)
             (apply #'nconc
                    (mapcar (lambda (parent)
                              (cons parent
                                    (or (get parent 'error-conditions)
                                        (error "Unknown signal `%s'" parent))))
                            parent))
           (cons parent (get parent 'error-conditions)))))
    (put name 'error-conditions
         (delete-dups (copy-sequence (cons name conditions))))
    (when message (put name 'error-message message))))

(irony-iotask--define-error 'irony-iotask-error "I/O task error")
(irony-iotask--define-error 'irony-iotask-filter-error "I/O task filter error")
(irony-iotask--define-error 'irony-iotask-bad-task "Bad I/O task")
(irony-iotask--define-error 'irony-iotask-bad-data "Bad I/O task data")


;;
;; Structures
;;

(cl-defstruct (irony-iotask-result (:constructor irony-iotask-result-create))
  -tag ;; 'value or 'error, or nil when unset
  -value
  -error -error-data
  )

(defun irony-iotask-result-valid-p (result)
  (and (irony-iotask-result--tag result) t))

(defun irony-iotask-result-value-p (result)
  (eq (irony-iotask-result--tag result) 'value))

(defun irony-iotask-result-error-p (result)
  (eq (irony-iotask-result--tag result) 'error))

(defun irony-iotask-result-set-value (result value)
  (setf (irony-iotask-result--tag result) 'value)
  (setf (irony-iotask-result--value result) value))

(defun irony-iotask-result-set-error (result error &rest error-data)
  (setf (irony-iotask-result--tag result) 'error)
  (setf (irony-iotask-result--error result) error)
  (setf (irony-iotask-result--error-data result) error-data))

(irony-iotask--define-error 'irony-iotask-result-get-error
  "Result not set before call to get")

(defun irony-iotask-result-get (result)
  (cl-case (irony-iotask-result--tag result)
    ('value (irony-iotask-result--value result))
    ('error (signal (irony-iotask-result--error result)
                    (irony-iotask-result--error-data result)))
    (t
     (signal 'irony-iotask-result-get-error (list result)))))

;; FIXME: quoting issues? I cannot write "#'(lambda ()...)" as property value
(defmacro irony-iotask-define-task (var docstring &rest properties)
  "A task is simply a property list.

Each of these function are called in the buffer they were
originally created (at schedule time).

The functions `irony-iotask-put', `irony-iotask-get',
`irony-iotask-set-result' and `irony-iotask-set-error' are
available to the task's functions to set the task's result.

Properties:

`:start' (mandatory)
     Function to call to launch the task.

     Usually the function sends a string/command/message to the
     execution context. If the task do some caching it's possible
     that nothing is send, instead the execution context result
     should be set to indicate that the task is ready.

     The following additional functions are available to call
     inside the `:start' function to communicate with the
     underlying process:

     - `irony-iotask-send-string'
     - `irony-iotask-send-region'
     - `irony-iotask-send-eof'

`:update' (mandatory)
     Function to call when some process output is available.

     The function should determine whether a message is complete
     and set the result when it is. It should also detect if the
     message is invalid and throw the 'invalid-msg tag with a
     value of t in this case. If the message is incomplete, the
     function should do nothing.

     The process output is the current buffer.

`:finish' (optional)

     Function to call after the result has been set but before
     the callback is called.

     Usually performs some kind of cleanup operation.

     Note: it makes no sense to set a result or error in this
     function as it is necessarily been set beforehand.

`:on-success' (optional)

     Same as `:finish' but called only if the result IS NOT an error.

`:on-error' (optional)

     Same as `:finish' but called only if the result IS an error."
  (declare (indent 1)
           (doc-string 2))
  `(progn
     (defvar ,var nil ,docstring)
     ;; Use `setq' to reset the var every time the macro is called.
     ;; This is useful, for example when evaluating using C-M-x (`eval-defun').
     ;; Trick stolen from auto-complete's `ac-define-source'
     (setq ,var '(,@properties))))

(cl-defstruct (irony-iotask-packaged-task
               (:constructor irony-iotask-packaged-task--create))
  task
  args
  result
  plist
  continuation)

(defun irony-iotask-package-task (task &rest args)
  (irony-iotask-packaged-task--create :task task
                                      :result (irony-iotask-result-create)
                                      :args args))

(defvar irony-iotask--current-packaged-task) ;dynamically bound
(defun irony-iotask-package-task-invoke (packaged-task prop-fn
                                                       &optional ignore-missing
                                                       &rest leading-args)
  (let* ((task (irony-iotask-packaged-task-task packaged-task))
         (args (irony-iotask-packaged-task-args packaged-task))
         (fn (plist-get task prop-fn)))
    (condition-case err
        (if fn
            ;; let binding for irony-iotask-{get,put}
            ;; and irony-iotask-set-{result,error}
            (let ((irony-iotask--current-packaged-task packaged-task))
              (apply fn (append leading-args args)))
          (unless ignore-missing
            (signal 'irony-iotask-bad-task
                    (list task (format "no %s function" prop-fn)))))
      (error
       (apply #'irony-iotask-result-set-error
              (irony-iotask-packaged-task-result packaged-task)
              (car err) (cdr err))))))

(defun irony-iotask--chain-1 (packaged-task-1 packaged-task-2)
  (while (irony-iotask-packaged-task-continuation packaged-task-1)
    (setq packaged-task-1
          (irony-iotask-packaged-task-continuation packaged-task-1)))
  (setf (irony-iotask-packaged-task-continuation packaged-task-1)
        packaged-task-2))

(defun irony-iotask-chain (packaged-task-1 packaged-task-2 &rest others)
  (setq others (cons packaged-task-2 others))
  (while others
    (irony-iotask--chain-1 packaged-task-1 (car others))
    (setq others (cdr others)))
  packaged-task-1)

(cl-defstruct (irony-iotask-ectx
               (:constructor irony-iotask-ectx--create))
  started
  packaged-task
  callback
  schedule-buffer)

(defun irony-iotask-ectx-call-callback (ectx result)
  (let ((cb-buffer (irony-iotask-ectx-schedule-buffer ectx)))
    (when (buffer-live-p cb-buffer)     ;[GH-427]
      (with-demoted-errors "Irony I/O task: error in callback: %S"
        (with-current-buffer cb-buffer
          (funcall (irony-iotask-ectx-callback ectx) result))))))

(defvar irony-iotask--process)

(defun irony-iotask--start-next (process)
  (let* ((ectx (car (process-get process :ectx-q)))
         (packaged-task (irony-iotask-ectx-packaged-task ectx)))
    (setf (irony-iotask-ectx-started ectx) t)
    ;; erase the buffer before we call :start so the next :update starts anew
    (erase-buffer)
    ;; for `irony-iotask-send-string', `irony-iotask-send-region' and
    ;; `irony-iotask-send-eof'
    (let ((irony-iotask--process process))
      (save-current-buffer
        (irony-iotask-package-task-invoke packaged-task :start)))
    (irony-iotask--check-result process)))

(defun irony-iotask--start-next-safe (process)
  "Run the next task, if any."
  (let ((ectx-q (process-get process :ectx-q)))
    (when (and ectx-q (not (irony-iotask-ectx-started (car ectx-q))))
      (irony-iotask--start-next process))))

(defun irony-iotask--check-result (process)
  (let* ((ectx (car (process-get process :ectx-q)))
         (packaged-task (irony-iotask-ectx-packaged-task ectx))
         (result (irony-iotask-packaged-task-result packaged-task)))
    (when (irony-iotask-result-valid-p result)
      (save-current-buffer
        (if (irony-iotask-result-value-p result)
            (irony-iotask-package-task-invoke packaged-task :on-success t)
          (irony-iotask-package-task-invoke packaged-task :on-error t)))
      (save-current-buffer
        (irony-iotask-package-task-invoke packaged-task :finish t))
      (if (and (irony-iotask-packaged-task-continuation packaged-task)
               (irony-iotask-result-value-p result))
          ;; we got a non-error, we can chain to the next continuation
          (progn
            (setf (irony-iotask-ectx-packaged-task ectx)
                  (irony-iotask-packaged-task-continuation packaged-task))
            (irony-iotask--start-next process))
        ;; no continuation or an error, call the callback
        ;; and skip any potential continuation
        (setq ectx (pop (process-get process :ectx-q)))
        (irony-iotask-ectx-call-callback ectx result)
        (irony-iotask--start-next-safe process)))))

(irony-iotask--define-error 'irony-iotask-aborted "I/O task aborted")

(defun irony-iotask--abort-all (process &rest reasons)
  (let ((result (irony-iotask-result-create))
        ectx)
    (apply #'irony-iotask-result-set-error result 'irony-iotask-aborted reasons)
    (while (setq ectx (pop (process-get process :ectx-q)))
      (irony-iotask-ectx-call-callback ectx result))))


;;
;; Implementation details, internal mechanic
;;

(defun irony-iotask-process-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((ectx (car (process-get process :ectx-q)))
            packaged-task
            result)
        ;; if no task this is an error, a spurious message is an error
        (unless ectx
          (signal 'irony-iotask-filter-error (list "spurious output" output)))
        (setq packaged-task (irony-iotask-ectx-packaged-task ectx))
        (setq result (irony-iotask-packaged-task-result packaged-task))
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point))
        ;; go to the first char, so the `:update' function is called at the
        ;; beginning of the buffer, it is more convenient
        (goto-char (point-min))
        (when (catch 'invalid-msg
                (save-current-buffer
                  (irony-iotask-package-task-invoke packaged-task :update))
                nil)
          (irony-iotask-result-set-error result
                                         'irony-iotask-bad-data
                                         packaged-task
                                         (buffer-string)))
        (irony-iotask--check-result process)))))

(defun irony-iotask-process-sentinel (process event)
  ;; events usually ends with a newline, we don't want it
  (setq event (replace-regexp-in-string "\n\\'" "" event))
  (unless (process-live-p process)
    (irony-iotask--abort-all process "process stopped running" event)
    (message "%s process stopped!" (process-name process))))

(defun irony-iotask-check-process (process)
  (let ((pfilter (process-filter process))
        (psentinel (process-sentinel process)))
    (unless (eq pfilter 'irony-iotask-process-filter)
      (signal 'irony-iotask-error
              (list "invalid process filter" pfilter
                    "did you call `irony-iotask-setup-process'?")))
    (unless (eq psentinel 'irony-iotask-process-sentinel)
      (signal 'irony-iotask-error
              (list "invalid process sentinel" psentinel
                    "did you call `irony-iotask-setup-process'?"))))
  (unless (process-live-p process)
    (signal 'irony-iotask-error (list "Process ain't running!")))
  (unless (buffer-live-p (process-buffer process))
    (signal 'irony-iotask-error (list "Process' buffer dead!"))))


;;
;; Public API
;;

(defun irony-iotask-setup-process (process)
  "Call after creating the asynchronous process to let
irony-iotask setup the PROCESS filter and anything else that may
be needed."
  (with-current-buffer (process-buffer process)
    (set-process-filter process #'irony-iotask-process-filter)
    (set-process-sentinel process #'irony-iotask-process-sentinel)
    (buffer-disable-undo)))

(defun irony-iotask-schedule (process packaged-task callback)
  (let ((ectx (irony-iotask-ectx--create :packaged-task packaged-task
                                         :callback callback
                                         :schedule-buffer (current-buffer))))
    (irony-iotask-check-process process)
    (with-current-buffer (process-buffer process)
      ;; append the new task to the queue
      (process-put process :ectx-q (append (process-get process :ectx-q)
                                           (list ectx)))
      ;; run task if none were running
      (unless (cdr (process-get process :ectx-q))
        (irony-iotask--start-next process)))))

(defun irony-iotask-run (process packaged-task)
  "Blocking/waiting counterpart of `irony-iotask-schedule'.

Return the result (or signal the stored error) instead of passing
it to a callback.

Returns nil when quitting.

This function isn't reentrant, do not call it from another task."
  (lexical-let (run-result)
    ;; schedule an asynchronous task that set result when done
    (irony-iotask-schedule process
                           packaged-task
                           (lambda (result)
                             (setq run-result result)))

    ;; wait for the task to complete
    ;; quitting is allowed, in this case the task will still run but
    ;; asynchronously, it won't block the user interface but the result will be
    ;; lost
    (if (with-local-quit
          (while (not run-result)
            (accept-process-output process 0.05))
          t)
        ;; didn't quit, task was completed
        (irony-iotask-result-get run-result)
      ;; C-g was used
      ;; TODO: We cannot abort the task because that may break the other tasks,
      ;; the process will be in an unpredictable state. Howewer we can cancel
      ;; the continuations.
      )))

(defun irony-iotask-get (propname)
  (plist-get (irony-iotask-packaged-task-plist
              irony-iotask--current-packaged-task)
             propname))

(defun irony-iotask-put (propname value)
  (setf (irony-iotask-packaged-task-plist irony-iotask--current-packaged-task)
        (plist-put (irony-iotask-packaged-task-plist
                    irony-iotask--current-packaged-task)
                   propname
                   value)))

(defun irony-iotask--result ()
  (irony-iotask-packaged-task-result irony-iotask--current-packaged-task))

(defun irony-iotask-set-result (value)
  (irony-iotask-result-set-value (irony-iotask--result) value))

(defun irony-iotask-set-error (err &rest error-data)
  (apply #'irony-iotask-result-set-error (irony-iotask--result) err error-data))

(defun irony-iotask-send-string (string)
  (process-send-string irony-iotask--process string))

(defun irony-iotask-send-region (start end)
  (process-send-region irony-iotask--process start end))

(defun irony-iotask-send-eof (string)
  (process-send-eof irony-iotask--process))

(provide 'irony-iotask)
;;; irony-iotask.el ends here
