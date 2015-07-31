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
;; request but we still want to be able to push new, different tasks

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

Properties:

`start' (mandatory)
     Function to call to launch the task.

     Usually the function sends a string/command/message to the
     execution context. If the task do some caching it's possible
     that nothing is send, instead the execution context result
     should be set to indicate that the task is ready.

     Takes an execution-context as parameter (`irony-iotask-ectx').

`update' (mandatory)
     Function to call when some process output is available.

     The function should determine whether a message is complete,
     in this case it should set the result in the execution
     context, it should also detect if the message is invalid and
     throw the 'invalid-msg tag with a value of t in this case.
     If the message is incomplete the function should do nothing.
     The return value isn't used.

     Takes two parameters, the execution context and an array of
     bytes."
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
  (process-output "")
  result
  continuation)

(defun irony-iotask-packaged-task--append-output (packaged-task output)
  (let ((new-output
         (concat (irony-iotask-packaged-task-process-output packaged-task)
                 output)))
    (setf (irony-iotask-packaged-task-process-output packaged-task)
          new-output)))

(defun irony-iotask-package-task (task &rest args)
  (irony-iotask-packaged-task--create :task task
                                      :result (irony-iotask-result-create)
                                      :args args))

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
  "The execution context used as arguments for tasks."
  -process
  -packaged-task
  ;; TODO: buffer should be determined thanks to an attribute :bound-to
  ;; {nil,process,buffer}, so we know in which context to execute the tasks,
  ;; they can do caching buffer-local variable in these buffers. Also, when the
  ;; buffer dies, the task should set the result to some kind of error (or maybe
  ;; we don't call the function at all?).
  ;;
  ;; what the default should be? process is safer?
  -buffer
  -callback)

(defun irony-iotask-ectx-create (process packaged-task callback)
  (irony-iotask-ectx--create :-process process
                             :-packaged-task packaged-task
                             :-buffer (current-buffer)
                             :-callback callback))

(defun irony-iotask-ectx--result (ectx)
  (irony-iotask-packaged-task-result (irony-iotask-ectx--packaged-task ectx)))

(defun irony-iotask-ectx-set-result (ectx value)
  (irony-iotask-result-set-value (irony-iotask-ectx--result ectx)
                                 value))

(defun irony-iotask-ectx-set-error (ectx error &rest error-data)
  (apply #'irony-iotask-result-set-error
         (irony-iotask-ectx--result ectx)
         error
         error-data))

(defun irony-iotask-ectx-write-string (ectx string)
  (process-send-string (irony-iotask-ectx--process ectx) string))

(defun irony-iotask-ectx--process-output (ectx)
  (irony-iotask-packaged-task-process-output
   (irony-iotask-ectx--packaged-task ectx)))

(defun irony-iotask-ectx--append-output (ectx output)
  (irony-iotask-packaged-task--append-output
   (irony-iotask-ectx--packaged-task ectx) output))

(defun irony-iotask-ectx-call (ectx prop-fn &rest leading-args)
  (let* ((packaged-task (irony-iotask-ectx--packaged-task ectx))
         (task (irony-iotask-packaged-task-task packaged-task))
         (args (irony-iotask-packaged-task-args packaged-task))
         (fn (plist-get task prop-fn)))
    (condition-case err
        (progn
          (unless fn
            (signal 'irony-iotask-bad-task
                    (list task (format "no %s function" prop-fn))))
          (with-current-buffer (irony-iotask-ectx--buffer ectx)
            (apply fn ectx (append leading-args args))))
      (error
       (apply #'irony-iotask-ectx-set-error ectx (car err) (cdr err))))))

(cl-defstruct (irony-iotask-pdata
               ;; The process is optional so that we can unit-test the queue.
               ;; Whether or not it is good design is debatable.
               (:constructor irony-iotask-pdata-create (&optional process)))
  "Structure for storing the necessary mechanics for running
tasks on a process. pdata stands for \"process data\"."
  queue)

(defun irony-iotask-pdata-enqueue (pdata task)
  (setf (irony-iotask-pdata-queue pdata)
        (append (irony-iotask-pdata-queue pdata) (list task))))

(defun irony-iotask-pdata-run-next (pdata)
  (let ((ectx (car (irony-iotask-pdata-queue pdata))))
    (irony-iotask-ectx-call ectx :start)
    (irony-iotask-pdata-check-result pdata)))

(defun irony-iotask-pdata-run-next-safe (pdata)
  "Run the next task, if any."
  (when (irony-iotask-pdata-queue pdata)
    (irony-iotask-pdata-run-next pdata)))

(defun irony-iotask-pdata--process-result (pdata)
  (let* ((ectx (pop (irony-iotask-pdata-queue pdata)))
         (result (irony-iotask-packaged-task-result
                  (irony-iotask-ectx--packaged-task ectx))))
    (with-demoted-errors "Irony I/O task: error in callback: %S"
      (with-current-buffer (irony-iotask-ectx--buffer ectx)
        (funcall (irony-iotask-ectx--callback ectx) result)))))

(defun irony-iotask-pdata-check-result (pdata)
  (let* ((ectx (car (irony-iotask-pdata-queue pdata)))
         (packaged-task (irony-iotask-ectx--packaged-task ectx))
         (result (irony-iotask-packaged-task-result packaged-task)))
    (when (irony-iotask-result-valid-p result)
      (if (and (irony-iotask-packaged-task-continuation packaged-task)
               (irony-iotask-result-value-p result))
          ;; we got a non-error, we can chain to the next continuation
          (progn
            (setf (irony-iotask-ectx--packaged-task ectx)
                  (irony-iotask-packaged-task-continuation packaged-task))
            (irony-iotask-pdata-run-next pdata))
        ;; no continuation or an error, call the callback
        ;; and skip any potential continuation
        (irony-iotask-pdata--process-result pdata)
        (irony-iotask-pdata-run-next-safe pdata)))))

(irony-iotask--define-error 'irony-iotask-aborted "I/O task aborted")

(defun irony-iotask-pdata-abort-current (pdata &rest reasons)
  (let ((ectx (car (irony-iotask-pdata-queue pdata))))
    (apply #'irony-iotask-ectx-set-error ectx 'irony-iotask-aborted reasons)
    (irony-iotask-pdata--process-result pdata)))

(defun irony-iotask-pdata-abort-all (pdata &rest reasons)
  (let (ectx)
    (while (setq ectx (car (irony-iotask-pdata-queue pdata)))
      (apply #'irony-iotask-ectx-set-error ectx 'irony-iotask-aborted reasons)
      (irony-iotask-pdata--process-result pdata))))


;;
;; Implementation details, internal mechanic
;;

(defun irony-iotask-process-data (process)
  (process-get process 'irony-iotask-pdata))

;; removing the dependance to a process is useful for testing
(defun irony-iotask-filter (pdata output)
  (let ((ectx (car (irony-iotask-pdata-queue pdata))))
    ;; if no task this is an error, a spurious message is an error
    (unless ectx
      (signal 'irony-iotask-filter-error (list "spurious output" output)))
    (irony-iotask-ectx--append-output ectx output)
    (let ((bytes (irony-iotask-ectx--process-output ectx)))
      (when (catch 'invalid-msg
              (irony-iotask-ectx-call ectx :update bytes)
              nil)
        (irony-iotask-ectx-set-error ectx 'irony-iotask-bad-data ectx bytes)))
    (irony-iotask-pdata-check-result pdata)))

(defun irony-iotask-process-filter (process output)
  (irony-iotask-filter (irony-iotask-process-data process) output))

(defun irony-iotask-process-sentinel (process event)
  ;; events usually ends with a newline, we don't want it
  (setq event (replace-regexp-in-string "\n\\'" "" event))
  (let ((pdata (irony-iotask-process-data process)))
    (unless (process-live-p process)
      (irony-iotask-pdata-abort-all pdata "process stopped running" event))))

(defun irony-iotask-check-process (process)
  (unless (process-live-p process)
    (signal 'irony-iotask-error (list "Process ain't running!")))
  (let ((pdata (irony-iotask-process-data process))
        (pfilter (process-filter process))
        (psentinel (process-sentinel process)))
    (unless (irony-iotask-pdata-p pdata)
      (signal 'irony-iotask-error
              (list (concat "invalid process data:"
                            " did you call `irony-iotask-setup-process'?"))))
    (unless (eq pfilter 'irony-iotask-process-filter)
      (signal 'irony-iotask-error
              (list "invalid process filter" pfilter)))
    (unless (eq psentinel 'irony-iotask-process-sentinel)
      (signal 'irony-iotask-error
              (list "invalid process sentinel" psentinel)))))


;;
;; Public API
;;

(defun irony-iotask-setup-process (process)
  "Call after creating the asynchronous process to let
irony-iotask setup the PROCESS filter and anything else that may
be needed."
  (set-process-filter process #'irony-iotask-process-filter)
  (set-process-sentinel process #'irony-iotask-process-sentinel)
  (process-put process 'irony-iotask-pdata
               (irony-iotask-pdata-create process))
  (buffer-disable-undo (process-buffer process)))

(defun irony-iotask-schedule (process task callback)
  ;; check argument
  (irony-iotask-check-process process)
  (let ((pdata (irony-iotask-process-data process)))
    (irony-iotask-pdata-enqueue pdata (irony-iotask-ectx-create process
                                                                task
                                                                callback))
    ;; run task if none were running
    (when (= (length (irony-iotask-pdata-queue pdata)) 1)
      (irony-iotask-pdata-run-next pdata))))

(defun irony-iotask-run (process task)
  "Blocking/waiting counterpart of `irony-iotask-schedule'.

Return the result (or signal the stored error) instead of passing
it to a callback.

Returns nil when quitting.

This function isn't reentrant, do not call it from another task."
  (lexical-let (run-result)
    ;; schedule an asynchronous task that set result when done
    (irony-iotask-schedule process task (lambda (result)
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
      ;; the process will be in an unpredictable state. Howewer we cancel the
      ;; continuations.
      )))

(provide 'irony-iotask)
;;; irony-iotask.el ends here
