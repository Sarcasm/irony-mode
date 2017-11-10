;; -*-no-byte-compile: t; -*-
(load (concat (file-name-directory (or load-file-name
                                       buffer-file-name))
              "test-config"))

;; load irony-iotask
;;
;; XXX: No idea why this is necessary, test-config already adds the directory to
;; the load-path so irony is found...
(unless (require 'irony-iotask nil t)
  (let ((irony-iotask-dir (expand-file-name "../../.." test-dir)))
    (add-to-list 'load-path irony-iotask-dir)
    (require 'irony-iotask)))

(defun irony-iotask-echo-process-exit-filter (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (process-mark process))
      (insert output)
      (set-marker (process-mark process) (point))
      (when (>= (buffer-size) (length "exit\n"))
        (should (string= (buffer-string) "exit\n"))
        (erase-buffer)))))

;; Note: these tests use process communication with the standard I/O streams.
;; The subprocess used for this communication is Emacs.
;;
;; The following article provides useful information for using Elisp as a
;; scripting language, Emacs as an interpreter, it details how the standard I/O
;; streams works in Elisp scripts:
;; - http://www.lunaryorn.com/2014/08/12/emacs-script-pitfalls.html

(defmacro irony-iotask/with-echo-process (&rest body)
  "Start an Emacs process that runs the given PROCESS-SCRIPT.

The process is setup with `irony-iotask-setup-process'.

It's possible to schedule some iotasks in the BODY for testing.

There is an exposed variable named `process' available for use in
BODY.

Elisp is used as a scripting language because it should be
available on all OSes irony-iotask support."
  (declare (indent 1))
  `(let ((process-connection-type nil)
         (process-adaptive-read-buffering nil)
         process)
     (setq process
           (start-process "emacs-irony-test"
                          "*emacs-irony-test*"
                          (expand-file-name invocation-name
                                            invocation-directory)
                          "-Q"
                          "--batch"
                          "--eval"
                          (prin1-to-string
                           '(let ((msg))
                              (while (not (equal msg "exit"))
                                (setq msg (read-from-minibuffer ""))
                                (message msg))))))
     (unwind-protect
         (progn
           (irony-iotask-setup-process process)
           ,@body)
       ;; the iotask process filter does not clean the process buffer
       ;; at the end of a request, but at the begining of a new one
       (with-current-buffer (process-buffer process)
         (erase-buffer))
       (set-process-filter process #'irony-iotask-echo-process-exit-filter)
       (process-send-string process "exit\n")
       ;; wait for the process to finish normally, or kill it if it doesn't
       (with-timeout (1 (kill-process process))
         (while (process-live-p process)
           (sit-for 0.05)))
       ;; start with a clean buffer,
       ;; Emacs 24.3 seems to keep some
       (kill-buffer (process-buffer process))
       (delete-process process))))

;; irony-iotask-result

(ert-deftest irony-iotask-result/ready-p-value ()
  (let ((result (irony-iotask-result-create)))
    (should-not (irony-iotask-result-valid-p result))
    (irony-iotask-result-set-value result 1)
    (should (irony-iotask-result-valid-p result))))

(ert-deftest irony-iotask-result/ready-p-error ()
  (let ((result (irony-iotask-result-create)))
    (should-not (irony-iotask-result-valid-p result))
    (irony-iotask-result-set-error result 'irony-iotask-error (list "blah"))
    (should (irony-iotask-result-valid-p result))))

(ert-deftest irony-iotask-result/set-value ()
  (let ((result (irony-iotask-result-create)))
    (irony-iotask-result-set-value result 'blah)
    (should (eq (irony-iotask-result-get result) 'blah))))

(irony--define-error 'irony-iotask-result/test-error
                     "Irony I/O task sample error")

(ert-deftest irony-iotask-result/set-error ()
  (let ((result (irony-iotask-result-create)))
    (irony-iotask-result-set-error result 'irony-iotask-result/test-error)
    (should-error (irony-iotask-result-get result)
                  :type 'irony-iotask-result/test-error)))

(ert-deftest irony-iotask-result/set-error-data ()
  (let ((result (irony-iotask-result-create)))
    (irony-iotask-result-set-error result
                                   'irony-iotask-result/test-error
                                   'foo 'bar 'baz 'qux)
    (condition-case err
        (irony-iotask-result-get result)
      (irony-iotask-result/test-error
       (should (equal (cdr err) '(foo bar baz qux)))))))

(ert-deftest irony-iotask-result/get-empty ()
  (let ((result (irony-iotask-result-create)))
    (should-error (irony-iotask-result-get result)
                  :type 'irony-iotask-result-get-error)))

;; task

(irony-iotask-define-task irony-iotask/task-start-t
  "doc"
  :start (lambda (&optional value)
           (irony-iotask-set-result (or value 42))))

(ert-deftest irony-iotask/task-start/simple ()
  (let ((task (irony-iotask-package-task irony-iotask/task-start-t)))
    (irony-iotask/with-echo-process
     (should (equal 42 (irony-iotask-run process task))))))

(ert-deftest irony-iotask/task-start/with-arguments ()
  (let ((task (irony-iotask-package-task irony-iotask/task-start-t 43)))
    (irony-iotask/with-echo-process
     (should (equal 43 (irony-iotask-run process task))))))

(irony-iotask-define-task irony-iotask/task-update-t
  "doc"
  :start (lambda (&optional hello)
           (irony-iotask-send-string (format "%s\n" (or hello "hello"))))
  :update (lambda (&optional hello)
            (setq hello (or hello "hello"))
            (when (string= (buffer-string) (format "%s\n" hello))
              (irony-iotask-set-result (format "%s ok" hello)))))

(ert-deftest irony-iotask-schedule/task-update/simple ()
  (let ((task (irony-iotask-package-task irony-iotask/task-update-t)))
    (irony-iotask/with-echo-process
     (should (string= "hello ok" (irony-iotask-run process task))))))

(ert-deftest irony-iotask-schedule/task-update/with-arguments ()
  (let ((task (irony-iotask-package-task irony-iotask/task-update-t "bonjour")))
    (irony-iotask/with-echo-process
     (should (string= "bonjour ok" (irony-iotask-run process task))))))

(irony-iotask-define-task irony-iotask/task-invalid-msg-t
  "doc"
  :start (lambda ()
           (irony-iotask-send-string "ping\n"))
  :update (lambda ()
            (when (string= (buffer-string) "ping\n")
              (throw 'invalid-msg t))))

(ert-deftest irony-iotask-schedule/task-update/invalid-msg ()
  (let ((task (irony-iotask-package-task irony-iotask/task-invalid-msg-t)))
    (irony-iotask/with-echo-process
     (should-error (irony-iotask-run process task)
                   :type 'irony-iotask-bad-data))))

(ert-deftest irony-iotask-chain/simple ()
  (let ((task (irony-iotask-chain
               (irony-iotask-package-task irony-iotask/task-update-t "hi")
               (irony-iotask-package-task irony-iotask/task-update-t "hej"))))
    (irony-iotask/with-echo-process
     (should (equal "hej ok" (irony-iotask-run process task))))))

(defvar irony-iotask/task-finish-var nil)
(defvar irony-iotask/task-on-var nil)
(irony-iotask-define-task irony-iotask/task-finish-t
  "doc"
  :start (lambda ()
           (irony-iotask-put :text "how")
           (irony-iotask-send-string "hello\n"))
  :update (lambda ()
            (cond
             ((string= (buffer-string) "hello\n")
              (irony-iotask-put :text (concat (irony-iotask-get :text) " are"))
              (irony-iotask-set-result t))
             ((>= (buffer-size) (1+ (length "hello\n")))
              (throw 'invalid-msg t))))
  :on-success (lambda ()
                (setq irony-iotask/task-on-var "success"))
  :finish (lambda ()
            (setq irony-iotask/task-finish-var (concat (irony-iotask-get :text)
                                                       " you?"))))

(ert-deftest irony-iotask-schedule/task-finish/simple ()
  (let ((task (irony-iotask-package-task irony-iotask/task-finish-t)))
    (irony-iotask/with-echo-process
     (setq irony-iotask/task-finish-var nil)
     (irony-iotask-run process task)
     (should (equal "how are you?" irony-iotask/task-finish-var)))))

(ert-deftest irony-iotask-schedule/task-on-success/simple ()
  (let ((task (irony-iotask-package-task irony-iotask/task-finish-t)))
    (irony-iotask/with-echo-process
     (setq irony-iotask/task-on-var nil)
     (irony-iotask-run process task)
     (should (equal "success" irony-iotask/task-on-var)))))

(irony-iotask-define-task irony-iotask/task-on-error-t
  "doc"
  :start (lambda ()
           (irony-iotask-set-error 'irony-iotask-error))
  :on-error (lambda ()
              (setq irony-iotask/task-on-var "error")))

(ert-deftest irony-iotask-schedule/task-on-error/simple ()
  (let ((task (irony-iotask-package-task irony-iotask/task-on-error-t)))
    (irony-iotask/with-echo-process
     (setq irony-iotask/task-on-var nil)
     (ignore-errors
       (irony-iotask-run process task))
     (should (equal "error" irony-iotask/task-on-var)))))

(ert-deftest irony-iotask-schedule/callback/recalls-schedule ()
  (let ((task (irony-iotask-package-task irony-iotask/task-update-t "a")))
    (irony-iotask/with-echo-process
     (lexical-let ((run-process process)
                   results)
       (irony-iotask-schedule process task
                              (lambda (result)
                                (setq results (list result))
                                (irony-iotask-schedule
                                 run-process
                                 (irony-iotask-package-task
                                  irony-iotask/task-update-t "b")
                                 (lambda (result)
                                   (setq results (append results (list result)))))))
       (should (with-local-quit
                 (while (< (length results) 2)
                   (accept-process-output process 0.05))
                 t))
       (should (string= "a ok" (irony-iotask-result-get (nth 0 results))))
       (should (string= "b ok" (irony-iotask-result-get (nth 1 results))))))))
