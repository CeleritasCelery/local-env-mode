;;; config for `local enviroment' -*- lexical-binding: t -*-

;; Package-Requires: ((dash "2.12.1") (f "0.18.2") (emacs "25.1"))

(require 'dash)
(require 'f)

(defvar env-dump-dir "/tmp")
(defvar local-env-timestamps '((env)
                               (alias)
                               (func)))

(defvar local-env-vars '((env . process-environment)
                         (alias . process-aliases)
                         (func . process-functions)))

(defvar local-env-capture-variables nil
  "list of environment variables that will be added to the global
  enviroment with `local-env-capture'")

(defvar local-env-shell-pid nil
  "Set this variable when the buffer process PID is not the shell PID.")
(make-variable-buffer-local 'local-env-shell-pid)

(defvar process-aliases nil)
(defvar process-functions nil)

(defun local-env-get-shell-pid ()
  (or local-env-shell-pid
      ;; we can only use the buffer process PID
      ;; with local shells
      (unless (file-remote-p default-directory)
	(-some->> (current-buffer)
		  get-buffer-process
		  process-id))))

(defun shx-cmd-set-pid (pid)
  "(SAFE) sets env local shell PID.
  Add the following lines to (or equvilent) to your shell starup file

  echo \"<set-pid $$>\""
  (setq local-env-shell-pid pid))

(defun local-env-get-file (type)
  (-some->> (local-env-get-shell-pid)
            (format (concat "%s." (symbol-name type)))
            (f-join env-dump-dir)
            (concat (file-remote-p default-directory))))

(defun local-env-update (type)
  (let ((var (alist-get type local-env-vars))
	(split-string-default-separators (rx (1+ "\n"))))
    (make-local-variable var)
    (-some->> (local-env-get-file type)
              f-read-text
              string-trim
              split-string 
              (set var))))

(defun local-env-sync (type)
  (when-let ((valid local-env-mode)
             (src-file (local-env-get-file type))
	     (time-stamp (->> src-file
			      (file-attributes)
			      (nth 5))))
    (unless (equal time-stamp (alist-get type local-env-timestamps))
      (local-env-update type)
      (setf (alist-get type local-env-timestamps) time-stamp))))

(defun local-env-sync-env (&rest _)
  "sync the enviroment variables"
  (local-env-sync 'env))

;;;###autoload
(defun local-env-capture ()
  "Load the local enviroment out of the buffer and into Emacs"
  (interactive)
  (cl-loop for env in local-env-shell-pid
           do (let ((val (getenv env)))
                ;; leave the current buffer scope to write the global
                ;; environment values
                (with-temp-buffer
                  (setenv env val)))))

;;;###autoload
(define-minor-mode local-env-mode
  "Toggle Local env mode.
have a seperate environment from
`process-environment' for the local buffer. This environment can
be synced to a remote process that is properly configured"
  :init-value nil
  :lighter " env"
  
  (if local-env-mode
      (advice-add 'getenv :before #'local-env-sync-env)
    (advice-remove 'getenv 'local-env-sync-env)
    (kill-local-variable 'process-environment)))

(provide 'local-env)
