;;; config for `local enviroment' -*- lexical-binding: t -*-


(require 'dash)
(require 'f)

(defvar env-dump-dir "/tmp")
(defvar local-env-timestamps '((env)
                               (alias)
                               (func)))

(defvar local-env-vars '((env . process-environment)
                         (alias . process-aliases)
                         (func . process-functions)))

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
  (when-let ((src-file (local-env-get-file type))
	     (time-stamp (->> src-file
			      (file-attributes)
			      (nth 5))))
    (unless (equal time-stamp (alist-get type local-env-timestamps))
      (local-env-update type)
      (setf (alist-get type local-env-timestamps) time-stamp))))

(defun local-env-sync-env (&rest _)
  "sync the enviroment variables"
  (local-env-sync 'env))

(advice-add 'getenv :before #'local-env-sync-env)

(provide 'local-env)
