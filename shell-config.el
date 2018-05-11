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

(defvar process-aliases nil)
(defvar process-functions nil)

(defun local-env-get-file (type)
  (-some->> ($get-local-pid)
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

(defun local-env-sync-env ()
  "sync the enviroment variables"
  (local-env-sync 'env))

(advice-add 'getenv :before #'local-env-sync-env)

(provide 'local-env)
