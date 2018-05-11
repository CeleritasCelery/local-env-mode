;;; config for `shell' -*- lexical-binding: t -*-

(require 'dash-functional)

(defun company-command--prefix ()
  (when (member major-mode company-env-enabled-modes)
    (when-let (prefix (company-grab-symbol))
      (when (and (not (s-contains? "/" prefix))
                 (not (s-prefix? "$" prefix))
                 (s-equals? prefix
                            (buffer-substring
                             (line-beginning-position)
                             (point))))
        prefix))))

(setq company-command--types '((builtin . "-b")
                               (keyword . "-k")
                               (command . "-c")))

(defun company-command--get-type (type)
  (-if-let* ((var (intern (concat "company-command--" (symbol-name type))))
             (value (and (boundp var) (symbol-value var))))
      value
    (set var (company-command--fetch type))))

;; IFS=':';for i in $PATH; do test -d "$i" && find "$i" -maxdepth 1 -executable -type f -name 'd*' -exec basename {} \;; done
(defun company-command--fetch (type &optional prefix)
  (let ((flag (alist-get type company-command--types))
        (buffer (generate-new-buffer "command-types")))
    (call-process "bash" nil buffer nil "-c" (s-join " " (list "compgen" flag prefix)))
    (let ((candidates (s-trim (with-current-buffer buffer
                                (buffer-string)))))
      (kill-buffer buffer)
      (when (s-present? candidates)
        (s-lines candidates)))))

(defun $proc-to-string (proc &rest args)
  (let ((buffer (generate-new-buffer "proc")))
    (apply #'call-process proc (null :in-file) buffer (null :display) args)
    (let ((output (s-trim (with-current-buffer buffer
                            (buffer-string)))))
      (kill-buffer buffer)
      (when (s-present? output)
        output))))

(defun company-command--candidates (prefix)
  (shell-env-sync 'func)
  (when (shell-env-sync 'alias)
    (setq process-aliases (--map (->> it
                                      (s-chop-prefix "alias ")
                                      (s-chop-suffix "'")
                                      (s-replace "='" "=")
                                      (s-replace "'\\''" "'"))
                                 process-aliases)))

  (cl-flet ((annotate (annot list)
                      (--map (progn (put-text-property 0 1 'annotation annot it) it)
                             list)))
    (->> (list process-aliases
               process-functions
               (company-command--get-type 'builtin)
               (company-command--get-type 'keyword)) ;; all the competion sources
         (--map (--filter (s-prefix? prefix it) it)) ;; filter by those matching the prefix
         (funcall (-flip #'-snoc) ;; add the command compeletion (already filtered)
                  (company-command--fetch 'command prefix))
         (-zip-with #'annotate '("alias" "function" "built-in" "keyword" "executable")) ;; add annotation
         (--map-first t (--map (-let [(cand meta) (s-split-up-to "=" it 1 t)]
                                 (when meta
                                   (put-text-property 0 1 'meta meta cand))
                                 cand)
                               it)) ;; added meta data to the aliases
         (-remove #'null) ;; remove empty sources
         (-flatten) ;; flaten into a single list
         (-distinct)))) ;; remove duplicates

(defun company-command--meta (cand)
  (if (s-equals? "executable" (get-text-property 0 'annotation cand))
      ($proc-to-string "which" cand)
    (get-text-property 0 'meta cand)))

(defun company-command (command &optional arg &rest ignored)
  "Complete shell commands and options using Fish shell. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-command))
    (prefix (company-command--prefix))
    (candidates (company-command--candidates arg))
    (meta (company-command--meta arg))
    (annotation (format " (%s)" (get-text-property 0 'annotation arg)))))

(defvar company-env-commands '("unset" "unsetenv" "munge"))
(defvar company-env-enabled-modes '(shell-mode) "enabled modes.")

(defun company-env--annotation (candidate)
  (-let [annotation (get-text-property 0 'annotation candidate)]
    (when annotation
      (format " (%s)" annotation))))

(defun company-env--prefix ()
  (when (member major-mode company-env-enabled-modes)
    (when-let (prefix (with-syntax-table (make-syntax-table (syntax-table))
                      (modify-syntax-entry ?{ "_")
                      (company-grab-symbol)))
      (unless (s-contains? "/" prefix)
        (-let [(cmd arg)
               (s-split (rx (+ space))
                        (buffer-substring (line-beginning-position)
                                          (point))
                        t)]
        (cond ((s-prefix? "$" prefix) (let ((var (s-chop-prefixes '("$" "{") prefix)))
                                        (cons var (1+ (length var))))) ;; expansion
                ;; When using unset export etc the variable name does not have a
                ;; `$' so we need to make sure to watch for this senario
              ((and (s-equals? arg prefix)
                      (-contains? company-env-commands cmd))
                 prefix)
                ;; used in asigment (i.e. FOO=bar). Force update the environment
                ;; to ensure we are not using this backend when
                ;; `company-command' would be better
                ((and (s-equals? cmd prefix) ;; used in assignment
                    (progn (shell-env-sync 'env)
                           (--any? (s-prefix? prefix it)
                                     process-environment)))
                 prefix)))))))

(defun company-env--candidates (prefix)
  (shell-env-sync 'env)
  (--map  (-let [(cand annot) (s-split-up-to "=" it 1 t)]
            (when annot
              (put-text-property 0 1 'annotation annot cand))
            cand)
          (--filter (s-prefix? prefix it)
                    process-environment)))

(defun company-env (command &optional arg &rest ignored)
  "Complete shell commands and options using Fish shell. See `company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-env))
    (prefix (company-env--prefix))
    (candidates (company-env--candidates arg))
    (annotation " (env)")
    (meta (get-text-property 0 'annotation arg))
    ))

(defvar env-dump-dir "/tmp")
(defvar shell-env-timestamps '((env)
                               (alias)
                               (func)))

(defvar shell-env-vars '((env . process-environment)
                         (alias . process-aliases)
                         (func . process-functions)))
(defvar process-aliases nil)


(defun shell-env-sync (type)
  (-when-let* ((src-file (shell-env-get-file type))
               (time-stamp (->> src-file
                                (file-attributes)
                                (nth 5))))
    (unless (equal time-stamp (alist-get type shell-env-timestamps))
      (shell-env-update type)
      (setf (alist-get type shell-env-timestamps) time-stamp))))

(advice-add 'getenv :before (lambda (&rest _) (shell-env-sync 'env)))

(provide 'local-env)
