;;; company-command.el --- shell command completion via company  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Troy Hinckley

;; Author: Troy Hinckley <t.macman@gmail.com>
;; Keywords: lisp

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

;; 

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'local-env)
(require 'subr-x)

(defvar company-command-enabled-modes '(shell-mode)
  "modes for which company commands is enabled")

(defvar company-command-shell-type "bash"
  "shell used to fetch commands")

(defvar company-command--types '((builtin . "-b")
				 (keyword . "-k")
				 (command . "-c")))

(defun company-command--prefix ()
  (when (member major-mode company-command-enabled-modes)
    (when-let (prefix (company-grab-symbol))
      (when (and (not (string-match-p "/" prefix))
                 (not (string-prefix-p "$" prefix))
                 (equals prefix
			 (buffer-substring
			  (line-beginning-position)
			  (point))))
        prefix))))

(defun company-command--get-type (type)
  (-if-let* ((var (intern (concat "company-command--" (symbol-name type))))
             (value (and (boundp var) (symbol-value var))))
      value
    (set var (company-command--fetch type))))

(defun company-command--fetch (type &optional prefix)
  (let ((flag (alist-get type company-command--types))
        (buffer (generate-new-buffer "command-types")))
    (call-process company-command-shell-type
		  nil buffer nil "-c" (format "compgen %s %s" flag prefix))
    (let ((candidates (string-trim (with-current-buffer buffer
				     (buffer-string)))))
      (kill-buffer buffer)
      (when (and candidates
		 (not (equal "" candidates)))
        (split-string candidates "\n")))))

(defun $proc-to-string (proc &rest args)
  (let ((buffer (generate-new-buffer "proc")))
    (apply #'call-process proc (null 'in-file) buffer (null 'display) args)
    (let ((output (string-trim (with-current-buffer buffer
				 (buffer-string)))))
      (kill-buffer buffer)
      (when (and output
		 (not (equal "" output)))
        output))))

(defun company-command--candidates (prefix)
  (shell-env-sync 'func)
  (when (shell-env-sync 'alias)
    (setq process-aliases (--map (->> it
                                      (string-remove-prefix "alias ")
                                      (string-remove-suffix "'")
                                      (replace-regexp-in-string "='" "=")
                                      (replace-regexp-in-string "'\\''" "'"))
                                 process-aliases)))

  (cl-flet ((annotate (annot list)
                      (--map (progn (put-text-property 0 1 'annotation annot it) it)
                             list)))
    (->> (list process-aliases
               process-functions
               (company-command--get-type 'builtin)
               (company-command--get-type 'keyword)) ;; all the competion sources
         (--map (--filter (string-prefix-p prefix it) it)) ;; filter by those matching the prefix
         (funcall (-flip #'-snoc) ;; add the command compeletion (already filtered)
                  (company-command--fetch 'command prefix))
         (-zip-with #'annotate '("alias" "function" "built-in" "keyword" "executable")) ;; add annotation
         (--map-first t (--map (-let [(cand meta) (split-string "=" it 'omit-nulls)]
                                 (when meta
                                   (put-text-property 0 1 'meta meta cand))
                                 cand)
                               it)) ;; added meta data to the aliases
         (-remove #'null) ;; remove empty sources
         (-flatten) ;; flaten into a single list
         (-distinct)))) ;; remove duplicates

(defun company-command--meta (cand)
  (if (equals "executable" (get-text-property 0 'annotation cand))
      ($proc-to-string "which" cand)
    (get-text-property 0 'meta cand)))

(defun company-command (command &optional arg &rest ignored)
  "Complete shell commands in shell mode. See `company's COMMAND
ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-command))
    (prefix (company-command--prefix))
    (candidates (company-command--candidates arg))
    (meta (company-command--meta arg))
    (annotation (format " (%s)" (get-text-property 0 'annotation arg)))))

(provide 'company-command)
;;; company-commands.el ends here
