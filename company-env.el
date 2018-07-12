;;; company-env.el --- company completion for enviroment variables  -*- lexical-binding: t; -*-

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
      (unless (string-match-p "/" prefix)
        (-let [(cmd arg)
               (split-string (buffer-substring (line-beginning-position)
					       (point))
			     (rx (+ space))
			     'omit-nulls)]
	  (cond ((string-prefix-p "$" prefix)
		 (let ((var (string-remove-prefix "{" (string-remove-prefix "$" prefix))))
		   (cons var (1+ (length var))))) ;; expansion
                ;; When using unset export etc the variable name does not have a
                ;; `$' so we need to make sure to watch for this senario
		((and (equal arg prefix)
                      (-contains? company-env-commands cmd))
                 prefix)
                ;; used in asigment (i.e. FOO=bar). Force update the environment
                ;; to ensure we are not using this backend when
                ;; `company-command' would be better
                ((and (equal cmd prefix) ;; used in assignment
		      (progn (local-env-sync 'env)
			     (--any? (string-prefix-p prefix it)
                                     process-environment)))
                 prefix)))))))

(defun company-env--candidates (prefix)
  (local-env-sync 'env)
  (--map  (-let [(cand annot) (split-string it "=" 'omit-nulls)]
            (when annot
              (put-text-property 0 1 'annotation annot cand))
            cand)
          (--filter (string-prefix-p prefix it)
                    process-environment)))

;;;###autoload
(defun company-env (command &optional arg &rest ignored)
  "Complete eviroment variables based on local enviroment. See
`company's COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-env))
    (prefix (company-env--prefix))
    (candidates (company-env--candidates arg))
    (annotation " (env)")
    (meta (get-text-property 0 'annotation arg))))

(provide 'company-env)
;;; company-env.el ends here
