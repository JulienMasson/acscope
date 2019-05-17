;;; acscope-extras.el --- Cscope Extras Utils

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/acscope
;; Created: 2019-05-14

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'acscope-database)

;; dired in acscope-database-list
(defun acscope-dired--assoc-database ()
  "Return assoc list of truncated path of database"
  (mapcar (lambda (path)
	    (cons (file-name-base (directory-file-name path))
		  path))
	  acscope-database-list))

(defun acscope-dired-directory ()
  "Dired in one of the directory of `acscope-database-list'"
  (interactive)
  (if-let* ((databases (acscope-dired--assoc-database))
	    (target (completing-read "Dired to: "
				     (mapcar #'car databases)))
	    (path (assoc-default target databases)))
      (dired path)))

;; add database with pycscope
(defun acscope-python--find-cmd ()
  "Find command when creating cscope database with pycscope"
  (concat "find . -name \"*.py\" > " acscope-database-source-file))

(defun acscope-python--source-file-cmd (dir)
  "Default pycscope database source file command"
  (let ((default-directory dir)
	(inhibit-message t))
    (shell-command (acscope-python--find-cmd))))

(defun acscope-python--database-create (dir)
  "Create cscope database with pycscope"
  (acscope-buffer-check)
  (let* ((default-directory dir)
	 (acscope-request-program-name "pycscope")
	 (args '("-D" "-R"))
	 (desc "Creating database cscope with pycscope ...\n")
	 (data (make-acscope-data :dir dir
				  :desc desc))
	 (request (make-acscope-request :dir dir :args args
					:start 'acscope-buffer-init-header
					:fail 'acscope-buffer-insert-fail
					:finish 'acscope-database-finish
					:data data)))
    (unless (file-exists-p acscope-database-source-file)
      (acscope-python--source-file-cmd dir))
    (acscope-request-run request)))

(defun acscope-python-database-add (dir)
  "Add pycscope database"
  (interactive "DAdd database pycscope: ")
    (if (file-exists-p (concat dir acscope-database-file))
	(add-to-list 'acscope-database-list dir t)
      (acscope-python--database-create dir)))

(provide 'acscope-extras)
