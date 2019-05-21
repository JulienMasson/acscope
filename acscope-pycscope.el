;;; acscope-pycscope.el --- Pycscope Database Management

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/acscope
;; Created: 2019-05-21

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

(require 'acscope-buffer)
(require 'acscope-database)

(defgroup acscope-pycscope nil
  "Pycscope Database Management"
  :group 'acscope)

;;; Customization

(defcustom acscope-database-pycscope-program-name "pycscope"
  "Pycscope program name"
  :type 'string
  :group 'acscope-pycscope)

;;; Internal vars

(defconst acscope-database--pycscope-cross-ref (concat acscope-database--prefix
						      "pycscope.out")
  "Pycscope cscope cross-reference database file name")

(defconst acscope-database--pycscope-source-file (concat acscope-database--prefix
						      "pycscope.files")
  "Pycscope cscope source file name")

;;; Internal Functions

(defun acscope-database--pycscope-find-cmd ()
  "Pycscope find command when creating cscope database"
  (concat "find . -name \"*.py\" > " acscope-database--pycscope-source-file))

(defun acscope-database--pycscope-source-file-cmd (dir)
  "Pycscope cscope database source file command"
  (let ((default-directory dir)
	(inhibit-message t))
    (shell-command (acscope-database--pycscope-find-cmd))))

(defun acscope-database--pycscope-args ()
  "Pycscope cscope arguments following database options"
  (append (list "-i" acscope-database--pycscope-source-file
		"-f" acscope-database--pycscope-cross-ref)))

(defun acscope-database--pycscope-cleanup (dir)
  "Pycscope cscope database cleanup"
  (mapc (lambda (file)
	  (let ((default-directory dir))
	    (if (file-exists-p file)
		(delete-file file))))
	(list acscope-database--pycscope-cross-ref
	      acscope-database--pycscope-source-file)))

(defun acscope-database--pycscope-check (dir)
  "Pycscope cscope database check"
  (let ((default-directory dir))
    (file-exists-p acscope-database--pycscope-cross-ref)))

(defun acscope-database--pycscope-create-request (dir)
  "Create pycscope cscope database request"
  (let* ((default-directory dir)
	 (args (list "-f" (eval acscope-database--pycscope-cross-ref)
		     "-D" "-R"))
	 (desc (concat "Creating database cscope with "
		       (propertize "Pycscope" 'face 'warning)
		       " ...\n"))
	 (data (make-acscope-data :dir dir
				  :desc desc))
	 (request (make-acscope-request :program acscope-database-pycscope-program-name
					:dir dir :args args
					:start 'acscope-buffer-init-header
					:fail 'acscope-buffer-insert-fail
					:finish 'acscope-database-finish
					:data data)))
    request))

;;; External Functions

(defun acscope-database-add-pycscope (dir)
  "Add pycscope database"
  (interactive "DAdd pycscope database: ")
    (if (file-exists-p (concat dir acscope-database--pycscope-cross-ref))
	(add-to-list 'acscope-database-list dir t)
      (acscope-database-create "pycscope" dir)))

(provide 'acscope-pycscope)
