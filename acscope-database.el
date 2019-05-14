;;; acscope-database.el --- Cscope Database Management

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

(require 'acscope-buffer)
(require 'acscope-lib)
(require 'acscope-request)

(defgroup acscope-database nil
  "Cscope Database Management"
  :group 'acscope)

;;; Customization

(defcustom acscope-database-file "cscope.out"
  "Default cscope database file name"
  :type 'string
  :group 'acscope-database)

(defcustom acscope-database-source-file "cscope.files"
  "Default cscope source file name"
  :type 'string
  :group 'acscope-database)

(defcustom acscope-database-fast-symbol nil
  "If non nil, enable fast symbol lookup"
  :type 'boolean
  :group 'acscope-database)

(defcustom acscope-database-source-file-cmd 'acscope-database-default-source-file-cmd
  "Function used to generate cscope source file"
  :type 'function
  :group 'acscope-database)

;; External vars

(defvar acscope-database-list nil
  "List of cscope database directory")

;;; Internal vars

(defconst acscope-database--fast-symbol-files '("cscope.out.in" "cscope.out.po")
  "Cscope Fast symbol files")

;;; Internal Functions

(defun acscope-database--git-toplevel ()
  "Return the path of the toplevel git directory"
  (if-let* ((git-cmd "git rev-parse --show-toplevel")
	    (output (shell-command-to-string git-cmd)))
      (file-name-as-directory (replace-regexp-in-string "\n$" "" output))))

(defun acscope-database--find-cmd ()
  "Find command when creating cscope database"
  (concat "find . -name \"*.[chxsS]\" > " acscope-database-source-file))

(defun acscope-database--check-fast-symbol (dir)
  "Return t if the database respect `acscope-database-fast-symbol',
Otherwise we return nil"
  (let ((fast-symbol-files (mapcar (lambda (file)
				     (concat dir file))
				   acscope-database--fast-symbol-files)))
    (if acscope-database-fast-symbol
	(cl-find-if #'file-exists-p fast-symbol-files)
      (cl-find-if-not #'file-exists-p fast-symbol-files))))

;;; External Functions

(defun acscope-database-finish (output error data)
  "Handler when the database has been finished"
  (add-to-list 'acscope-database-list (acscope-data-dir data) t)
  (acscope-buffer-insert (format "Database created in %.2f seconds\n\n"
				 (- (acscope-get-time-seconds)
				    (acscope-data-start data)))))

(defun acscope-database-args ()
  "Return default cscope arguments following database options"
  (append (list "-k" "-i" acscope-database-source-file
		"-f" acscope-database-file)
	  (if acscope-database-fast-symbol '("-q"))))

(defun acscope-database-default-source-file-cmd (dir)
  "Default cscope database source file command"
  (let ((default-directory dir)
	(inhibit-message t))
    (shell-command (acscope-database--find-cmd))))

(defun acscope-database-remove-files (dir)
  "Remove cscope database files"
  (mapc (lambda (file)
	  (let ((default-directory dir))
	    (if (file-exists-p file)
		(delete-file file))))
	(append acscope-database--fast-symbol-files
		(list acscope-database-file acscope-database-source-file))))

(defun acscope-database-create (dir)
  "Create cscope database"
  (acscope-buffer-check)
  (acscope-database-remove-files dir)
  (let* ((default-directory dir)
	 (args (append (acscope-database-args) '("-b")))
	 (desc (concat "Creating "
		       (if acscope-database-fast-symbol
			   (propertize "Fast Symbol " 'face 'bold))
		       "cscope database ...\n"))
	 (data (make-acscope-data :dir dir
				  :desc desc))
	 (request (make-acscope-request :dir dir :args args
					:start 'acscope-buffer-init-header
					:fail 'acscope-insert-request-fail
					:finish 'acscope-database-finish
					:data data)))
    (unless (file-exists-p acscope-database-source-file)
      (funcall acscope-database-source-file-cmd dir))
    (acscope-request-run request)))

(defun acscope-database-add (dir)
  "Add cscope database"
  (interactive "DAdd cscope database: ")
  (if (and (file-exists-p (concat dir acscope-database-file))
	   (acscope-database--check-fast-symbol dir))
      (add-to-list 'acscope-database-list dir t)
    (acscope-database-create dir)))

(defun acscope-database-reset ()
  "Reset cscope database list `acscope-database-list'"
  (interactive)
  (setq acscope-database-list nil))

(defun acscope-database-check ()
  "Check database environment"
  (unless acscope-database-list
    (if-let ((git-repo (acscope-database--git-toplevel)))
	(add-to-list 'acscope-database-list git-repo t)
      (call-interactively 'acscope-database-add)))
  (mapc (lambda (dir)
	  (let ((database-file (concat dir acscope-database-file))
		(fmt "Database (%s) doesn't exist, create it ? "))
	    (if (file-exists-p database-file)
		(unless (acscope-database--check-fast-symbol dir)
		  (acscope-database-create dir))
	      (if (yes-or-no-p (format fmt database-file))
		  (acscope-database-create dir)
		(acscope-abort)))))
	acscope-database-list))

(provide 'acscope-database)
