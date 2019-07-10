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

(require 'tramp)
(require 'acscope-buffer)
(require 'acscope-lib)
(require 'acscope-request)

(defgroup acscope-database nil
  "Cscope Database Management"
  :group 'acscope)

;;; Customization

(defcustom acscope-database-fast-symbol nil
  "If non nil, enable fast symbol lookup"
  :type 'boolean
  :group 'acscope-database)

(defcustom acscope-database-default-sym "default"
  "Default symbol use when managing acscope database"
  :type 'string
  :group 'acscope-database)

(defcustom acscope-database-default-files
  (list "c" "cc" "cpp" "h" "s" "S")
  "List of file extension to include in acscope database"
  :type 'list
  :group 'acscope-database)

;; External vars

(defvar acscope-database-list nil
  "List of cscope database directory")

;;; Internal vars

(defconst acscope-database--prefix "cscope-"
  "Prefix use to make a matching between cscope files name and acscope symbols")

(defconst acscope-database--default-cross-ref (concat acscope-database--prefix
						      "default.out")
  "Default cscope cross-reference database file name")

(defconst acscope-database--default-source-file (concat acscope-database--prefix
						      "default.files")
  "Default cscope source file name")

(defconst acscope-database--default-fast-symbol `(,(concat acscope-database--prefix
							  "default.out.in")
						  ,(concat acscope-database--prefix
							  "default.out.po"))
  "Default cscope Fast symbol files name")

;;; Internal Functions

(defun acscope-database--git-toplevel ()
  "Return the path of the toplevel git directory"
  (if-let* ((git-cmd "git rev-parse --show-toplevel")
	    (output (shell-command-to-string git-cmd))
	    (local-path (file-name-as-directory
			 (replace-regexp-in-string "\n$" "" output))))
      (if (tramp-tramp-file-p default-directory)
	  (let* ((dissect (tramp-dissect-file-name default-directory))
		 (file-name (tramp-file-name-localname dissect))
		 (remote (replace-regexp-in-string
			  file-name "" default-directory)))
	    (concat remote local-path))
	local-path)))

(defun acscope-database--autodetect-sym (dir)
  "Autodetect the acscope symbol at DIR"
  (let* ((match (concat acscope-database--prefix "\\(.*\\)\.out$"))
	 (files (directory-files dir nil match))
	 (syms (mapcar (lambda (file)
			 (when (string-match match file)
			   (match-string 1 file)))
		       files)))
    (cond ((null syms) syms)
	  ((= (length syms) 1) (car syms))
	  (t (completing-read "Multi database detected, choose one: " syms)))))

(defun acscope-database--call (sym &rest arg)
  "Call the specific database function for SYM with ARG"
  (let ((fun (intern-soft (concat "acscope-database--" sym))))
    (if (functionp fun)
	(apply fun arg)
      (message "Function %s not found" fun))))

;; Default
(defun acscope-database--default-find-cmd ()
  "Default find command when creating cscope database"
  (format "find . -regex \".*\\.\\(%s\\)\" > %s"
	  (mapconcat 'identity acscope-database-default-files "\\|")
	  acscope-database--default-source-file))

(defun acscope-database--default-source-file-cmd (dir)
  "Default cscope database source file command"
  (let ((default-directory dir)
	(inhibit-message t))
    (shell-command (acscope-database--default-find-cmd))))

(defun acscope-database--default-args ()
  "Default cscope arguments following database options"
  (append (list "-k" "-i" acscope-database--default-source-file
		"-f" acscope-database--default-cross-ref)
	  (if acscope-database-fast-symbol '("-q"))))

(defun acscope-database--default-check-fast-symbol (dir)
  "Return t if the database respect `acscope-database-fast-symbol',
Otherwise we return nil"
  (let ((fast-symbol-files (mapcar (lambda (file)
				     (concat dir file))
				   acscope-database--default-fast-symbol)))
    (if acscope-database-fast-symbol
	(cl-find-if #'file-exists-p fast-symbol-files)
      (cl-find-if-not #'file-exists-p fast-symbol-files))))

(defun acscope-database--default-cleanup (dir)
  "Default cscope database cleanup"
  (mapc (lambda (file)
	  (let ((default-directory dir))
	    (if (file-exists-p file)
		(delete-file file))))
	(append acscope-database--default-fast-symbol
		(list acscope-database--default-cross-ref
		      acscope-database--default-source-file))))

(defun acscope-database--default-check (dir)
  "Default cscope database check"
  (let ((default-directory dir))
    (if (file-exists-p acscope-database--default-cross-ref)
	(unless (acscope-database--default-check-fast-symbol dir)
	  (acscope-database-create "default" dir)))))

(defun acscope-database--default-create-request (dir)
  "Create default cscope database request"
  (let* ((default-directory dir)
	 (args (append (acscope-database--default-args) '("-b")))
	 (desc (concat "Creating "
		       (if acscope-database-fast-symbol
			   (propertize "Fast Symbol " 'face 'bold))
		       "cscope database ...\n"))
	 (data (make-acscope-data :dir dir
				  :desc desc))
	 (request (make-acscope-request :program acscope-default-program-name
					:dir dir :args args
					:start 'acscope-buffer-init-header
					:fail 'acscope-buffer-insert-fail
					:finish 'acscope-database-finish
					:data data)))
    request))

;;; External Functions

(defun acscope-database-finish (output error data)
  "Handler when the database has been finished"
  (add-to-list 'acscope-database-list (acscope-data-dir data) t)
  (acscope-buffer-insert (format "Database created in %.2f seconds\n\n"
				 (- (acscope-get-time-seconds)
				    (acscope-data-start data)))))

(defun acscope-database-create (sym dir)
  "Create cscope database"
  (acscope-buffer-check)
  (let* ((cleanup (concat sym "-cleanup"))
	 (create-request (concat sym "-create-request"))
	 (request (acscope-database--call create-request dir))
	 (source-file-cmd (concat sym "-source-file-cmd")))
    (acscope-database--call cleanup dir)
    (acscope-database--call source-file-cmd dir)
    (when request
      (acscope-request-run request))))

(defun acscope-database-recreate ()
  "Recreate all databases `acscope-database-list'"
  (interactive)
  (acscope-database-check-files)
  (mapc (lambda (dir)
	  (if-let ((sym (acscope-database--autodetect-sym dir)))
	    (acscope-database-create sym dir)))
	  acscope-database-list))

(defun acscope-database-reset ()
  "Reset cscope database list `acscope-database-list'"
  (interactive)
  (setq acscope-database-list nil))

(defun acscope-database-args (dir)
  "Return cscope arguments for DIR directory"
  (if-let* ((sym (acscope-database--autodetect-sym dir))
	    (args (concat sym "-args")))
      (acscope-database--call args)))

(defun acscope-database-check-files ()
  "Check cscope files for all database in `acscope-database-list'"
  (mapc (lambda (dir)
	  (let* ((sym (acscope-database--autodetect-sym dir))
		 (check (concat sym "-check")))
	    (if sym
		(acscope-database--call check dir)
	      (acscope-database-create acscope-database-default-sym dir))))
	acscope-database-list))

(defun acscope-database-check ()
  "Check database environment"
  (unless acscope-database-list
    (if-let ((git-repo (acscope-database--git-toplevel)))
	(add-to-list 'acscope-database-list git-repo t)
      (call-interactively 'acscope-database-add-default)))
  (acscope-database-check-files))

(defun acscope-database-add-default (dir)
  "Add default cscope database"
  (interactive "DAdd cscope database: ")
  (if (and (file-exists-p (concat dir acscope-database--default-cross-ref))
	   (acscope-database--default-check-fast-symbol dir))
      (add-to-list 'acscope-database-list dir t)
    (acscope-database-create "default" dir)))

(provide 'acscope-database)
