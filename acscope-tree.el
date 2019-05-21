;;; acscope-tree.el --- Cscope Tree Management

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
(require 'acscope-database)
(require 'acscope-find)
(require 'acscope-lib)
(require 'acscope-request)

(defgroup acscope-tree nil
  "Cscope Tree Management"
  :group 'acscope)

;;; Customization

(defcustom acscope-tree-depth-max 4
  "Maximum cscope tree depth"
  :type 'number
  :group 'acscope-tree)

;;; Internal vars

(defvar acscope-tree--pattern-list nil)

(defvar acscope-tree--assoc-data nil)

(defvar acscope-tree--current-data nil)

(defvar acscope-tree--current-depth 0)

(defvar acscope-tree--start nil)

;;; Internal Functions

(defmacro acscope-tree--args (args)
  `(lexical-let ((args args))
     (lambda (dir)
       (append (acscope-database-args dir) ,args))))

(defun acscope-tree--insert-data (beg func file depth &optional line)
  "Recursive function, insert data in the buffer
until we reach `acscope-tree-depth-max'"
  (acscope-buffer-insert (concat (propertize func 'face 'acscope-function-face)
				 (when line
				   (format "[%s]"
					   (propertize line 'face
						       'acscope-line-number-face)))))
  (when file
    (acscope-add-properties beg (acscope-point-max) file func
				  (string-to-number line)))
  (acscope-buffer-insert "\n")
  (when (assoc func acscope-tree--assoc-data)
    (mapc (lambda (func)
	    (let* ((beg (acscope-point-max))
		   (file (cadr func))
		   (line (cl-caddr func))
		   (fmt-line "%-15s %s")
		   (arrow "┗━▶ ")
		   (length (+ (length arrow) (* depth 8)))
		   (spaces (make-string length (string-to-char " "))))
	      (when (< depth acscope-tree-depth-max)
		(acscope-buffer-insert (format fmt-line
					       (propertize (file-name-nondirectory file)
							   'face 'acscope-file-face)
					       (concat spaces arrow)))
		(acscope-tree--insert-data beg (car func) file (+ 1 depth) line))))
	  (assoc-default func acscope-tree--assoc-data))))

(defun acscope-tree--insert (data)
  "Insert results in the buffer"
  (if (not acscope-tree--assoc-data)
      (acscope-buffer-insert "\n --- No matches were found ---\n")
    (acscope-buffer-insert (format "\n%-15s " " "))
    (acscope-tree--insert-data nil (caar acscope-tree--assoc-data) nil 0))
  (acscope-buffer-insert (format "\nSearch time = %.2f seconds\n\n"
				 (- (acscope-get-time-seconds) acscope-tree--start))))

(defun acscope-tree--fail (output error data)
  "Insert failure message and insert current results collected"
  (acscope-buffer-insert (concat (propertize "ERROR: " 'face 'error)
				 error "\n"
				 (mapconcat 'identity output "\n")))
  (acscope-tree--insert data))

(defun acscope-tree--next-search (data)
  "Collect the data collected, create next tree requests or insert data"
  ;; add current data
  (setq acscope-tree--assoc-data (append acscope-tree--assoc-data
					 acscope-tree--current-data))
  ;; increase depth search
  (setq acscope-tree--current-depth (+ acscope-tree--current-depth 1))

  (let (patterns)

    ;; join all patterns to search
    (mapc (lambda (funcs)
	    (mapc (lambda (func)
		    (unless (assoc func acscope-tree--assoc-data)
		      (add-to-list 'patterns func t)))
		  (mapcar #'car (cdr funcs))))
	  acscope-tree--current-data)

    ;; reset current data
    (setq acscope-tree--current-data nil)

    ;; set new patterns
    (mapc (lambda (pattern)
	    (setq acscope-tree--pattern-list
		  (append acscope-tree--pattern-list
			  (make-list (length acscope-database-list) pattern))))
	  patterns)

    ;; create new requests
    (if acscope-tree--pattern-list
	(mapc (lambda (pattern)
		(let* ((dir (acscope-data-dir data))
		       (args `("-L" "-3" ,pattern))
		       (get-args (acscope-tree--args args))
		       (requests (acscope-create-multi-request
				  nil get-args pattern nil
				  'ignore
				  'ignore
				  'acscope-tree--fail
				  'acscope-tree--finish)))
		  (mapc #'acscope-request-run requests)))
	      patterns)
      (acscope-tree--insert data))))

(defun acscope-tree--handle-results (results data)
  "Handle results from the tree requests"
  (let ((pattern (pop acscope-tree--pattern-list))
	(dir (acscope-data-dir data))
	funcs)

    ;; collect functions who called this pattern
    (when results
      (mapc (lambda (result)
	      (mapc (lambda (elem)
		      (when-let ((func (plist-get elem :func))
				 (line (plist-get elem :line-nbr))
				 (file (concat dir (car result))))
			(unless (assoc func funcs)
			  (add-to-list 'funcs (cons func (list file line)) t))))
		    (cdr result)))
	    results)

      ;; add to current data at this tree depth
      (add-to-list 'acscope-tree--current-data (cons pattern funcs) t))

    ;; if we still have patterns, that means other data are coming
    (unless acscope-tree--pattern-list
      (if (and (< acscope-tree--current-depth acscope-tree-depth-max)
	       acscope-tree--current-data)
	  (acscope-tree--next-search data)
	(acscope-tree--insert data)))))

(defun acscope-tree--finish (output error data)
  "Handler when one tree request has been finished"
  (let ((results (acscope-find--collect-results output)))
    (acscope-tree--handle-results results data)))

(defun acscope-tree--insert-directory (data)
  "Insert the tree directory of the current request "
  (acscope-buffer-insert (format "━▶ Tree directory: %s\n"
				 (propertize (acscope-data-dir data)
					     'face 'acscope-default-directory-face))))

(defun acscope-tree--insert-initial (data)
  "Insert initial tree header"
  (acscope-buffer-insert-separator)
  (setq acscope-tree--start (acscope-get-time-seconds))
  (acscope-buffer-insert (concat "\n" (acscope-data-desc data) "\n"))
  (acscope-tree--insert-directory data)
  (acscope-switch-to-buffer acscope-buffer-name)
  (goto-char (point-max)))

(defun acscope-tree--reset ()
  "Reset internal tree vars"
  (setq acscope-tree--pattern-list nil)
  (setq acscope-tree--assoc-data nil)
  (setq acscope-tree--current-data nil)
  (setq acscope-tree--current-depth 0)
  (setq acscope-tree--start nil))

;;; External Functions

(defun acscope-tree-function-calling (pattern)
  "Tree functions calling this function"
  (interactive (list (acscope-prompt-for-symbol "Tree function calling")))
  (acscope-buffer-check)
  (acscope-database-check)
  (acscope-marker-save)
  (acscope-tree--reset)
  (let* ((dir (car acscope-database-list))
	 (desc (format "Tree function calling: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-3" ,pattern))
	 (get-args (acscope-find-args args))
	 (requests (acscope-create-multi-request
		    desc get-args pattern nil
		    'acscope-tree--insert-initial
		    'acscope-tree--insert-directory
		    'acscope-buffer-insert-fail
		    'acscope-tree--finish)))
    (setq acscope-tree--pattern-list (make-list (length acscope-database-list)
						pattern))
    (mapc #'acscope-request-run requests)))

(provide 'acscope-tree)
