;;; acscope-find.el --- Cscope Find Management

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
(require 'acscope-lib)
(require 'acscope-request)

(defgroup acscope-find nil
  "Cscope Find Management"
  :group 'acscope)

;;; Customization

(defcustom acscope-find-auto-update t
  "If non nil, update the cscope database for each `acscope-find--command' call"
  :type 'boolean
  :group 'acscope-find)

(defcustom acscope-find-default-regexp
  "^\\(.*\\)[ \t]+\\(.*\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)"
  "Default regexp used to match cscope output"
  :type 'string
  :group 'acscope-find)

(defcustom acscope-find-file-entry "***"
  "Default string to indicate file entry in `acscope-buffer-name' buffer"
  :type 'string
  :group 'acscope-find)

;;; Internal Functions

(defun acscope-find--jump-first-result ()
  "Jump to first result found in the current request"
  (with-current-buffer acscope-buffer-name
    (goto-char (point-max))
    (search-backward acscope-buffer-separator nil t)
    (search-forward acscope-find-file-entry nil t)
    (next-line)
    (acscope-find-enter)))

(defun acscope-find--parse-line (line &optional filter pattern)
  "Parse line and return cons (file . plist)
The plist contain: func, line-nbr and line-str"
  (when (string-match acscope-find-default-regexp line)
    (let ((file (substring line (match-beginning 1) (match-end 1)))
	  (func (substring line (match-beginning 2) (match-end 2)))
	  (line-nbr (substring line (match-beginning 3) (match-end 3)))
	  (line-str (substring line (match-beginning 4) (match-end 4))))
      (when (or (null filter) (funcall filter pattern file func line))
	(cons file `((:func ,func :line-nbr ,line-nbr :line-str ,line-str)))))))

(defun acscope-find--collect-results (output &optional filter pattern)
  "Parse output and return a list of result"
  (let (results)
    (mapc (lambda (line)
    	    (when-let ((result (acscope-find--parse-line line filter pattern)))
	      (if-let ((data (assoc-default (car result) results)))
	      	  (setcdr (assoc (car result) results)
	      		  (add-to-list 'data (cadr result) t))
    		(add-to-list 'results result t))))
	  output)
    results))

(defun acscope-find--insert-line-entry (pattern func nbr str)
  "Insert propertized line entry"
  (let ((func-prop (propertize func 'face 'acscope-function-face))
	(nbr-prop (propertize nbr 'face 'acscope-line-number-face))
	(str-prop (acscope-bold-string str pattern))
	(fmt-line "%-35s %s")
	(fmt-header "%s[%s]"))
    (acscope-buffer-insert (format fmt-line (format fmt-header func-prop nbr-prop)
			   str-prop))))

(defun acscope-find--insert-results (dir pattern results)
  "Insert results in the buffer"
  (mapc (lambda (result)
	  (let* ((file (car result))
		 (data (cdr result))
		 (beg (acscope-point-max)))
	    ;; insert file
	    (acscope-buffer-insert (propertize (format "%s %s:" acscope-find-file-entry file)
					       'face 'acscope-file-face))
	    (acscope-add-properties beg (acscope-point-max) (concat dir file))
	    (acscope-buffer-insert "\n")

	    ;; insert data
	    (mapc (lambda (elem)
	    	    (let ((func (plist-get elem :func))
	    		  (line-nbr (plist-get elem :line-nbr))
	    		  (line-str (plist-get elem :line-str))
	    		  (beg (acscope-point-max))
	    		  plist)
		      (acscope-find--insert-line-entry pattern func line-nbr line-str)
	    	      (acscope-add-properties beg (acscope-point-max)
						     (concat dir file)
						     pattern
						     (string-to-number line-nbr))
	    	      (acscope-buffer-insert "\n")))
	    	  data)
	    (acscope-buffer-insert "\n")))
	results))

(defun acscope-find--finish (output error data)
  "Handler when the find request has been finished"
  (let* ((filter (acscope-data-filter data))
	 (dir (acscope-data-dir data))
	 (pattern (acscope-data-pattern data))
	 (results (acscope-find--collect-results output filter pattern)))
    (if results
	(acscope-find--insert-results dir pattern results)
      (acscope-buffer-insert " --- No matches were found ---\n\n"))
    (when (and (eq (length results) 1)
	       (eq (length (cdar results)) 1))
      (acscope-find--jump-first-result))
    (acscope-buffer-insert (format "Search time = %.2f seconds\n\n"
				   (- (acscope-get-time-seconds)
				      (acscope-data-start data))))))

(defun acscope-find--command (desc args pattern &optional filter)
  "Run cscope find command"
  (acscope-buffer-check)
  (acscope-database-check)
  (acscope-marker-save)
  (let* ((args (append (acscope-find-args) args))
	 (requests (acscope-create-multi-request
		    desc args pattern filter
		    'acscope-buffer-init-header
		    'acscope-buffer-insert-header
		    'acscope-buffer-insert-fail
		    'acscope-find--finish)))
    (mapc #'acscope-request-run requests)))

(defun acscope-find--struct-filter (pattern file func line)
  "Custom filter to return only struct definition"
  (cond ((string-match-p (format "struct %s {" pattern) line))
	((string-match (format "typedef struct \\(.*\\) %s" pattern) line)
	 (acscope-find-struct-definition (match-string 1 line)))))

;;; External Functions

(defun acscope-find-enter ()
  "Goto current entry with the properties found at point"
  (interactive)
  (let ((file (get-text-property (point) 'acscope-file))
	(pattern (get-text-property (point) 'acscope-pattern))
	(line (get-text-property (point) 'acscope-line)))
    (when file
      (acscope-switch-to-buffer (find-file-noselect file))
      (when line
	(goto-line line)
	(when pattern
	  (end-of-line)
	  (search-backward pattern nil t)))
      (acscope-marker-save))))

(defun acscope-find-args ()
  "Return default find arguments"
  (append (acscope-database-args)
	  (unless acscope-find-auto-update '("-d"))))

(defun acscope-find-symbol (pattern)
  "Find this C symbol"
  (interactive (list (acscope-prompt-for-symbol "Find symbol")))
  (let* ((desc (format "Finding symbol: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-0" ,pattern)))
    (acscope-find--command desc args pattern)))

(defun acscope-find-global-definition (pattern)
  "Find this function definition"
  (interactive (list (acscope-prompt-for-symbol "Find global definition")))
  (let* ((desc (format "Finding global definition: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-1" ,pattern)))
    (acscope-find--command desc args pattern)))

(defun acscope-find-function-calling (pattern)
  "Find functions calling this function"
  (interactive (list (acscope-prompt-for-symbol "Find function calling")))
  (let* ((desc (format "Finding function calling: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-3" ,pattern)))
    (acscope-find--command desc args pattern)))

(defun acscope-find-text-string (pattern)
  "Find this text string"
  (interactive (list (acscope-prompt-for-symbol "Find text string")))
  (let* ((desc (format "Finding text string: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-4" ,pattern)))
    (acscope-find--command desc args pattern)))

(defun acscope-find-egrep (pattern)
  "Find this egrep pattern"
  (interactive (list (acscope-prompt-for-symbol "Find text with egrep")))
  (let* ((desc (format "Finding text with egrep: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-6" ,pattern)))
    (acscope-find--command desc args pattern)))

(defun acscope-find-file (pattern)
  "Find this file"
  (interactive (list (acscope-prompt-for-symbol "Find file")))
  (let* ((desc (format "Finding file: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-7" ,pattern)))
    (acscope-find--command desc args pattern)))

(defun acscope-find-symbol-assignment (pattern)
  "Find assignments to this symbol"
  (interactive (list (acscope-prompt-for-symbol "Find symbol assignment")))
  (let* ((desc (format "Finding symbol assignment: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-9" ,pattern)))
    (acscope-find--command desc args pattern)))

(defun acscope-find-struct-definition (pattern)
  "Find this struct definition"
  (interactive (list (acscope-prompt-for-symbol "Find struct definition")))
  (let* ((desc (format "Finding struct definition: %s\n"
		       (propertize pattern 'face 'bold)))
	 (args `("-L" "-1" ,pattern))
	 (filter 'acscope-find--struct-filter))
    (acscope-find--command desc args pattern filter)))

(provide 'acscope-find)
