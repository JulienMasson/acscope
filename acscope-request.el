;;; acscope-request.el --- Cscope Request Management

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

(require 'subr-x)
(require 'cl-macs)
(require 'cl-seq)

(defgroup acscope-request nil
  "Cscope Request Management"
  :group 'tools)

(cl-defstruct acscope-request
  (dir    nil     :read-only t)
  (args   nil     :read-only t)
  (start  'ignore :read-only t)
  (fail   'ignore :read-only t)
  (finish 'ignore :read-only t)
  (data   nil     :read-only nil))

;;; Customization

(defcustom acscope-request-program-name "cscope"
  "Cscope program name"
  :type 'string
  :group 'acscope-request)

(defcustom acscope-request-buffer-name "*acscope-process*"
  "Cscope Request buffer name"
  :type 'string
  :group 'acscope-request)

;;; Internal Variables

(defvar acscope-request--pending nil
  "List of pending acscope request")

(defvar acscope-request--current nil
  "Current acscope request")

(defvar acscope-request--output nil
  "Data collected of the current acscope request")

;;; Internal Functions

(defun acscope-request--tramp-executable-find (dir program)
  "Find program over tramp"
  (with-parsed-tramp-file-name dir nil
    (let ((buffer (tramp-get-connection-buffer v))
	  (cmd (concat "which " program)))
      (with-current-buffer buffer
	(tramp-send-command v cmd)
	(goto-char (point-min))
	(when (looking-at "^\\(.*\\)")
	  (match-string 1))))))

(defun acscope-request--find-program (dir)
  "Find `acscope-request-program-name' executable"
  (let ((default-directory dir))
    (if (tramp-tramp-file-p dir)
	(acscope-request--tramp-executable-find
	 dir acscope-request-program-name)
      (executable-find acscope-request-program-name))))

(defun acscope-request--get-output ()
  "Return a list of data collected from current acscope request"
  (if acscope-request--output
      (delq nil (split-string acscope-request--output "\n"))))

(defun acscope-request--process-next ()
  "Process next acscope request"
  (setq acscope-request--current nil)
  (when-let ((request (pop acscope-request--pending)))
    (acscope-request--exec request)))

(defun acscope-request--funcall (func &rest args)
  "Print error message when corresponding funcall failed"
  (condition-case-unless-debug err
      (apply func args)
    (error (message "Error %s: %S" (symbol-name func) err))))

(defun acscope-request--process-sentinel (process status)
  "Acscope request sentinel process"
  (let ((data (acscope-request-data acscope-request--current))
	(output (acscope-request--get-output))
    	(func (if (eq (process-exit-status process) 0)
    		  (acscope-request-finish acscope-request--current)
    		(acscope-request-fail acscope-request--current))))
    (acscope-request--funcall func output status data)
    (setq acscope-request--output nil)
    (acscope-request--process-next)))

(defun acscope-request--process-filter (process str)
  "Collect data from the current acscope request process"
  (setq acscope-request--output (concat acscope-request--output str)))

(defun acscope-request--start-process (dir program args)
  "Start the acscope request process"
  (let* ((default-directory dir)
	 (buffer (get-buffer-create acscope-request-buffer-name))
	 (process (apply 'start-file-process "acscope" buffer
			 program  args)))
    (set-process-filter process 'acscope-request--process-filter)
    (set-process-sentinel process 'acscope-request--process-sentinel)))

(defun acscope-request--raise-error (request status)
  "Raise an error and process next request"
  (let ((data (acscope-request-data request))
	(func (acscope-request-fail request)))
    (acscope-request--funcall func nil status data)
    (acscope-request--process-next)))

(defun acscope-request--exec (request)
  "Execute the acscope request"
  (setq acscope-request--current request)
  (acscope-request--funcall (acscope-request-start request)
			   (acscope-request-data request))
  (let* ((dir (acscope-request-dir request))
	 (program (acscope-request--find-program dir))
	 (args (acscope-request-args request)))
    (cond ((not (file-exists-p dir))
	   (acscope-request--raise-error
	    request (concat dir " doesn't exist !")))
	  ((string= "" program)
	   (acscope-request--raise-error
	    request (concat "Cannot find: " acscope-request-program-name)))
	  ((or (not (listp args)) (cl-member nil args))
	   (acscope-request--raise-error
	    request "Incorrect arguments format"))
	  (t (acscope-request--start-process dir program args)))))

;;; External Functions

(defun acscope-request-cancel-current ()
  "Cancel current acscope request"
  (interactive)
  (setq acscope-request--output nil)
  (if-let ((process (get-buffer-process acscope-request-buffer-name)))
      (kill-process process)
    (acscope-request--process-next)))

(defun acscope-request-cancel-all ()
  "Cancel all acscope requests"
  (interactive)
  (setq acscope-request--pending nil)
  (acscope-request-cancel-current))

(defun acscope-request-run (request)
  "Run the acscope request if we don't have any pending acscope request.

Otherwise the acscope request is added to `acscope-request--pending' and will run later."
  (if acscope-request--current
      (setq acscope-request--pending (append acscope-request--pending
					    (list request)))
    (acscope-request--exec request)))

(provide 'acscope-request)
