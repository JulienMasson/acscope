;;; acscope-lib.el --- acscope library containing common utils

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

(defgroup acscope-faces nil
  "Cscope Faces"
  :group 'acscope)

(cl-defstruct acscope-data
  (dir     nil :read-only t)
  (desc    nil :read-only t)
  (pattern nil :read-only t)
  (filter  nil :read-only t)
  (start   nil :read-only nil))

;;; Faces

(defface acscope-separator-face
  '((t :inherit 'font-lock-comment-delimiter-face))
  "Default face used to display separator"
  :group 'acscope-faces)

(defface acscope-default-directory-face
  '((t :inherit 'font-lock-keyword-face))
  "Default face used to display default directory"
  :group 'acscope-faces)

(defface acscope-file-face
  '((t :inherit 'font-lock-constant-face))
  "Default face used to display file"
  :group 'acscope-faces)

(defface acscope-function-face
  '((t :inherit 'font-lock-type-face))
  "Default face used to display function"
  :group 'acscope-faces)

(defface acscope-line-number-face
  '((t :inherit 'font-lock-string-face))
  "Default face used to display line number"
  :group 'acscope-faces)

;;; Customization

(defcustom acscope-default-program-name "cscope"
  "Cscope program name"
  :type 'string
  :group 'acscope)

(defcustom acscope-marker-ring-max 32
  "Length of the ring containing acscope mark `acscope--marker-ring'"
  :type 'number
  :group 'acscope)

;; Internal vars

(defvar acscope-marker--ring (make-ring acscope-marker-ring-max))

(defvar acscope-marker--index 0)

(defvar acscope-prompt--history nil)

;;; External Functions

(defun acscope-bold-string (str pattern)
  "Bold the string pattern in str"
  (if (string-match pattern str)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(concat (substring str 0 beg)
		(propertize pattern 'face 'bold)
		(substring str end (length str))))
    str))

(defun acscope-switch-to-buffer (buffer)
  "Custom acscope switch-to-buffer command"
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer-other-window buffer)))

;; prompt
(defun acscope-prompt-for-symbol (prompt)
  "Prompt the symbol at point"
  (let ((symbol (substring-no-properties
		 (symbol-name (symbol-at-point)))))
    (read-string (concat prompt " ("
			 (propertize symbol 'face 'success)
			 "): ")
		 nil 'acscope-prompt--history symbol)))

;; time
(defun acscope-get-time-seconds ()
  "Return time in seconds"
  (string-to-number (format-time-string "%s.%3N" (current-time))))

;; marker
(defun acscope-marker-save ()
  "Save point marker at point"
  (setq acscope-marker--index 0)
  (let ((marker (point-marker)))
    (when-let ((index (ring-member acscope-marker--ring marker)))
      (ring-remove acscope-marker--ring index))
    (ring-insert acscope-marker--ring marker)))

(defun acscope-marker-next ()
  "Get next marker in `acscope-marker--ring'"
  (unless (ring-empty-p acscope-marker--ring)
    (let ((index acscope-marker--index)
	  (length (ring-length acscope-marker--ring)))
      (setq acscope-marker--index (mod (+ index 1) length))
      (ring-ref acscope-marker--ring acscope-marker--index))))

(defun acscope-marker-pop ()
  "Pop back to where acscope was last invoked"
  (interactive)
  (when-let* ((marker (acscope-marker-next))
	      (buffer (marker-buffer marker))
	      (pos (marker-position marker)))
    (when (get-buffer buffer)
      (acscope-switch-to-buffer buffer)
      (goto-char pos))))

;; request utils
(defun acscope-create-request (dir desc get-args pattern filter start fail finish)
  "Create acscope request with `acscope-data' as data"
  (let ((data (make-acscope-data :dir dir
				 :desc desc
				 :pattern pattern
				 :filter filter)))
    (make-acscope-request :program acscope-default-program-name
			  :dir dir :args get-args
			  :start start
			  :fail fail
			  :finish finish
			  :data data)))

(defun acscope-create-multi-request (desc get-args pattern filter init next fail finish)
  "Create multi request, NEED COMMENTS HERE !!!!"
  (let ((first-request (list (acscope-create-request
			      (car acscope-database-list)
			      desc get-args pattern filter
			      init fail finish)))
	(next-requests (mapcar (lambda (dir)
				 (acscope-create-request
				  dir desc get-args pattern filter
				  next fail finish))
			       (cdr acscope-database-list))))
    (append first-request next-requests)))

;; properties
(defun acscope-add-properties (beg end file &optional pattern line)
  "Add properties to region from beg to end in `acscope-buffer-name' buffer"
  (with-current-buffer acscope-buffer-name
    (let ((inhibit-read-only t)
	  plist)
      (setq plist (plist-put plist 'acscope-file file))
      (when pattern
	(setq plist (plist-put plist 'acscope-pattern pattern)))
      (when line
	(setq plist (plist-put plist 'acscope-line line)))
      (add-text-properties beg (point-max) plist))))

(provide 'acscope-lib)
