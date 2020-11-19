;;; acscope-buffer.el --- Buffer Management for acscope

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

(require 'acscope-lib)

(defgroup acscope-buffer nil
  "Cscope Buffer Management"
  :group 'acscope)

(define-derived-mode acscope-buffer-mode fundamental-mode
  "acscope buffer"
  (acscope-buffer-update-header-line)
  (toggle-read-only t))

;;; Customization

(defcustom acscope-buffer-name "*acscope*"
  "Default acscope buffer name"
  :type 'string
  :group 'acscope-buffer)

(defcustom acscope-buffer-keep-history t
  "If nil, `acscope-buffer-check' will erase the `acscope-buffer-name' buffer"
  :type 'boolean
  :group 'acscope-buffer)

;;; External vars

(defvar acscope-buffer-mode-map (make-sparse-keymap)
  "Keymap for `acscope-buffer-mode'")

(defvar acscope-buffer-header-line-alist nil
  "An alist containing (format . func) used by `acscope-buffer-update-header-line'")

(defconst acscope-buffer-separator (make-string 80 (string-to-char "="))
  "Separator used in `acscope-buffer-name' buffer")

;;; External Functions

(defun acscope-buffer-update-header-line ()
  "Update header line of `acscope-buffer-name' buffer.

Please look at `acscope-buffer-header-line-alist' to know what is displayed"
  (with-current-buffer acscope-buffer-name
    (let ((str (mapcar (lambda (elem)
			 (format (car elem) (funcall (cdr elem))))
		       acscope-buffer-header-line-alist)))
      (setq header-line-format (string-join str "  "))
      (force-mode-line-update))))

(defun acscope-point-max ()
  "Return point max of `acscope-buffer-name' buffer"
  (with-current-buffer acscope-buffer-name
    (point-max)))

(defun acscope-buffer-insert (str)
  "Insert str at the end of `acscope-buffer-name' buffer"
  (with-current-buffer acscope-buffer-name
    (let ((inhibit-read-only t))
      (save-excursion
	(goto-char (point-max))
	(insert str)))))

(defun acscope-buffer-insert-separator ()
  "Insert separator in `acscope-buffer-name' buffer"
  (acscope-buffer-insert (propertize (concat acscope-buffer-separator "\n")
				     'face 'acscope-separator-face)))

(defun acscope-buffer-erase-all ()
  "Erase all contents in `acscope-buffer-name' buffer"
  (interactive)
  (with-current-buffer acscope-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun acscope-buffer-check ()
  "Check if `acscope-buffer-name' buffer exist and erase all the buffer
if `acscope-buffer-keep-history' is set to t"
  (if (get-buffer acscope-buffer-name)
      (unless acscope-buffer-keep-history
	(acscope-buffer-erase-all))
    (with-current-buffer (get-buffer-create acscope-buffer-name)
      (acscope-buffer-mode))))

(defun acscope-buffer-insert-dir-desc (dir desc)
  "Insert default directory and description in `acscope-buffer-name' buffer"
  (acscope-buffer-insert (format "\n━▶ Default directory: %s\n"
				 (propertize dir 'face 'acscope-default-directory-face)))
  (acscope-buffer-insert (concat "\n" desc "\n")))

(defun acscope-buffer-insert-header (data)
  "Set start time and call `acscope-buffer-insert-dir-desc'"
  (setf (acscope-data-start data) (acscope-get-time-seconds))
  (acscope-buffer-insert-dir-desc (acscope-data-dir data) (acscope-data-desc data)))

(defun acscope-buffer-init-header (data)
  "Initialize the header and jump to `acscope-buffer-name' buffer"
  (acscope-buffer-insert-separator)
  (acscope-buffer-insert-header data)
  (acscope-switch-to-buffer acscope-buffer-name)
  (goto-char (point-max)))

(defun acscope-buffer-insert-fail (output error data)
  "Insert failure message in `acscope-buffer-name' buffer"
  (acscope-buffer-insert (concat (propertize "ERROR: " 'face 'error)
				 error "\n"
				 (string-join output "\n") "\n")))


(provide 'acscope-buffer)
