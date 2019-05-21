;;; acscope-dired.el --- Cscope Dired

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

(require 'acscope-database)

;;; Internal Functions

(defun acscope-dired--assoc-database ()
  "Return assoc list of truncated path of database"
  (mapcar (lambda (path)
	    (cons (file-name-base (directory-file-name path))
		  path))
	  acscope-database-list))

;;; External Functions

(defun acscope-dired-directory ()
  "Dired in one of the directory of `acscope-database-list'"
  (interactive)
  (if-let* ((databases (acscope-dired--assoc-database))
	    (target (completing-read "Dired to: "
				     (mapcar #'car databases)))
	    (path (assoc-default target databases)))
      (dired path)))

(provide 'acscope-dired)
