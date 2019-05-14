;;; acscope.el --- Another Cscope Emacs Interface

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
(require 'acscope-extras)
(require 'acscope-find)
(require 'acscope-tree)

(defgroup acscope nil
  "Cscope Interface"
  :group 'tools)

;;; Customization

(defcustom acscope-mode-hook-list '(c-mode-hook c++-mode-hook dired-mode-hook
						python-mode-hook
						acscope-buffer-mode-hook)
  "List of mode hook where we apply acscope minor mode"
  :type 'list
  :group 'acscope)

;;; acscope minor mode

(defvar acscope-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s =") 'acscope-find-symbol-assignment)
    (define-key map (kbd "C-c s a") 'acscope-database-add)
    (define-key map (kbd "C-c s c") 'acscope-find-function-calling)
    (define-key map (kbd "C-c s d") 'acscope-find-global-definition)
    (define-key map (kbd "C-c s e") 'acscope-find-egrep)
    (define-key map (kbd "C-c s f") 'acscope-find-file)
    (define-key map (kbd "C-c s p") 'acscope-python-database-add)
    (define-key map (kbd "C-c s r") 'acscope-database-reset)
    (define-key map (kbd "C-c s s") 'acscope-find-symbol)
    (define-key map (kbd "C-c s t") 'acscope-find-text-string)
    (define-key map (kbd "C-c s u") 'acscope-marker-pop)
    (define-key map (kbd "C-c s D") 'acscope-dired-directory)
    (define-key map (kbd "C-c s S") 'acscope-find-struct-definition)
    (define-key map (kbd "C-c s T") 'acscope-tree-function-calling)
    map)
  "Keymap for `acscope-mode'")

(define-minor-mode acscope-mode
  "acscope mode"
  :group 'acscope)

;;; acscope major mode

(defun acscope-setup-major-mode ()
  "Define key for acscope major mode"
  (define-key acscope-buffer-mode-map [return] 'acscope-find-enter)
  (define-key acscope-buffer-mode-map "c" 'acscope-request-cancel-current)
  (define-key acscope-buffer-mode-map "C" 'acscope-request-cancel-all)
  (define-key acscope-buffer-mode-map "d" 'acscope-erase-request)
  (define-key acscope-buffer-mode-map "f" 'acscope-toggle-fast-symbol)
  (define-key acscope-buffer-mode-map "D" 'acscope-buffer-erase-all)
  (define-key acscope-buffer-mode-map "n" 'acscope-next-file)
  (define-key acscope-buffer-mode-map "p" 'acscope-previous-file)
  (define-key acscope-buffer-mode-map "q" 'acscope-quit)
  (define-key acscope-buffer-mode-map "s" 'acscope-toggle-keep-history)
  (define-key acscope-buffer-mode-map "t" 'acscope-tree-set-depth-max)
  (define-key acscope-buffer-mode-map "u" 'acscope-toggle-auto-update)
  (define-key acscope-buffer-mode-map "U" 'acscope-recreate-database)
  (define-key acscope-buffer-mode-map (kbd "C-n") 'acscope-next-request)
  (define-key acscope-buffer-mode-map (kbd "C-p") 'acscope-previous-request))

(defun acscope-quit ()
  "Quit acscope"
  (interactive)
  (kill-buffer acscope-buffer-name))

(defun acscope-next-pattern (pattern)
  "Move point to next pattern"
  (with-current-buffer acscope-buffer-name
    (end-of-line)
    (search-forward pattern nil t)
    (beginning-of-line)))

(defun acscope-previous-pattern (pattern)
  "Move point to previous pattern"
  (with-current-buffer acscope-buffer-name
    (beginning-of-line)
    (search-backward pattern nil t)
    (beginning-of-line)))

(defun acscope-next-file ()
  "Move point to next file"
  (interactive)
  (acscope-next-pattern acscope-find-file-entry))

(defun acscope-previous-file ()
  "Move point to previous file"
  (interactive)
  (acscope-previous-pattern acscope-find-file-entry))

(defun acscope-next-request ()
  "Move point to next request"
  (interactive)
  (acscope-next-pattern acscope-buffer-separator))

(defun acscope-previous-request ()
  "Move point to next request"
  (interactive)
  (acscope-previous-pattern acscope-buffer-separator))

(defun acscope-erase-request ()
  "Erase current request at point"
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  beg)
      (acscope-previous-request)
      (setq beg (point))
      (acscope-next-request)
      (if (eq (point) beg)
	  (delete-region beg (point-max))
	(delete-region beg (point))))))

(defun acscope-toggle-header-line (value)
  "Return propertized on/off following value"
  (let ((on (propertize "on" 'face 'success))
	(off (propertize "off" 'face 'error)))
    (if value on off)))

(defun acscope-toggle-auto-update ()
  "Toggle cscope auto-update"
  (interactive)
  (setq acscope-find-auto-update (not acscope-find-auto-update))
  (acscope-buffer-update-header-line))

(defun acscope-toggle-fast-symbol ()
  "Toggle cscope fast-symbol"
  (interactive)
  (setq acscope-database-fast-symbol (not acscope-database-fast-symbol))
  (mapc #'acscope-database-create acscope-database-list)
  (acscope-buffer-update-header-line))

(defun acscope-toggle-keep-history ()
  "Toggle cscope keep-history"
  (interactive)
  (setq acscope-buffer-keep-history (not acscope-buffer-keep-history))
  (unless acscope-buffer-keep-history
    (acscope-buffer-erase-all))
  (acscope-buffer-update-header-line))

(defun acscope-tree-set-depth-max (depth)
  "Set cscope tree depth max"
  (interactive "nTree depth max: ")
  (setq acscope-tree-depth-max depth)
  (acscope-buffer-update-header-line))

(defun acscope-tree-propertize-depth-max ()
  "Return a propertize string of `acscope-tree-depth-max'"
  (propertize (number-to-string acscope-tree-depth-max) 'face 'warning))

(defun acscope-setup-header-line ()
  "Set `acscope-buffer-header-line-alist' with what we want to display"
  (setq acscope-buffer-header-line-alist
	(list (cons "[auto-update]: %s" (lambda () (acscope-toggle-header-line
						    acscope-find-auto-update)))
	      (cons "[fast-symbol]: %s" (lambda () (acscope-toggle-header-line
						    acscope-database-fast-symbol)))
	      (cons "[keep-history]: %s" (lambda () (acscope-toggle-header-line
						     acscope-buffer-keep-history)))
	      (cons "[Tree depth-max]: %s" 'acscope-tree-propertize-depth-max))))

(defun acscope-recreate-database ()
  "Recreate all databases `acscope-database-list'"
  (interactive)
  (mapc #'acscope-database-create acscope-database-list))

;;; global setup

(defun acscope-global-setup ()
  "Setup acscope environment"
  (interactive)
  (acscope-setup-major-mode)
  (acscope-setup-header-line)
  (mapc (lambda (hook)
	  (add-hook hook #'acscope-mode))
	acscope-mode-hook-list))

(provide 'acscope)
