;;; acscope-compatibility.el --- Another Compatibility with other emacs version

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/acscope
;; Created: 2019-05-19

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

(defmacro if-let* (varlist then &rest else)
  "Bind variables according to VARLIST and evaluate THEN or ELSE.
This is like `if-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 2)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   form body)))
  (if varlist
      `(let* ,(setq varlist (internal--build-bindings varlist))
         (if ,(caar (last varlist))
             ,then
           ,@else))
    `(let* () ,then)))

(defmacro when-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
This is like `when-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 1) (debug if-let*))
  (list 'if-let* varlist (macroexp-progn body)))

(provide 'acscope-compatibility)
