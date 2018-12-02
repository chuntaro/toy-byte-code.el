;;; disass-elc.el --- Disassemble compiled file  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; $ emacs --batch -l disass-elc.el foo.elc bar.elc ...

;;; Code:

(require 'cl-lib)

(defun disassemble-elc (elc)
  (let (form byte-codes)
    (cl-labels
        ((walk-form (form)
                    (cond
                     ((atom form)
                      (if (not (byte-code-function-p form))
                          form
                        (car (push (make-byte-code (aref form 0)
                                                   (aref form 1)
                                                   (aref form 2)
                                                   (aref form 3))
                                   byte-codes))))
                     ((eq (car form) 'byte-code)
                      (push (apply #'make-byte-code 0 (cdr form)) byte-codes)
                      form)
                     (t
                      (cons (car form)
                            (mapcar #'walk-form (cdr form)))))))
      (terpri)
      (princ elc)
      (terpri)
      (with-temp-buffer
        (insert-file-contents elc)
        (condition-case nil
            (while t
              (setq byte-codes nil
                    form (read (current-buffer))
                    form (walk-form form))
              (dolist (byte-code byte-codes)
                (princ "\n================================================================\n")
                (pp form)
                (princ "----------------------------------------------------------------\n")
                (princ (with-temp-buffer
                         (disassemble byte-code (current-buffer))
                         (buffer-string)))
                (princ "================================================================\n")))
          (end-of-file))))))

(when noninteractive
  (while command-line-args-left
    (disassemble-elc (car command-line-args-left))
    (pop command-line-args-left)))

(provide 'disass-elc)
;;; disass-elc.el ends here
