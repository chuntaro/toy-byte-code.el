;;; toy-byte-code.el --- Execution of byte-code  -*- lexical-binding: t; -*-

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

;; このファイル自体はバイトコンパイルしないでください
;;
;; foo.elc を単純に実行する
;; $ emacs --batch -l toy-byte-code.el -f eval-elc foo.elc
;;
;; foo.elc をトレース実行する
;; $ emacs --batch -l toy-byte-code.el -f trace-elc foo.elc
;;
;; trace-elc にスタックダンプ出力追加
;; $ emacs --batch -l toy-byte-code.el -f dump-elc foo.elc

;;; Code:

(require 'cl-lib)

(defvar enable-trace-byte-code nil)
(defvar enable-dump-stack nil)

(defun debugf (format-string &rest args)
  (when enable-trace-byte-code
    (princ (apply #'format format-string args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;
;; Stack operation
;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar stack (make-vector 256 0))
(defvar top -1)

(defsubst TOS ()
  "Top Of Stack"
  (aref stack top))

(defsubst $push (obj)
  (aset stack (cl-incf top) obj))

(defsubst $pop ()
  (prog1 (aref stack top)
    (cl-decf top)))

(defsubst $poke (n obj)
  "スタックトップからの相対位置 N に OBJ を書き込む"
  (aset stack (- top n) obj))

(defsubst $peek (n)
  "スタックトップからの相対位置 N から値を取得する"
  (aref stack (- top n)))

(defsubst $peekN (n)
  "スタックトップから N 個の値を vector で返す"
  (substring stack (- top n -1) (1+ top)))

(defsubst $popN (n)
  "`$peekN' の pop 版"
  (prog1 ($peekN n)
    (cl-decf top n)))

(defsubst $listN (n)
  "`$peekN' の list を返す版"
  (append ($peekN n) nil))

(defsubst $poplistN (n)
  "`$popN' の list を返す版"
  (append ($popN n) nil))

(defun $pushN (list)
  "LIST 内の値を順に `$push' して個数を返す"
  (let ((btm top))
    (dolist (elem list)
      ($push elem))
    (- top btm)))

(defun $dump ()
  "`stack' の内容をダンプする"
  (when enable-dump-stack
    (let ((i 0)
          (p top))
      (while (>= p 0)
        (debugf "                                  |%2d: %S\n" i (aref stack p))
        (cl-incf i)
        (cl-decf p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;
;; Debug output
;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-instruction (instruction &optional operand)
  "\"dup            \" や \"constant[0]    \" を返す"
  (format "%-28s" (if operand
                      (format "%s[%d]" instruction operand)
                    instruction)))

(defun dispp (instruction operand)
  "OPERAND で $peek するタイプ"
  (debugf "%s %S\n" (format-instruction instruction operand) ($peek operand)))

(defun disp1 (instruction &optional operand)
  "TOS から 1 つ使用するタイプ"
  (debugf "%s %S\n" (format-instruction instruction operand) (TOS)))

(defun disp2 (instruction)
  "TOS から 2 つ使用するタイプ"
  (debugf "%s %S %S\n" (format-instruction instruction) ($peek 1) (TOS)))

(defun dispN (instruction n)
  "TOS から N 個使用するタイプ"
  (debugf "%s %s\n" (format-instruction instruction) (mapconcat #'prin1-to-string ($peekN n) " ")))

(defun disp-header (vector maxdepth)
  (debugf "\n================================================================\n")
  (debugf "constants: %-40S maxdepth: %d\n" vector maxdepth)
  (debugf "----------------------------------------------------------------\n")
  (debugf "PC  Byte  Instruction                  Operand\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;
;; Call function
;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun funcall-byte-code-function (sym byte-code-function nargs)
  "BYTE-CODE-FUNCTION は #[257 \"\300...\207\" [+ 1 2 3] 6] のようなバイトコード関数"
  (let* ((params (aref byte-code-function 0))
         (min-args (logand params 7))
         (max-args (lsh params 8)))
    (when (or (< nargs min-args)
              (> nargs max-args))
      (signal 'wrong-number-of-arguments (list sym nargs)))
    (prog1 (byte-code (aref byte-code-function 1)
                      (aref byte-code-function 2)
                      (aref byte-code-function 3))
      (cl-decf top nargs))))

(defun op-call (nargs)
  "インストラクション `call' の実装"
  (dispN (format-instruction "call" nargs) (1+ nargs))
  (let* ((sym ($peek nargs))
         (func (indirect-function sym))
         (bytep (byte-code-function-p func)))
    ($poke 0 (if bytep
                 (funcall-byte-code-function sym func nargs)
               (apply sym ($poplistN nargs))))
    bytep))

(defsubst op-subrcall (subr)
  "`nth' 等の subr を呼び出す共通処理"
  (let* ((arg1 ($pop))
         (arg0 ($pop)))
    ($push (funcall subr arg0 arg1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;
;; Execute byte-code
;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun byte-code (bytestr vector maxdepth)
  "Emacs 標準の `byte-code' をオーバーライドする"
  (when (>= (+ top maxdepth) (length stack))
    (error "Stack overflow: (%s >= %s)" (+ top maxdepth) (length stack)))

  (disp-header vector maxdepth)

  (let ((pc 0)
        (end (length bytestr))
        (unwind top)
        opcode operand size retval bytep)
    (cl-flet ((operand (offset)
                       (setq operand (- opcode offset)))

              (fetch8 ()
                      (setq size 2
                            operand (prog1 (aref bytestr pc)
                                      (cl-incf pc))))

              (fetch16 ()
                       (setq size 3
                             operand (prog1 (+ (aref bytestr pc)
                                               (aref bytestr (1+ pc)))
                                       (cl-incf pc 2))))

              (op-stack-ref ()
                            (dispp "stack-ref" operand)
                            ($push ($peek operand)))

              (op-varref ()
                         (debugf "%s %S -> %S\n"
                                 (format-instruction "varref" operand)
                                 (aref vector operand)
                                 (symbol-value (aref vector operand)))
                         ($push (symbol-value (aref vector operand))))

              (op-varset ()
                         (debugf "%s %S <- %S\n"
                                 (format-instruction "varset" operand)
                                 (aref vector operand)
                                 (TOS))
                         (set (aref vector operand) ($pop))))
      (while (< pc end)
        (setq opcode (aref bytestr pc)
              size 1)
        (debugf "%2d  %3d   " pc opcode)
        (cl-incf pc)

        (cond
         ((= opcode 0)
          (error "Invalid byte opcode: op=%d, ptr=%d" opcode pc))

         ;; stack-ref
         ((<= 1 opcode 5)
          (operand 0)
          (op-stack-ref))

         ((= opcode 6)
          (fetch8)
          (op-stack-ref))

         ((= opcode 7)
          (fetch16)
          (op-stack-ref))

         ;; varref
         ((<= 8 opcode 13)
          (operand 8)
          (op-varref))

         ((= opcode 14)
          (fetch8)
          (op-varref))

         ((= opcode 15)
          (fetch16)
          (op-varref))

         ;; varset
         ((<= 16 opcode 21)
          (operand 16)
          (op-varset))

         ((= opcode 22)
          (fetch8)
          (op-varset))

         ((= opcode 23)
          (fetch16)
          (op-varset))

         ;; varbind
         ;; ((<= 24 opcode 31))

         ;; call
         ((<= 32 opcode 37)
          (operand 32)
          (setq bytep (op-call operand)))

         ((= opcode 38)
          (fetch8)
          (setq bytep (op-call operand)))

         ((= opcode 39)
          (fetch16)
          (setq bytep (op-call operand)))

         ;; unbind
         ;; ((<= 40 opcode 47))

         ;; pophandler
         ;; ((= opcode 48))

         ;; pushconditioncase
         ;; ((= opcode 49))

         ;; pushcatch
         ;; ((= opcode 50))

         ;; unused
         ;; ((<= 51 opcode 55))

         ((= opcode 56)
          (disp2 "nth")
          (op-subrcall #'nth))

         ((= opcode 57)
          (disp1 "symbolp")
          ($push (symbolp ($pop))))

         ((= opcode 58)
          (disp1 "consp")
          ($push (consp ($pop))))

         ((= opcode 59)
          (disp1 "stringp")
          ($push (stringp ($pop))))

         ((= opcode 60)
          (disp1 "listp")
          ($push (listp ($pop))))

         ((= opcode 61)
          (disp2 "eq")
          (op-subrcall #'eq))

         ((= opcode 62)
          (disp2 "memq")
          (op-subrcall #'memq))

         ((= opcode 63)
          (disp1 "not")
          (op-subrcall #'null))

         ((= opcode 64)
          (disp1 "car")
          ($push (car ($pop))))

         ((= opcode 65)
          (disp1 "cdr")
          ($push (cdr ($pop))))

         ((= opcode 66)
          (disp2 "cons")
          (op-subrcall #'cons))

         ((= opcode 67)
          (disp1 "list1")
          ($push (list ($pop))))

         ((= opcode 68)
          (disp2 "list2")
          (op-subrcall #'list))

         ((= opcode 69)
          (dispN "list3" 3)
          ($push ($poplistN 3)))

         ((= opcode 70)
          (dispN "list4" 4)
          ($push ($poplistN 4)))

         ((= opcode 71)
          (disp1 "length")
          ($push (length ($pop))))

         ((= opcode 72)
          (disp2 "aref")
          (op-subrcall #'aref))

         ((= opcode 73)
          (dispN "aset" 3)
          ($push (apply #'aset ($poplistN 3))))

         ((= opcode 74)
          (disp1 "symbol-value")
          ($push (symbol-value ($pop))))

         ((= opcode 75)
          (disp1 "symbol-function")
          ($push (symbol-function ($pop))))

         ((= opcode 76)
          (disp2 "set")
          (op-subrcall #'set))

         ((= opcode 77)
          (disp2 "fset")
          (op-subrcall #'fset))

         ((= opcode 78)
          (disp2 "get")
          (op-subrcall #'get))

         ((= opcode 79)
          (dispN "substring" 3)
          ($push (apply #'substring ($poplistN 3))))

         ((= opcode 80)
          (disp2 "concat2")
          (op-subrcall #'concat))

         ((= opcode 81)
          (dispN "concat3" 3)
          ($push (apply #'concat ($poplistN 3))))

         ((= opcode 82)
          (dispN "concat4" 4)
          ($push (apply #'concat ($poplistN 4))))

         ((= opcode 83)
          (disp1 "sub1")
          ($push (1- ($pop))))

         ((= opcode 84)
          (disp1 "add1")
          ($push (1+ ($pop))))

         ((= opcode 85)
          (disp2 "eqlsign")
          (op-subrcall #'=))

         ((= opcode 86)
          (disp2 "gtr")
          (op-subrcall #'>))

         ((= opcode 87)
          (disp2 "lss")
          (op-subrcall #'<))

         ((= opcode 88)
          (disp2 "leq")
          (op-subrcall #'<=))

         ((= opcode 89)
          (disp2 "geq")
          (op-subrcall #'>=))

         ((= opcode 90)
          (disp2 "diff")
          (op-subrcall #'-))

         ((= opcode 91)
          (disp1 "negate")
          ($push (- ($pop))))

         ((= opcode 92)
          (disp2 "plus")
          (op-subrcall #'+))

         ((= opcode 93)
          (disp2 "max")
          (op-subrcall #'max))

         ((= opcode 94)
          (disp2 "min")
          (op-subrcall #'min))

         ((= opcode 95)
          (disp2 "mult")
          (op-subrcall #'*))

         ;; point
         ;; ((= opcode 96))

         ;; save-current-buffer (obsolete)
         ;; ((= opcode 97))

         ;; goto-char
         ;; ((= opcode 98))

         ;; insert
         ;; ((= opcode 99))

         ;; point-max
         ;; ((= opcode 100))

         ;; point-min
         ;; ((= opcode 101))

         ;; char-after
         ;; ((= opcode 102))

         ;; following-char
         ;; ((= opcode 103))

         ;; preceding-char
         ;; ((= opcode 104))

         ;; current-column
         ;; ((= opcode 105))

         ;; indent-to
         ;; ((= opcode 106))

         ;; unused
         ;; ((= opcode 107))

         ;; eolp
         ;; ((= opcode 108))

         ;; eobp
         ;; ((= opcode 109))

         ;; bolp
         ;; ((= opcode 110))

         ;; bobp
         ;; ((= opcode 111))

         ;; current-buffer
         ;; ((= opcode 112))

         ;; set-buffer
         ;; ((= opcode 113))

         ;; save-current-buffer-1
         ;; ((= opcode 114))

         ;; unused
         ;; ((= opcode 115))

         ;; interactive-p (obsolete)
         ;; ((= opcode 116))

         ;; forward-char
         ;; ((= opcode 117))

         ;; forward-word
         ;; ((= opcode 118))

         ;; skip-chars-forward
         ;; ((= opcode 119))

         ;; skip-chars-backward
         ;; ((= opcode 120))

         ;; forward-line
         ;; ((= opcode 121))

         ;; char-syntax
         ;; ((= opcode 122))

         ;; buffer-substring
         ;; ((= opcode 123))

         ;; delete-region
         ;; ((= opcode 124))

         ;; narrow-to-region
         ;; ((= opcode 125))

         ;; widen
         ;; ((= opcode 126))

         ;; end-of-line
         ;; ((= opcode 127))

         ;; unused
         ;; ((= opcode 128))

         ((= opcode 129)
          (fetch16)
          (debugf "%s %S\n"
                  (format-instruction "constant2" operand)
                  (aref vector operand))
          ($push (aref vector operand)))

         ((= opcode 130)
          (fetch16)
          (disp1 "goto" operand)
          (setq pc operand))

         ((= opcode 131)
          (fetch16)
          (disp1 "goto-if-nil" operand)
          (unless ($pop)
            (setq pc operand)))

         ((= opcode 132)
          (fetch16)
          (disp1 "goto-if-not-nil" operand)
          (when ($pop)
            (setq pc operand)))

         ((= opcode 133)
          (fetch16)
          (disp1 "goto-if-nil-else-pop" operand)
          (if (null (TOS))
              (setq pc operand)
            ($pop)))

         ((= opcode 134)
          (fetch16)
          (disp1 "goto-if-not-nil-else-pop" operand)
          (if (TOS)
              (setq pc operand)
            ($pop)))

         ((= opcode 135)
          (disp1 "return")
          (setq retval ($pop)
                pc end))

         ((= opcode 136)
          (disp1 "discard")
          ($pop))

         ((= opcode 137)
          (disp1 "dup")
          ($push (TOS)))

         ;; save-excursion
         ;; ((= opcode 138))

         ;; save-window-excursion
         ;; ((= opcode 139))

         ;; save-restriction
         ;; ((= opcode 140))

         ;; catch
         ;; ((= opcode 141))

         ;; unwind-protect
         ;; ((= opcode 142))

         ;; condition-case
         ;; ((= opcode 143))

         ;; temp-output-buffer-setup
         ;; ((= opcode 144))

         ;; temp-output-buffer-show
         ;; ((= opcode 145))

         ;; unbind-all
         ;; ((= opcode 146))

         ;; set-marker
         ;; ((= opcode 147))

         ;; match-beginning
         ;; ((= opcode 148))

         ;; match-end
         ;; ((= opcode 149))

         ;; upcase
         ;; ((= opcode 150))

         ;; downcase
         ;; ((= opcode 151))

         ;; stringeqlsign
         ;; ((= opcode 152))

         ;; stringlss
         ;; ((= opcode 153))

         ;; equal
         ;; ((= opcode 154))

         ;; nthcdr
         ;; ((= opcode 155))

         ;; elt
         ;; ((= opcode 156))

         ;; member
         ;; ((= opcode 157))

         ((= opcode 158)
          (disp2 "assq")
          (op-subrcall #'assq))

         ((= opcode 159)
          (disp1 "nreverse")
          ($push (nreverse ($pop))))

         ((= opcode 160)
          (disp2 "setcar")
          (op-subrcall #'setcar))

         ((= opcode 161)
          (disp2 "setcdr")
          (op-subrcall #'setcdr))

         ((= opcode 162)
          (disp1 "car-safe")
          ($push (car-safe ($pop))))

         ((= opcode 163)
          (disp1 "cdr-safe")
          ($push (cdr-safe ($pop))))

         ((= opcode 164)
          (disp1 "nconc")
          (op-subrcall #'nconc))

         ((= opcode 165)
          (disp2 "quo")
          (op-subrcall #'/))

         ((= opcode 166)
          (disp2 "rem")
          (op-subrcall #'%))

         ((= opcode 167)
          (disp1 "numberp")
          ($push (numberp ($pop))))

         ((= opcode 168)
          (disp1 "integerp")
          ($push (integerp ($pop))))

         ;; unused
         ;; ((= opcode 169))

         ;; Rgoto
         ;; ((= opcode 170))

         ;; Rgotoifnil
         ;; ((= opcode 171))

         ;; Rgotoifnonnil
         ;; ((= opcode 172))

         ;; Rgotoifnilelsepop
         ;; ((= opcode 173))

         ;; Rgotoifnonnilelsepop
         ;; ((= opcode 174))

         ((= opcode 175)
          (fetch8)
          (dispN "listN" operand)
          ($push ($poplistN operand)))

         ((= opcode 176)
          (fetch8)
          (dispN "concatN" operand)
          ($push (apply #'concat ($poplistN operand))))

         ;; insertN
         ;; ((= opcode 177))

         ((= opcode 178)
          (fetch8)
          (dispp "stack-set" operand)
          ($poke (1- operand) ($pop)))

         ((= opcode 179)
          (fetch16)
          (dispp "stack-set2" operand)
          ($poke (1- operand) ($pop)))

         ;; unused
         ;; ((<= 180 opcode 181))

         ((= opcode 182)
          (fetch8)
          (dispN "discardN" operand)
          (unless (zerop (logand operand #x80))
            (setq operand (logand operand #x7f))
            ($poke (1- operand) ($pop)))
          (cl-decf top operand))

         ;; switch
         ((= opcode 183)
          (disp2 "switch")
          (let* ((arg1 ($pop))
                 (arg0 ($pop)))
            (setq pc (gethash arg0 arg1))))

         ;; unused
         ;; ((<= 184 opcode 191))

         ((<= 192 opcode 255)
          (operand 192)
          (debugf "%s %S\n"
                  (format-instruction "constant" operand)
                  (aref vector operand))
          ($push (aref vector operand)))

         (t
          (error "Not implemented opcode: op=%d, ptr=%d" opcode pc)))

        ;; Debug output
        (when (< pc end)
          (cond
           ((= size 2)
            (debugf "%12d\n" operand))
           ((= size 3)
            (debugf "%12d\n%12d\n" operand (- operand operand))))
          ($dump)
          (when (and (<= 32 opcode 39) bytep)
            (disp-header vector maxdepth))))

      (setq top unwind)
      retval)))

(defun eval-elc ()
  "*.elc を上から下へ `eval' しつつバイトコード関数を自前の `byte-code' を呼び出すように書き換える"
  (cl-labels ((walk-form (form)
                         (if (atom form)
                             form
                           (let* ((sym (car form))
                                  (func (indirect-function sym))
                                  (args (cdr form)))
                             (if (byte-code-function-p func)
                                 `(funcall-byte-code-function ',sym ,func ($pushN ',args))
                               (cons sym (mapcar #'walk-form args)))))))
    (let ((print-escape-newlines t))
      (while command-line-args-left
        (debugf "\n%s\n" (car command-line-args-left))
        (with-temp-buffer
          (insert-file-contents (car command-line-args-left))
          (condition-case nil
              (while t
                (eval (walk-form (read (current-buffer)))))
            (end-of-file)))
        (pop command-line-args-left)))))

(defun trace-elc ()
  "`eval-elc' のトレース出力する版"
  (let ((enable-trace-byte-code t)
        (enable-dump-stack nil))
    (eval-elc)))

(defun dump-elc ()
  "`trace-elc' にスタックダンプを追加した版"
  (let ((enable-trace-byte-code t)
        (enable-dump-stack t))
    (eval-elc)))

(provide 'toy-byte-code)
;;; toy-byte-code.el ends here
