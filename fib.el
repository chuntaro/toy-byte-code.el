;;; -*- lexical-binding: t; -*-

(defun fib (n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (t (+ (fib (1- n))
         (fib (- n 2))))))

(print (fib 2))
