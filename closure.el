;;; -*- lexical-binding: t; -*-

(defun make-closure ()
  (let ((count 0))
    (lambda () (setq count (1+ count)))))

(setq cls (make-closure))
(print cls)

(print (funcall cls))
(print cls)

(print (funcall cls))
(print cls)
