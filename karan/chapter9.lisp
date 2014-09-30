;;;; Chapter 9 September 27, 2014

;;; Exercise 1
(setf x 'a)
(setf y 'b)
(setf z '(c d))

;; (a)
`(,z ,x z)

;; (b)
`(x ,y ,@z)

;; (c)
`((,@z ,x) z)

;;; Exercise 2
; (if gaurd
;   this
;   or-this)
;
; (cond ((gaurd) this)
;       (t or-this))

;;; Exercise 3
(defmacro nth-expr (n &rest exprs)
  `(case ,n
     ,@(let ((i 0))
         (mapcar #'(lambda (expr)
                     `(,(incf i) ,expr))
                 exprs))))

;;; Exercise 4
(defmacro ntimes (n &rest body)
  `(labels ((iterator-func (index)
             (if (> index 0)
               (block nil 
                      ,@body
                      (iterator-func (1- index))))))
     (iterator-func ,n)))




;;; Exercise 5
;;; Exercise 6
;;; Exercise 7
;;; Exercise 8
