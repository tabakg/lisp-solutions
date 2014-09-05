; 1 a. We add 5-1 and 3+7
(+ (- 5 1) (+ 3 7))

; 1 b. make a list of 1 and 2+3
( list 1 ( + 2 3))

; 1 c. 1 is not a list, so return the second argument.
(if (listp 1)
    (+ 1 3)
    (+ 3 4))

; 1 d. make a list. The first element is nil (aka false) since 3 is not a 
; list. The second element is 1+2
(list 
  (and (listp 3) t)
  (+ 1 2))

; 2 distinct expressions that return (a b c)

(cons 'a '(b c))
(cons 'a (cons 'b '(c)) )
(cons 'a (cons 'b (cons 'c '() )))

; 3 Return the fourth element of a list

(defun blah(x)
       (car(cdr(cdr(cdr x)))))

(blah '(a b c d))

; 4 greater of two arguments

(defun greater (x y)
       (if (> x y)
	   x
	   y))
(greater 3 5)
(greater 6 2)

; 5a the first line checks x is not empty.
; next, we check each element for being empty.
; thus enigma returns true for a list x if x has an element nil
(defun enigma (x)
       (and (not (null x))
	    (or (null (car x))
		(enigma (cdr x)))))

(enigma '(nil))

; as before, ignore first part
; if first element of y is equal to x
; call the remainder of y recursively with same x
; increment z at each layer x is not the first element of y
; thus, this counts the number of elements in y before the element x
(defun mystery (x y)
       (if (null y)
	   nil
	   (if (eql (car y) x)
	       0
	       (let ((z (mystery x (cdr y))))
		    (and z (+ z 1))))))

(mystery 'a '(d s a c d s a d f ))

; 6 what could occur in place of x?
;a. (car (x (cdr '(a (b c) d)))) 
;b
;x = b
;b (x 13  (/ 1 0))
;13
;x = or
;c (x #'list 1 nil)
;(1)
;x = apply. The function defined returns lists with values of 
; lists that were empty and had the element 1 appended to the left.

;7 take a list and return true iff one element is a list

(defun g(x)
       (and (not (null x))
       (or (listp (car x))
	    (g (cdr x)) )))
(g '(a (v 2) d))
(g '(a b c))

;8 iterative and recursive functions that 
;a. takes positive integers an prints that many dots
(defun dots(x)
       (format t ".")
       (if (not (eql x 0))
	   (dots (- x 1))
	   nil))

(defun dots2(x)
       (do ((i 0 (+ i 1)))
	   ((> i x))
	   (format t ".")))
(dots2 5)

;8b. take a list and return the number of times a symbol occurs
(defun number(lst sym)
       (if (null lst)
	   0
	   (if (eql (car lst) sym)
	       (+ 1 (number (cdr lst) sym))
	       (number (cdr lst) sym))))

(number '(a b c d c d d s c d d r a d c a) 'a)

(defun number2(lst sym)
       (setf count 0)
       (do ((i lst (cdr lst)))
	   ((eql i nil))
	    (if (eql (car i) sym)
		(setf count (+ 1 count))))
       count)

(number '(a b c d c d d s c d d r a d c a) 'a)

;9 first implementataion had the issue that applying the remove didn't affect 
; the original lst, but only returned a new list instead.
; in the second implementation there is no base case.
; I fixed both below.

(defun summit (lst)
       (apply #'+ (remove nil lst)))

(summit '(3 4 5 nil 3 4 nil 3))

(defun summit2(lst)
       (if (null lst)
	   0
	   (let ((x (car lst)))
	    (if (null x)
		(summit2 (cdr lst))
		(+ x (summit2 (cdr lst)))))))

(summit2 '(3 4 5 nil 3 4 nil 3))
