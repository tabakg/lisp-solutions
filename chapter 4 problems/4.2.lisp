;use reduce to define copy-list and reverse (for lists)

(defun my-copy-list (lst)
        (reduce #'(lambda (x y) (append x (list y))) ;append lists using reduce
		(cons (list (car lst)) (cdr lst)) )  ;set initial element to list of itself
       )

(my-copy-list '(a b c d e f))

(defun my-reverse (lst)
       (reduce #'(lambda (x y) (append (list y) x )) ;append lists using reduce
	       (cons (list (car lst)) (cdr lst)) )  ;set initial element to list of itself     
       )

(my-reverse '(a b c d e f))

