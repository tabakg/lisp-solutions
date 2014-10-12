;Problem 3.2

(defun add-element (lst el)
  (if lst
    (if (eql el (car lst))
      lst
      (cons (car lst) (add-element (cdr lst) el))
      )
    (list el)
    )
  )

(defun new-union (lst1 lst2)
  (if lst2
    (new-union
      (add-element lst1 (car lst2))
      (cdr lst2))
    lst1
    )
  )

(new-union '(a b c) '(b a d))

;Problem 3.3
;Define a function that takes a list and returns a list indicating the number of times each (eql) element appears, sorted from most to least common.

;find number of times el appears in lst
(defun num (el lst)
  (if lst
    (if (eql el (car lst))
      (+ 1 (num el (cdr lst)))
      (num el (cdr lst)))
    0))

(defun el-dot-num (el lst)
  (cons el (num el lst)))

(defun purge (lst el)
  (if lst
    (if (eql el (car lst))
      (purge (cdr lst) el)
      (cons (car lst) (purge (cdr lst) el) ))
    nil))

(defun occurrences-no-order (lst)
  (if lst
    (cons (el-dot-num (car lst) lst)
          (occurrences-no-order (purge (cdr lst )
                                        (car lst))))
    nil))

(defun occurrences (lst)
  (sort (occurrences-no-order lst) #'(lambda (x y) (> (cdr x) (cdr y))) )
  )

; used to test

(set 'mylist '(a b a d a c d c a))

(purge mylist 'a)

(eql 'a (car mylist))

(num 'a mylist)

(el-dot-num 'a mylist)

(occurrences mylist)


;4. Returns nil becuase memeber uses eql, which checks if the object is the same object. This will only work for atoms.
(eql '(a b) '(a b))

;5.

; recursion
(defun add-one (lst)
  (if lst
    (cons (+ 1 (car lst))
        (add-one (cdr lst)) )
    nil
    ))


(defun  pos+ (lst)
  (if lst
    (cons (car lst)
          (add-one (cdr lst))
          )
    nil
    )
  )

(pos+ '(7 1 5 4))


;iteration:
(defun pos+it (lst)
  (loop 
    for i from 0 to (- (length lst) 1)
    do (setf (nth i lst) (+ i (nth i lst)))
    )
  lst
  )

(pos+it '(7 1 5 4))


;mapcar

(defun pos+mapcar (lst)
  (mapcar #'+ lst 
          (loop for n from 0 below (length lst) by 1
                collect n)))

(pos+mapcar '(7 5 4 1))
;Gov cimmission decides lists shoudl be represented by using the cdr to point to hte first element and car to point to the rest of the list. 6 Define gov version of 
;
;a. cons would effectively have its arguments switched. 
;
;b. list would be constructed by using car to point at successive elements.
;
;c,d length and member would just have car and cdr switched in their definitions.



;7 Modify the program in 3.6 to use fewer cons cells (use dotted lists).
; Use compression of the form ((1.3) (0.4)). Not worth writing out.

;8. Print in dot notation

(defun showdots (lst)
  (if (null lst)
    "NIL"
    (concatenate 'string "(" (string (car lst)) "."
            (showdots (cdr lst))
            ")"))
  )

(showdots '(a c d e ))

(setf mylist '(a v c))

;9. Find the longest finite path through a network. 
;It may not exist. There may be infinitely many finite paths of increasing lengths (in the case of a cycle).



