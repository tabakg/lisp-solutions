;;;; Chapter 4 July 31, 2014

;;; Exercise 1
;;; a
((lambda (x)
   (cons x x))
    (car '(a b c)))

;;; b
;; the code in the book is equivalent to the following
(let ((w (car x)))
  (let ((y (+ w z)))
    (cons w y)))
;; and here is the solution
((lambda (x z)
  ((lambda (w)
     (cons w (+ w z))) (car x))) '(1 2 3) 5)

;;; Exercies 2
;; reproduced and simplified the code from page 29
(defun mystery (x y)
  (if (null y)
    nil
    (if (eql (car y) x)
      0
      (+ 1 (mystery x (cdr y))))))
;;rewritten using cond
(defun new-mystery (x y)
  (cond
    ((null y) nil)
    ((eql x (car y)) 0)
    ( t (+ 1 (new-mystery x (cdr y))))))

;;; Exercise 3
(defun square-maybe? (n)
  (cond
    ((< n 6) nil)
    ( t (* n n))))

;;; Exercise 4
;;; I feel like PG meant to say month-num, not num-month.
;;; And no, this problem is trivial and too tedius.

;;; Exercise 5
;; Here is the recursive solution.
(defun precedes (x v)
  (do-precedes x (reverse v) ()))

(defun do-precedes (x v char-set)
    (cond
      ((< (length v) 2) char-set)
      ((equal x (aref v 0)) (do-precedes x (subseq v 1) (adjoin (aref v 1) char-set)))
      ( t (do-precedes x (subseq v 1) char-set))))

;; Here is the iterative solution.
(defun itr-precedes (x v)
  (let ((rev-v (reverse v))
        (char-set ()))
    (do ((i 0 (+ i 1)))
      ((eql i (- (length rev-v) 1)) char-set)
      (if (eql x (aref rev-v i))
        (setf char-set (adjoin (aref rev-v (+ i 1)) char-set))))))

;;; Exercise 6
(defun intersperse (x lst)
  (do-intersperse x lst ()))

(defun do-intersperse (x lst ret-lst)
  (cond
    ((null lst) ret-lst)
    ((eql 1 (length lst)) (append ret-lst lst))
    (t (do-intersperse x (cdr lst) (append ret-lst
                                           (list (car lst) x))))))

(defun itr-intersperse (x lst)
  (let ((lnth-1 (- (length lst) 1))
        (ret-lst ()))
    (do ((i 0 (+ i 1)))
      ((eql i lnth-1) (append ret-lst (last lst)))
      (setf ret-lst (append ret-lst (list (nth i lst) x))))))

;;; Exercise 7
;; a
(defun one-pairs (lst)
  (let ((x (first lst))
        (y (second lst)))
    (cond
      ((and (< (length lst) 3)
            (eql 1 (abs (- x y)))) t)
      ((eql 1 (abs (- x y))) (do-one-pairs (cdr lst)))
      ( t nil ))))

;; b
(defun itr-one-pairs (lst)
  (do ((i 1 (+ i 1)))
    ((eql i (length lst)) t)
    (if (/= (abs
              (- (nth i lst)
                 (nth (- i 1) lst)))
            1)
      (return nil))))

;; c
(defun mapc-one-pairs (lst)
  (block nil
    (let ((prev nil))
      (mapc #'(lambda (x) (if (and (not (null prev))
                                   (/= (abs (- x prev))
                                       1))
                              (return nil)
                              (setf prev x)))
            lst ))))

;;; Exercise 8
(defun min-max (vctr)
  (do-min-max vctr (svref vctr 0) (svref vctr 0)))

(defun do-min-max (vctr cur-min cur-max)
  (block nil
    (if (eql 0 (length vctr))
      (return (values cur-min cur-max)))
    (cond
      ((< (svref vctr 0) cur-min) (setf new-cur-min (svref vctr 0)))
      ((> (svref vctr 0) cur-max) (setf new-cur-max (svref vctr 0)))
      ( t (and (setf new-cur-min cur-min)
               (setf new-cur-max cur-max))))
    (do-min-max (subseq vctr 1) new-cur-min new-cur-max)))

;;; Exercise 9
;; a
(defun shortest-path (start end net)
  (catch 'done
         (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (when (not (null queue))
    (dolist (x queue)
      (if (eql (car x) end)
        (throw 'done (reverse x)))))
  (if (null queue)
    nil
    (let* (( path (car queue)) (node (car path)))
        (if (eql node end)
          (reverse path)
          (bfs end
               (append (cdr queue)
                       (new-paths path node net))
               net)))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

;; b
;; The only parts that needs to be rewritten are shortest path and bfs
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (block head
         (if (null queue)
           (return-from head nil))
         (do* ((i 0 (+ i 1))
               (x (car queue) (nth i queue)))
           ((eql i (length queue)))
           (if (eql (car x) end)
             (return-from head (reverse x))))
         (let* (( path (car queue)) (node (car path)))
           (if (eql node end)
             (reverse path)
             (bfs end
                  (append (cdr queue)
                          (new-paths path node net))
                  net)))))
