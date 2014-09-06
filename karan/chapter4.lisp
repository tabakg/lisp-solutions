;;;; Chapter 4 July 31, 2014

;;; Exercise 1
(defun iterate-indicies (arr)
  (let ((copy (make-array (array-dimensions arr)))
        (size (- (car (array-dimensions arr)) 1)))
    (do ((i 0 (+ i 1))) ((> i size) copy)
      (do ((j 0 (+ j 1))) ((> j size))
        (setf (aref copy (nth 0 (rotation-coordinates i j size)) (nth 1 (rotation-coordinates i j size))) (aref arr i j))))))

(defun rotation-coordinates (i j n)
  (list j (pivot-around-middle i n)))

(defun pivot-around-middle (i n)
  (if (and (oddp n) (eq i (/ n 2)))
    i
    (if (> (/ n 2) i)
      (+ (/ n 2) (- (/ n 2) i))
      (- (/ n 2) (- i (/ n 2))))))


;;; Exercise 2
(defun our-copy-list (lst)
  (reduce #'(lambda (x y) (cons x y)) lst :from-end t :initial-value nil))

;; similar to the one above except we start from the beginning and swap the cons
;; order
(defun our-list-reverse (lst)
  (reduce #'(lambda (x y) (cons y x)) lst :initial-value nil))

;;; Exercise 3
;; our primary data structure
(defstruct node
  data (left nil) (middle nil) (right nil))

;;; we assume the data is an atom so there is no point in duplicating it
(defun copy-tri-tree (tree)
  (if tree
    (make-node
      :data   (node-data tree)
      :left   (copy-tri-tree (node-left tree))
      :middle (copy-tri-tree (node-middle tree))
      :right  (copy-tri-tree (node-right tree)))
    nil))

(defun tri-tree-member (tree obj)
  (if tree
    (or (eql (node-data tree) obj)
        (tri-tree-member (node-left tree) obj)
        (tri-tree-member (node-middle tree) obj)
        (tri-tree-member (node-right tree) obj))
    nil))

;;; Exercise 4
(defstruct bst-node
  data (left nil) (right nil))

(defun bst-in-order (bst)
  (if bst
    (append (bst-in-order (bst-node-left bst))
            (list (bst-node-data bst))
            (bst-in-order (bst-node-right bst)))
    nil))

;;; Exercies 5

;; we assume the tree only stores integers
;; prety much the same as the code in figure 4.5, what was the point of that PG?
(defun bst-adjoin (bst obj)
  (if (null bst)
    (make-bst-node :data obj)
    (let ((elm (bst-node-data bst)))
      (if (eql elm obj)
        bst
        (if (< obj elm)
          (make-bst-node
            :data elm
            :left (bst-insert (bst-node-left bst) obj)
            :right (bst-node-right bst))
          (make-bst-node
            :data elm
            :left (bst-node-left bst)
            :right (bst-insert (bst-node-right bst) obj)))))))

;;; Exercies 6
(defun list-to-hash (lst)
  (let ((ht (make-hash-table)))
    (and (do-list-to-hash lst ht)
         ht)))
(defun do-list-to-hash (lst ht)
  (if (null lst)
    ht
    (let ((key (car (car lst))) (value (cdr (car lst))))
      (and (setf (gethash key ht) value)
           (do-list-to-hash (cdr lst) ht)))))

(defun hash-to-list (ht)
  (let ((lst ()))
    (or (maphash #'(lambda(k v)
                      (setf lst (append lst (list (cons k v)))))
                  ht)
         lst)))
