; 2
(defun ou (uset set1 set2)
  (if (and (null set1)
           (null set2))
           uset
           (ou (aiu (aiu uset (car set1)) (car set2)) (cdr set1) (cdr set2))))

(defun aiu (set elm)
  (if (or (member elm set)
          (null elm))
      set
      (append set (list elm))))

; 3
(defun occurences (lst)
  (do-occurences nil lst))

(defun do-occurences (o-counter lst)
  (if (null lst)
      o-counter
      (if (assoc (car lst) o-counter)
        (do-occurences (inc-elm-counter (car lst) o-counter nil) (cdr lst))
        (do-occurences (append o-counter (list (cons (car lst) 1))) (cdr lst)))))

(defun inc-elm-counter (elm oldlst newlst)
  (if (null oldlst)
      newlst
      (if (equal elm (car (car oldlst)))
          (inc-elm-counter elm (cdr oldlst) (append newlst (list (cons (car (car oldlst)) (+ 1 (cdr (car oldlst)))))))
          (inc-elm-counter elm (cdr oldlst) (append newlst (list (cons (car (car oldlst)) (cdr (car oldlst)))))))))

; 4
; member uses eq to compare the elements, and the two lists allocate memory in different places
; this functional call will return true if you pass :test #'equal

; 5
(defun recursive-post+ (lst)
  (do-recursive-post+ lst 0))

(defun do-recursive-post+ (lst idx)
  (if (null lst)
      lst
      (cons (+ idx (car lst)) (do-recursive-post+ (cdr lst) (+ 1 idx)))))

(defun iterative-post+ (lst)
  (setf new nil)
  (if (null lst)
    lst
    (do ((i 0 (+ i 1)))
        ((> i (length (cdr lst))) new)
      (setf new (append new (list (+ i (nth i lst))))))))

(defun mapcar-post+ (lst)
  (setf idxlst (indexlist lst 0))
  (mapcar #'(lambda (x y)(+ x y)) lst idxlst))

(defun indexlist (lst idx)
  (if (null lst)
    lst
    (cons idx (indexlist (cdr lst) (+ 1 idx)))))

; 6
; WTF Paul Graham?

; 7
; The n-elts function can be optimized. The original returns a list with two
; elements. This representation requires two conses. With a dotted list you 
; can achieve a similar representation with only one cons cell.
(defun n-elts (elt n)
  (if (> n 1)
    (cons n elt)
    elt))

; 8
(defun showdots (lst)
  (if (null lst)
    (format t "NIL")
    (progn (format t "(~A . " (car lst))
         (showdots (cdr lst))))
  (format t ")"))

; 9
; My function returns the longest path without cycles.
; I use a variant of Breadth First Search. I keep track of possible paths to 
; store the solution paths I find. Because of BFS The last solution should be 
; the longest path. I keep track of previously visited nodes to prevent going
; in cycles.
(defun longest-path (start end net)
  (bfs end (list (list start)) net nil nil))

(defun bfs (end queue net possible visited)
  (if (null queue)
    (reverse (last possible))
    (let ((path (car queue)))
      (let ((node (car path)))
          (if (member node visited)
            (bfs end
                 (cdr queue)
                 net
                 (append possible (list path))
                 (append visited (list node)))
            (if (eql node end)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net
                   (append possible (list path))
                   (append visited (list node)))
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net
                   possible
                   (append visited (list node)))))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))
