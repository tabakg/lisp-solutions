;;;; Chapter 6 August 13, 2014

;;; Exercise 1
(defun tokens (str &optional (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
      (let ((p2 (position-if #'(lambda (c)
                                  (not (funcall test c)))
                             str :start p1)))
        (cons (subseq str p1 p2)
              (if p2
                (tokens str test p2)
                nil)))
      nil)))
(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

;;; Exercise 2
;;; I'm not sure what PG had in mind for :key.
(defun bin-search (obj vec &key (test #'eql) (start 0) (end (- (length vec) 1)))
    (and (not (eql -1 end))
         (finder obj vec test start end)))
(defun finder (obj vec test start end)
  (let ((range (- end start)))
    (if (zerop range)
      (if (funcall test obj (svref vec start))
        obj
        nil)
      (let* ((mid (+ start (round (/ range 2))))
             (obj2 (svref vec mid)))
        (if (< obj obj2)
          (finder obj vec test start (- mid 1))
          (if (> obj obj2)
            (finder obj vec test (+ mid 1) end)
            obj))))))

;;; Exercise 3
;; Named after the same method in ruby.
(defun airity (&rest args)
  (length args))

;;; Exercise 4
(defun most-elem (fn lst)
  (if (null lst)
    nil
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
              (when (> score max)
                (setf wins obj
                      max score))))
      wins)))
;;; Assumes lst is a list of at least two elements.
;;; putting if statments isn't hard, just unnecessary clutter.
(defun most (fn lst)
  (let* ((first-max (most-elem fn lst))
         (second-max (most-elem fn (remove first-max
                                           lst
                                           :count 1))))
    (values first-max second-max)))

;;; Exercise 5
;;; First I'll define the filter function from page 105.
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))
;; All we need to do is pass a function to filter that is logically opposite to
;; the one we passed in to our-remove-if.
(defun our-remove-if (fn lst)
  (filter #'(lambda (x) 
              (and (not (funcall fn x))
                   x))
          lst))
(our-remove-if #'oddp '(1 2 4 5 6))

;;; Exercise 6
(let ((val nil))
  (defun greatest-val-so-far (n)
    (if (or (null val)
            (> n val)) 
      (setf val n)
      (setf val val)))) ; For some reason it forgets val exists unless I assign it.

;;; Exercise 7
(let ((val nil))
  (defun greater-than-last (n)
    (setf old-val val)
    (setf val n)
    (if (null old-val)
      nil
      (if (> n old-val)
        T))))

;;; Exercise 8
(defun expensive (n)
  (format t "taking so long..")
  (+ 10 n))

(let ((ht (make-hash-table)))
  (defun frugal (n)
    (multiple-value-bind (value in-ht?) (gethash n ht)
      (if in-ht?
        value
        (setf (gethash n ht) (expensive n))))))

;;; Exercise 9
(let ((*print-base* 8))
  (princ 32) 
  (defun base8-apply (f &rest args)
    (apply f args)))
;; This doesn't change the printed base for some reason.
;; Can't seem to figure it out.
(base8-apply #'(lambda (n) (princ n)) 32)

