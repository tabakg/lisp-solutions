;4.6
; The contents of any hash table can be described by an assoc-list whose elements are (k . v), for each key0value pair in the hash-table
; a. Take assoc-list and return a hash-table
; b. Take hash-table and return assoc-list


(defun hash-to-assoc (hash &optional asso) 
	   (progn
	     (if (not (boundp asso))
		 (setf asso nil))
	     (maphash #'(lambda (k v)
			     (append asso (list (cons k v)))) hash)
	     ;(format t "~A" asso)
	     asso))

(defun assoc-to-hash (asso &optional hash)
       (if hash						;check if there is already some hash table
	   (if asso					;if elements left in associated list
	     (let ((el (car asso) )
		  (asso-left (cdr asso)) )
		  (progn
		    (setf (gethash (car el) hash) (cdr el) )
		    (assoc-to-hash asso-left hash)))
	     hash)					;no elements in assoc -> return hash table
	   (assoc-to-hash asso (make-hash-table))))



