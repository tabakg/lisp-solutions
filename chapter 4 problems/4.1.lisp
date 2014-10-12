;Chapter 4

;1. Define a function to take a square array and rorate it 90 degrees clockwise:


(defun range (n)
       (loop for i from 0 below n by 1
	 collect i ))

(defun quarter-turn (arr)
       (let ( (dim (array-dimensions arr ) )
	      )
	 (let ( (arr_out (make-array dim :initial-element nil))
		)
	   (loop for i from 0 below (car dim) 
		 do (loop for j from 0 below (car dim) 
		       do (setf (aref arr_out i j) (aref arr (- (car dim) (+ j 1) ) i) )
		       ))
	   arr_out
	   )
	 )
)

(quarter-turn #2A((a b) (c d)) )



