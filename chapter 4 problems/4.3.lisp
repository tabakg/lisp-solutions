;Define a strucutre to represent a tree where each node contains some data and has up to three children. Define
;a. A function to copy such a tree (so that no node in the copy is eql to a node in the original)
;b. A function that takes an object and suc ha tree, and returns true if the object is eql to the data field  of one of th nodes.

(defstruct (node)
	   elt (l nil) (c nil) (r nil)
	   )

(defun copy-trt (trt)
       (if (null trt)
	   nil
	   (make-node
	     :elt (node-elt trt)
	     :l (copy-trt (node-l trt))
	     :c (copy-trt (node-c trt))
	     :r (copy-trt (node-r trt))
	     )))

(copy-trt
  (make-node
    :elt 5
    :l (make-node :elt 1)
    :c (make-node :elt 2)
    :r (make-node :elt 3)
    ))

(defun my-eql-trt (trt1 trt2)
       (or (and
	     (null trt1)
	     (null trt2)) 	;if both are null, return true.
	   (and			;otherwise, check value and children.
	     (eql (node-elt trt1) (node-elt trt2))
	     (eql-trt (node-l trt1) (node-l trt2) )
	     (eql-trt (node-c trt1) (node-c trt2) )
	     (eql-trt (node-r trt1) (node-r trt2) )
	     )))

(my-eql-trt 
  (make-node
    :elt 5
    :l (make-node :elt 1)
    :c (make-node :elt 2)
    :r (make-node :elt 3)
    )
  (make-node
    :elt 5
    :l (make-node :elt 1)
    :c (make-node :elt 2)
    :r (make-node :elt 3)
    ))


