;5  
; define bst-adjoin. this function should take the same arguments as bst-insert, but should only insert the object if there is 
; nothing eql to it in the tree

(defun bst-adjoin (obj bst <)
       (if (null bst)
	   (make-node :elt obj)
	   (let ((el (node-elt bst)))
	     (if (eql obj elt)
		 bst     					; Seems to already be the case
		 (if
		   (funcall < obj elt)
		   (make-node
		     :elt el
		     :l (bst-insert obj (node-l bst) <)
		     :r (node-r bst)
		     )
		   (make-node
		     :elt el
		     :l (bst-insert obj (node-r bst) <)
		     :r (node-l bst)
		     )
		   )
		 )
	       )	
	   )
       )

