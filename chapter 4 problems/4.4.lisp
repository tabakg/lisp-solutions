;4.4
; Take a BST and return elements from greatest to least

;I didn't bother to test because this is a modification of traverse in the book.

(defun bst-GTL (bst)
       (when bst
	     (bst-GTL fn (node-r bst))
	     (node-elt bst)
	     (bst-GTL fn (node-l bst))
	     ))

