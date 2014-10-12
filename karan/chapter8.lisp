;;;; Chapter 8 September 20, 2014

;;; Exercise 1
; Yes, if you use the function MAKE-SYMBOL and no if you use the Lisp reader.
; Also yes if they are in different packages.

;;; Exercise 2
; Let's assume ASCII encoding for simplicity. A character will take up one
; byte and pointer will take 32 bits (4 bytes).
; String: The string needs a pointer and three characters so 8 bytes.
; Symbol: The symbol needs a name, 4 bytes, 5 pointers. Assuming the value,
; function, and plist are not initialized this will take 24 bytes.

;;; Exercise 3
; You want to use string as arguments for thi macro because if you pass in
; symbols they will automatically get interned then you will not be able to use
; those symbols normally in your code.

;;; Exercise 4
; Instead copying the entire code from the last chapter into this solution set
; I'll just write a simplified data structure in a package called "RING" and a
; simple function that uses that data structure in another package called
; "FILE".
(defpackage "RING"
  (:use "COMMON-LISP") 
  (:export "STK-PUSH" "STK-POP"))
(in-package ring)

(let ((stk ()))
  (defun stk-push (val)
    (setf stk (push val stk)))
  (defun stk-pop ()
    (setf x (car stk))
    (setf stk (cdr stk))
    x))

(defpackage "FILE"
  (:use "COMMON-LISP"))
(in-package file)

(ring:stk-push 3)
(ring:stk-push 2)
(ring:stk-push 1)

(ring:stk-pop)
(ring:stk-pop)
(ring:stk-pop)

(in-package common-lisp-user)

(setf lst ())
(setf (car (push 10 lst)) (1+ (car (push 1 lst))) ) 
lst

(setf lst ())
(car (push 1 lst))
(1+ (car (push 1 lst)))
lst
