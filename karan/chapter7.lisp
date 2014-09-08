;;;; Chapter 7 September 7, 2014

;;; Exercise 1
(defun read-helper (file-name read-function)
  (with-open-file (str file-name :direction :input)
    (let ((acc nil))
      (do ((line (funcall read-function str nil 'eof)
                 (funcall read-function str nil 'eof)))
        ((eql line 'eof) acc)
        (setf acc (append acc (list line)))))))

(defun file-to-strings (file-name)
  (read-helper file-name #'read-line))

;;; Exercise 2
(defun file-to-expressions (file-name)
  (read-helper file-name #'read))

;;; Exercise 3
(defun strip-comments (old-file new-file)
  (with-open-file (new new-file :direction :output
                       :if-exists :supersede)
    (let ((lines (file-to-strings old-file)))
      (dolist (line lines)
        (cond ((zerop (length line)) (format new "~%"))
              ((eql #\% (char line 0)) nil)
              ((position #\% line) (format new "~A~%" (subseq line 0 (position #\% line))))
              (t (format new "~A~%" line)))))))






(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1
    (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new  b) (buf-end   b))))


