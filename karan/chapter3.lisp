(defun second-word (str)
  (let ((p1 (+ (position #\  str) 1)))
  (subseq str p1 (position #\  str :start p1))))

(defun second-word (str)
  (let ((p1 (+ (position #\ str) 1)))
  (subseq str p1 (position #\ str :start p1))))
