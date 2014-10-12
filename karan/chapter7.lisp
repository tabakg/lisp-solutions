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

;;; Exercies 4
(defun pretty-print-2d (ary)
  (let* ((dimensions (array-dimensions ary))
         (i-max (nth 0 dimensions))
         (j-max (nth 1 dimensions)))
    (do ((i 0 (1+ i)))
      ((= i i-max))
      (do ((j 0 (1+ j)))
        ((= j j-max))
        (format t "~10,2F " (aref ary i j)))
      (format t "~%"))))

(setf input-4 #2A ((1 200.1) (1.11 2.002) (1.4 1.5)))
(pretty-print-2d input-4)

;;; Exercies 5
;;; First I'll reproduce the helper functions from the text.
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

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new   b) -1 (buf-end  b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
    ((> i (buf-end b)))
    (princ (bref b i) str)))

(defun stream-subst (old new in out comp-func)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
      ((eql c :eof))
      (cond ((funcall comp-func c (char old pos))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

;; And finally here we are at the solution.
(defun char=wildcard (c old-char)
  (if (char= #\+ old-char)
    T
    (char= c old-char)))

(defun file-subst-wildcard (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst old new in out #'char=wildcard))))

;;; Exercise 6
;;; Similar to the previous solution, I'll override the character comparing
;;; function. But this time the old "string" will actually be a list called the
;;; pattern. Pattern is made up of strings and symbols. String literals in the 
;;; pattern will be treated as actual string literals. 
;;; But three, not strings, have special meaning. Consider the examples below.
;;; ("id" '+)    digits strings, such as "id2" "id3, ...
;;; ("Mr " 'a0)  alphanumeric, strings such "Mr Z" "Mr a" "Mr 2"...
;;; ("Mrs " '*)  any character, strings such as "Mrs w" "Mrs !"...
;;; Also note I had to change the accessor function in and the length function
;;; in stream-subst.

;;; Just found out about functions like ALPHANUMERICP etc... oh well next
;;; time...
(defun char=pattern (c old-char)
  (let ((nums  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
        (alpha '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
      (cond ((symbolp old-char)
             (cond ((eql '+ old-char)
                    (member c nums))
                   ((eql 'a0 old-char)
                    (or (member c nums)
                        (member c alpha)))
                   ((eql '* old-char)
                    T)))
            (t
             (char= c old-char)))))

(defun pattern-index (pattern pos)
  (let ((elm (car pattern)))
    (cond ((typep elm 'string)
           (if (< pos (length elm))
             (char elm pos)
             (pattern-index (cdr pattern) (- pos (length elm)))))
          (t                          ; here elm must be a symbol 
           (if (zerop pos)
             elm
             (pattern-index (cdr pattern) (1- pos)))))))

(defun pattern-length (pattern &optional (current-length 0))
  (let ((elm (car pattern)))
    (cond ((null elm)
           current-length)
          ((typep elm 'string)
           (pattern-length (cdr pattern) (+ current-length (length elm))))
          (t      ; this is symbol case
           (pattern-length (cdr pattern) (1+ current-length))))))

(defun stream-subst-pattern (old new in out comp-func)
  (let* ((pos 0)
         (len (pattern-length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
      ((eql c :eof))
      (cond ((funcall comp-func c (pattern-index old pos))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0)))
      )
    (buf-flush buf out)))

;; And finally here we are at the solution.
(defun file-subst-pattern (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst-pattern old new in out #'char=pattern))))
