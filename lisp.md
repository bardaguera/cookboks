```lisp
(defun fn (n)
    (dotimes (i n) (format t "Hello World~%")))
```
```lisp
(defun f (n list) 
  (loop for cons on list do
        (dotimes (i n)
            (format t "~a~%" (car cons)))))
```

Чтение в 2 параметра -- n и лист -- из stdin
```lisp
(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(format t "~{~d~%~}" (f (read) (read-list)))
```
