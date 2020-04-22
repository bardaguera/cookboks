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
