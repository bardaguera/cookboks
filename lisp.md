## Пять раз напечатать Hello World
```lisp
(defun fn (n)
    (dotimes (i n) (format t "Hello World~%")))
```

## Напечатать несколько раз каждый элемент листа
```lisp
(defun f (n list) 
  (loop for cons on list do
        (dotimes (i n)
            (format t "~a~%" (car cons)))))
```

## Степень
```lisp
(defun power(b p)
    (if(= p 1)
       b
       (* b (power b (1- p)))))
```
## Факториал
```lisp
(defun factorial(n)
    (if(= n 0)
       1
       (* n (factorial(1- n)))))
```
## e^n
```lisp
(defun fn(x i)
    (if(= i 0)
    1
    (+ (/ (power x i) (factorial i)) (fn x (1- i)))))

(defun w(n)
    (format t "~,4f~%" (fn n 9)))
```
## Чтение в 2 параметра -- n и лист -- из stdin
```lisp
(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(format t "~{~d~%~}" (f (read) (read-list)))
```
## Чтение в лист длиной, определенной в первом числе
```lisp
(defun read-list (i)
    (let ((n (read *standard-input* nil)))
        (if (= 0 i)
            nil
            (cons n (read-list (1- i))))))
            
(loop for cons on (read-list (read)) do
      (w (car cons)))
```
