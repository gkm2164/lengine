;;; test code for list operation
(import "libs/default")

(def xs (cons 1 (cons 2 (cons 3 nil)))) ; test list concatenation

(fn n-th (n list)   ; extract n-th item from list
  (if (eq n 0)
    (head list)
    (n-th (- n 1) (tail list))))

(println xs)
(println (n-th 2 xs))

(println (reverse xs))

(println (reverse (list 1 2 3 4 5)))
