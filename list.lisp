;;; test code for list operation

(def xs (cons 1 (cons 2 (cons 3 nil)))) ; test list concatenation

(fn n-th (n list)   ; extract n-th item from list
  (if (eq n 0)
    (head list)
    (n-th (- n 1) (tail list))))

(println xs)
(println (n-th 2 xs))

(fn reverse-loop (acc xs)
  (if (eq (len xs) 0)
    acc
    (reverse-loop (cons (head xs) acc) (tail xs))))

(fn reverse (xs)
  (reverse-loop nil xs))

(println (reverse xs))