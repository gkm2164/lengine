(def xs (cons 1 (cons 2 (cons 3 nil))))

(fn n-th (n list)
  (if (eq n 0)
    (head list)
    (n-th (- n 1) (tail list))))

(println xs)
(println (n-th 2 xs))