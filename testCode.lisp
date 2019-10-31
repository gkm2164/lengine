(def x 3)
(def y 4)
(println "Hello, Lisp!")
(fn p [str] (println str))
(p (+ 3 4))
(fn fact [n]
  (if (> n 0)
     (* n (fact (- n 1)))
     1))

(fn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
(p (fact 3))
(p (fib 6))
