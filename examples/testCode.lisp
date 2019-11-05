(import "libs/math")
(import "libs/sequence")

(def x 3)
(def y 4)
(def 'z (println "This is lazy evaluation value"))
(fn if-test (cond? 'a 'b)
  (if cond? 'a 'b))

(if-test (eq 0 1) (println "Hello") (println "World"))
(println "Hello, Lisp!")
(fn p (str) (println str))
(p (+ 3 4))

(p (fact 3))
(p (fact-tailrec 1 3))

(p (fib 6))
(println 'z)
(println 3/5)
(println (float 3/5))
(let (name (read-line "Input your name: "))
  (println (concat "Hello, " name "!")))