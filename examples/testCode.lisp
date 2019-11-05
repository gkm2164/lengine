(import "libs/math")
(import "libs/sequence")

(def x 3)
(def y 4)
(def z? (println "Hello, Lisp2!"))
(println "Hello, Lisp!")
(fn p (str) (println str))
(p (+ 3 4))

(p (fact 3))
(p (fact-tailrec 1 3))

(p (fib 6))
(z?)
(println 3/5)
(println (float 3/5))
(let name (read-line "Input your name: ")
  (println (concat "Hello, " name "!")))