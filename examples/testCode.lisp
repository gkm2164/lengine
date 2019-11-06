(import "libs/math")
(import "libs/sequence")

(def x 3)
(def y 4)
(def 'z (println "This is lazy evaluation value"))
(fn if-test (cond? 'a 'b)
  (if cond? 'a 'b))

;;; quit should not be called
(if-test (eq 0 1) (quit) (println "World"))

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

(fn run-something () (do
  (println (+ 3 5))
  (println (+ 4 6))
  (println (/ 1 #C(1.0 2.0)))))

(loop for x in '(1 2 3 4 5)
      for y in '(2 3 4 5 6)
      (do (println x)
          (+ x y)))