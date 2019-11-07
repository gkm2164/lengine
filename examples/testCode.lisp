(import "libs/math")
(import "libs/sequence")

(def x 3)
(def y 4)
(def 'z (println "This is lazy evaluation value"))
(fn if-test (cond? 'a 'b)
  (if cond? 'a 'b))

;;; quit should not be called
(if-test (= 0 1) (quit) (println "World"))

(println "Hello, Lisp!")
(fn p (str) (println str))
(p (+ 3 4))

(p (fact 3))
(p (fact-tailrec 1 3))

(p (fib 6))
;;; lazy call test
(println 'z)

;;; print ratio type
(println 3/5)

;;; test casting ratio to float
(println (float 3/5))

;;; let test
(let (name (read-line "Input your name: "))
  (println (concat "Hello, " name "!")))

;;; do test
(fn run-something () (do
  (println (+ 3 5))
  (println (+ 4 6))
  (println (/ 1 #C(1.0 2.0)))))

(run-something)

;;; loop test
(loop for x in '(1 2 3 4 5)
      for y in '(2 3 4 5 6)
      (do (println x)
          (+ x y)))

(println (map '(1 2 3 4 5)
              (lambda (x) (+ x 1))))

;;; override test
(fn p-test (a b) (println a))
(fn p-test (a b c) (println b))

(p-test "a" "b") ;;; should print a
(p-test "a" "b" "c") ;;; should print b

(fn fact-match (acc 1) acc)
(fn fact-match (acc n) (fact-match (* acc n) (- n 1)))

(println (fact-match 1 5))