(def v 3)
(def str "Something Wonderful!")
(def name "YoonJung")

(println v)
(println #\n)
(println "Hello World")
(println (+ 2 3))
(println (+ 2 5.2))
(println (* 2 (/ (- 3 2) 5.3)))
(println (+ v 10))
(println str)

(println (+ "Hello, " name "!"))

(println (+ name " is beautiful woman!"))

(def seq [1 2 3 4 5 6 7 8 9 10])
(def seq2 [1 2 3 4 5 [1 2 3 4 5]])
(def seq3 [1 2 3 name])

(println seq)
(println seq2)
(println seq3)

(fn print-seq (x y)
  (println x))

(fn other-seq (x)
  (println x))

(print-seq "Hello" "World")

(println "Hello World!")

(print-seq "something" "World")
(other-seq "something2")
(print-seq "something" "World")
(other-seq "something3")
