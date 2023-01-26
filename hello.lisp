(def v 3)
(def str "Something Wonderful!")
(def name "YoonJung")

(println v)
(println "1234")
(println (+ 2 3))
(println "Hello World!")
(println #\n)
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

(fn print-seq ()
  (println seq3))

(fn other-thing ()
  (println name))

(print-seq)

(println "Hello World!")

(print-seq)
(other-thing)
(print-seq)
(other-thing)