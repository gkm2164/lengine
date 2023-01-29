(def v 3)
(def str "Something Wonderful!")
(def name "YoonJung")
(def seq [1 2 3 4 5])

(println (fold (range 1 10) 0 (lambda (acc elem) (+ acc elem))))