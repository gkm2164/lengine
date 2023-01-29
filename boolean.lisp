(def T true)
(def F false)
(def bool-list [T F])

(def lt (< 3 5))
(def gt (> 3 5))
(def a (and T T))
(def o (or T T))
(def e (= T T))
(def ne (/= T F))

(println T)
(println F)
(println bool-list)
(println lt)
(println gt)
(println a)
(println o)
(println e)
(println ne)

(def n1 3)
(def n2 5)

(if (< n1 n2)
 (println "n2 is bigger than n1")
 (println "n1 is bigger than n2"))

(if (> n1 n2)
  (println "n1 is bigger than n2")
  (println "n2 is bigger than n1"))

(println "Define range 1")
(def r1 (range 1 5))
(println "Define range 2")
(def r2 (range 1 5))

(println
    (loop for x in r1
          for y in r2
          (+ x y)))