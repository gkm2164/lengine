(module ReadLine)

(println "Type anything =>")
(def s (read-line))

(println (+ "You typed: " s))

(println "Type any number =>")
(def v (int (read-line)))

(println (+ "<Parsed number> + 3 = " (+ v 3)))

(println "Type a number =>")
(def x (int (read-line)))
(println "Type a number =>")
(def y (int (read-line)))

(if (< x y)
  (println "y is bigger than x")
  (if (= x y)
    (println "x and y are identical")
    (println "x is bigger than y")))