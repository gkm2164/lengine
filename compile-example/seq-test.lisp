(module Seq)
(def seq [1 2 3 4 5])
(def fold-result (fold (range 1 10) 0 (lambda (acc elem) (+ acc elem))))

(println "Testing sequence")
(assert "sequence is sequence" (= seq [1 2 3 4 5]))

(println "Testing fold function")
(assert "fold-result should be 45" (= fold-result 45))

(fn sum (seq)
  (fold seq 0 (lambda (acc elem)
    (+ acc elem))))

(assert "sum should be 55" (= 55 (sum (range 1 11))))

(export sum (lambda (seq)
              (fold seq 0 (lambda (acc elem)
                (+ acc elem)))))

(println (take 3 seq))
(println (drop 3 seq))
(println (take-while (lambda (x) (<= x 3)) seq))
(println (drop-while (lambda (x) (<= x 3)) seq))

(assert "should same" (= (take 3 seq) [1 2 3]))
(assert "should same" (= (drop 3 seq) [4 5]))
(assert "should same" (= (take-while (lambda (x) (<= x 3)) seq) [1 2 3]))
(assert "should same" (= (drop-while (lambda (x) (<= x 3)) seq) [4 5]))
(assert "should same" (= (len seq) 5))

(println (filter (lambda (x) (> x 3)) [1 2 3 4 5]))

(assert "should same" (= (filter (lambda (x) (> x 3)) [1 2 3 4 5]) [4 5]))