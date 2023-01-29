(def seq [1 2 3 4 5])
(def fold-result (fold (range 1 10) 0 (lambda (acc elem) (+ acc elem))))

(println "Testing sequence")
(assert "sequence is sequence" (= seq [1 2 3 4 5]))

(println "Testing fold function")
(assert "fold-result should be 45" (= fold-result 45))
