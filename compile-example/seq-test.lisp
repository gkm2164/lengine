(module Seq)

(import Runtime.fold)

(def seqs [1 2 3 4 5])
(def fold-result (fold (range 1 10) 0 +))

(println "Testing sequence")
(println seqs)
(assert "sequence is sequence" (= seqs [1 2 3 4 5]))

(println "Testing fold function")
(assert "fold-result should be 45" (= fold-result 45))

(fn sum (seqs)
  (fold seqs 0 (lambda (acc elem)
    (+ acc elem))))

(assert "sum should be 55" (= 55 (sum (range 1 11))))

(export sum (lambda (seqs)
              (fold seqs 0 (lambda (acc elem)
                (+ acc elem)))))

(println (take 3 seqs))
(println (drop 3 seqs))
(println (take-while (lambda (x) (<= x 3)) seqs))
(println (drop-while (lambda (x) (<= x 3)) seqs))

(println (take 3 seqs))
(println (drop 3 seqs))
(println (take-while (lambda (x) (<= x 3)) seqs))
(println (drop-while (lambda (x) (<= x 3)) seqs))

(assert "should same" (= (take 3 seqs) [1 2 3]))
(assert "should same" (= (drop 3 seqs) [4 5]))
(assert "should same" (= (take-while (lambda (x) (<= x 3)) seqs) [1 2 3]))
(assert "should same" (= (drop-while (lambda (x) (<= x 3)) seqs) [4 5]))
(println (len seqs))
(assert "should same" (= (len seqs) 5))

(println (filter (lambda (x) (> x 3))
                 [1 2 3 4 5]))

(assert "should same" (= (filter (lambda (x) (> x 3))
                                 [1 2 3 4 5])
                         [4 5]))

(loop for x in (seq "something")
      (println x))

(println (fold (range 1 5)
               ""
               (lambda (x y) (+ x y))))

(def comb (loop for x in (range 1 10)
                for y in (=range 1 x)
                [x y]))

(println (loop for l in comb
               (do (println l)
                   return (flatten l))))

(println nil)
