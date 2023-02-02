(module Seq)

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


(def xs nil)
(def xs-1 (cons 2 nil))
(def xs-2 (cons 1 (cons 2 (cons 3 nil))))

(println xs)
(println xs-1)
(println xs-2)

(println (fold xs-2 0 +))

(fn fold-left (seq acc folder)
               (fold seq acc folder))

(fn fold-right (seq acc folder)
                (fold seq acc (lambda (elem acc) (folder acc elem))))

(def make-lengine-list (fold-right (=range 1 10) nil cons))

(println make-lengine-list)

(fn reverse-loop (acc xs)
                 (if (nil? xs)
                     acc
                     ($ (cons (head xs) acc) (tail xs))))

(fn reverse (lst)
            (reverse-loop nil lst))

(println (reverse make-lengine-list))

(fn ++ (xs ys)
       (if (nil? xs) ys
                     (cons (head xs) ($ (tail xs) ys))))

(println (++ xs-1 xs-2))

