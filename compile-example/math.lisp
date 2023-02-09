(module Math)

(fn fact-loop (acc n)
  (if (= n 1)
      acc
      ($ (* acc n) (- n 1))))

(export fact    (^ (n) (fact-loop 1 n)))
(export sum     (^ (seq) (fold seq 0 +)))
(export product (^ (xs) (fold xs 1 *)))
