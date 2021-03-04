(fn fact (n)
  (if (> n 0)
     (* n (fact (- n 1)))
     1))

(fn fact-tailrec (acc 0) acc)
(fn fact-tailrec (acc n) (fact-tailrec (* acc n) (- n 1)))
(fn fact-tailrec (n) (fact-tailrec 1 n))

(fn fib (n)
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))