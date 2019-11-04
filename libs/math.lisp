(fn fact (n)
  (if (> n 0)
     (* n (fact (- n 1)))
     1))

(fn fact-tailrec (acc n)
  (if (> n 0) (fact-tailrec (* acc n) (- n 1))
   acc))

(fn fib (n)
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))