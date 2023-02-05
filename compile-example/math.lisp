(module Math)

(fn fact-loop (acc n)
         (if (= n 1)
             acc
             ($ (* acc n) (- n 1))))

(export fact (lambda (n)
                     (fact-loop 1 n)))

(export sum (lambda (seq)
    (fold seq 0 (lambda (x y)
                        (+ x y)))))
(export product (lambda (seq)
    (fold seq 1 (lambda (x y)
                        (* x y)))))
