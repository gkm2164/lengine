(module Math)

(export fact (lambda (n)
                 (if (> n 1)
                     (* n ($ (- n 1)))
                     1)))

(fn fact2 (acc n)
         (if (= n 1)
             acc
             ($ (* acc n) (- n 1))))

(export sum (lambda (seq)
    (fold seq 0 (lambda (x y)
                        (+ x y)))))
(export product (lambda (seq)
    (fold seq 1 (lambda (x y)
                        (* x y)))))

(export fact2 fact2)