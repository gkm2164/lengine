(module Math)

(fn fact (n)
    (if (> n 1)
        (* n (fact (- n 1)))
        1))

(export fact fact)
(export sum (lambda (seq)
    (fold seq 0 (lambda (x y)
                        (+ x y)))))
(export product (lambda (seq)
    (fold seq 1 (lambda (x y)
                        (* x y)))))