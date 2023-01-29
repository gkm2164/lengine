(module Module)

(export map (lambda (f seq)
  (loop for x in seq
    (f x))))

(fn fact (n)
    (if (> n 1)
        (* n (fact (- n 1)))
        1))

(export fact fact)