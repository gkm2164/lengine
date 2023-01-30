(module Module)

(fn map (f seq)
  (loop for x in seq
    (f x)))

(export map map)

(fn fact (n)
    (if (> n 1)
        (* n (fact (- n 1)))
        1))

(export fact fact)

(fn split-real (s delim)
    (if (= 0 (len s))
        []
        (let (delim-check (lambda (ch) (= ch delim)))
        (let (splitted (split-at delim-check s))
        (let (first (head splitted))
        (let (last (head (tail splitted)))
             (+ [first] (split-real last delim))))))))

(fn to-string (xs)
    (fold xs "" (lambda (acc elem) (+ acc elem))))

(export to-string to-string)

(export split (lambda (s delim)
        (map (lambda (x) (to-string x))
             (split-real (seq s) delim))))

(export compose (lambda (f g)
                        (lambda (x) (f (g x)))))
