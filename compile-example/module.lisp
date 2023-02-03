(module Module)

(fn map (f xs)
        (loop for x in xs
              (f x)))

(export map (lambda (f xs)
                    (loop for x in xs
                          (f x))))

(fn split-real (s delim)
    (if (= 0 (len s))
        []
        (let ((delim-check (lambda (ch) (= ch delim)))
              (splitted (split-at delim-check s))
              (first (head splitted))
              (last (head (tail splitted))))
             (+ [first] (split-real last delim)))))

(fn to-string (xs)
    (fold xs "" (lambda (acc elem) (+ acc elem))))

(export to-string to-string)

(export split (lambda (s delim)
        (map (lambda (x) (to-string x))
             (split-real (seq s) delim))))

(export compose (lambda (f g)
                        (lambda (x) (f (g x)))))

(export fact (lambda (n)
                (if (> n 1)
                    (* n ($ (- n 1)))
                    1)))

(fn append (xs elem)
           (if (nil? xs) (cons elem nil)
               ($ (tail xs) elem)))

(export filter (lambda (xs p)
        (fold xs nil (lambda (acc elem)
                             (if (p elem)
                                 (append acc elem)
                                 acc)))))

(export fold-custom (lambda (seq acc f)
                            (let ((elem (head seq))
                                  (next-acc (f acc elem)))
                                 ($ (tail seq) next-acc f))))
