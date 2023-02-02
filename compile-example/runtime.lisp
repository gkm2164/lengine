(module Runtime)

;;; This file is to define basic runtime functions
;;; The goal of this program is to put every basic "Functional" functions to be here.

(export map (lambda (f xs)
                    (loop for x in xs
                          (f x))))

(fn empty? (seq)
           (= 0 (len seq)))

(export empty? empty?)

(fn fold (xs acc f)
         (if (empty? xs) acc
             (let ((elem (head xs))
                   (next-acc (f acc elem)))
                   ($ (tail xs) next-acc f))))

(export fold fold)

(export to-string (lambda (xs)
                          (fold xs "" (lambda (acc elem) (+ acc elem)))))

(export split (lambda (s delim)
                      (if (= 0 (len s))
                      []
                      (let ((delim-check (lambda (ch) (= ch delim)))
                            (splitted (split-at delim-check s))
                            (first (head splitted))
                            (last (head (tail splitted))))
                           (+ [first] ($ last delim))))))
