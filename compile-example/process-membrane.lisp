(module ProcessMembrane)

(import Module.split)
(import Module.map)
(import Module.filter)
(import Module.reverse)

(def all-data (read-file "./102521_membrane_GC_cell_lines.csv"))

(def splitted (split all-data #\Linefeed))

(def first-values (reverse (loop for x in splitted
                                 (let ((line (split x #\,)))
                                 (head line)))))

(def numbers (map (lambda (x) (double x))
                  (filter (drop 1 first-values)
                          (lambda (x) (and (/= x "NaN") (/= x "EOF"))))))

(def sum (fold numbers
               0.0
               (lambda (acc elem) (+ acc elem))))
(def avg (/ sum (len numbers)))

(println sum)
(println avg)
