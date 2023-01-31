(module ProcessMembrane)

(import Module.split)
(import Module.map)

(def all-data (read-file "./102521_membrane_GC_cell_lines.csv"))

(def splitted (split all-data #\Linefeed))

(def first-values (loop for x in splitted
                        (let (line (split x #\,))
                             (head line))))

(def numbers (map (lambda (x) (double x))
                  (filter (lambda (x) (and (/= x "NaN") (/= x "EOF")))
                          (drop 1 first-values))))

(def sum (fold numbers
               0.0
               (lambda (acc elem) (+ acc elem))))
(def avg (/ sum (len numbers)))

(println sum)
(println avg)