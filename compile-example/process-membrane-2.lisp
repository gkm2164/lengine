(module ProcessMembrane2)

(import Module.split)
(import Module.map)

(def file-sequence (read-file-seq "./102521_membrane_GC_cell_lines.csv"))

(def head-values (fold file-sequence [] (lambda (acc elem)
                                                (let (commas (split elem #\,))
                                                     (+ acc [(head commas)])))))

(def numbers (map (lambda (x) (double x))
                  (filter (lambda (x) (and (/= x "NaN") (/= x "EOF")))
                          (drop 1 head-values))))

(def sum (fold numbers
               0.0
               (lambda (acc elem) (+ acc elem))))
(def avg (/ sum (len numbers)))

(println sum)
(println avg)