(module ProcessMembrane2)

(import Module.split)
(import Module.map)
(import Module.filter)

;;; Read file in Sequence type
(def file-sequence (read-file-seq "./102521_membrane_GC_cell_lines.csv"))

;;; Now the sequence type can be folded.
(def head-values (fold file-sequence nil (^ (acc elem)
                                                (let ((commas (split elem #\,)))
                                                     (+: acc (head commas))))))

(def numbers (map (^ (x) (double x))
                  (filter (drop 1 head-values)
                          (^ (x) (and (/= x "NaN") (/= x "EOF"))))))

(println numbers)

(def sum (fold numbers
               0.0
               (lambda (acc elem)
                       (do (println elem)
                           return (+ acc elem)))))

(def avg (/ sum (len numbers)))

(println sum)
(println avg)
