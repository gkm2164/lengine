(module ProcessMembrane2)

(import Module.split)
(import Module.map)

;;; Read file in Sequence type
(def file-sequence (read-file-seq "./102521_membrane_GC_cell_lines.csv"))

;;; Now the sequence type can be folded.
(def head-values (fold file-sequence [] (lambda (acc elem)
                                                (let ((commas (split elem #\,)))
                                                     (+ acc [(head commas)]) ))))

(def numbers (map (lambda (x) (double x))
                  (filter (lambda (x) (and (/= x "NaN") (/= x "EOF")))
                          (drop 1 head-values))))

(def sum (fold numbers
               0.0
               (lambda (acc elem)
                       (do (println elem)
                           return (+ acc elem)))))
(def avg (/ sum (len numbers)))

(println sum)
(println avg)
