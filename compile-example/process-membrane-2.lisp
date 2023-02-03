(module ProcessMembrane2)

(import Module.split)
(import Module.map)
(import Module.filter)
(import Module.append)

;;; Read file in Sequence type
(def file-sequence (read-file-seq "./102521_membrane_GC_cell_lines.csv"))

;;; Now the sequence type can be folded.
(def head-values (fold file-sequence [] (lambda (acc elem)
                                                (let ((commas (split elem #\,)))
                                                     (append acc (head commas))))))

(println head-values)
(def numbers (map (lambda (x) (double x))
                              (filter (drop 1 head-values)
                                      (lambda (x) (and (/= x "NaN") (/= x "EOF"))))))

(println numbers)
(def sum (fold numbers
               0.0
               (lambda (acc elem)
                       (do (println elem)
                           return (+ acc elem)))))
(def avg (/ sum (len numbers)))

(println sum)
(println avg)
