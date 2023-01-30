(module ReadLine)

(import Module.map)
(import Module.split)
(import Module.to-string)

(def str (read-line))
(def trimmed (to-string (filter (lambda (ch) (/= ch #\Space)) (seq str))))
(def splitted (split trimmed #\,))
(def num-seq (map (lambda (s) (int s)) splitted))
(def sum (fold num-seq 0 (lambda (acc elem) (+ acc elem))))
(def avg (/ (double sum) (len num-seq)))

(println sum)
(println avg)