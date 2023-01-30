(module ReadLine)

(import Module.map)
(import Module.split)
(import Module.to-string)

(println "Type any numbers with comma => ")
(def str (read-line))
(def trimmed (to-string (filter (lambda (ch) (/= ch #\Space)) (seq str))))
(def splitted (split trimmed #\,))
(def num-seq (map (lambda (s) (int s)) splitted))
(def sum (fold num-seq 0 (lambda (acc elem) (+ acc elem))))

(println sum)
