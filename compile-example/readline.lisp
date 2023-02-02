(module ReadLine)

(import Module.map)
(import Module.split)
(import Module.to-string)

(def line (read-line))
(def trimmed (to-string (filter (lambda (ch) (/= ch #\Space)) (seq line))))
(def splitted (split trimmed #\,))
(def nums (map (lambda (s) (int s)) splitted))
(def sum (fold nums 0 +))
(def avg (/ (double sum) (len nums)))

(println sum)
(println avg)
