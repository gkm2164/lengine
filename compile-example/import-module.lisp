(module ImportModule)

(import Module.map)
(import Module.fact)

(println (map (lambda (x) (+ x 3)) [1 2 3 4 5]))
(println (fact 5))