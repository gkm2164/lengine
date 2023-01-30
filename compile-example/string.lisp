(module StringModule)

(import Module.map)
(import Module.split)

(def xs (let (x "33") (+ x 234)))

(println (len (seq "Hello World")))
(println (split "Hello,World,This,Is,Real,World" #\,))

(println (head [1 2 3 4 5]))
(println (tail [1 2 3 4 5]))
(println (head (tail [1 2 3 4 5])))

(loop for x in (split "Hello,World,This,Is,Real,World" #\,)
    (println x))