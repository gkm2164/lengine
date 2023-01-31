(module InfinityLoop)

(fn recursion ()
    (do (println "Run forever!")
        return ($)))

(recursion)