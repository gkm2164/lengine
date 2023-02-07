(module InfinityLoop)

;;; Testing tail-recursion
(fn recursion ()
    (do (println "Run forever!")
        return ($)))

(recursion)
