(module Hello)

(def start (now))

(println "=== Declare variables ===")
(def v 3)
(def strs "Something Wonderful!")
(def name "YoonJung")

(println "=== Testing variable reference ===")
(println v)
(println strs)
(println name)
(println "=== Print char ===")
(println #\n)
(println "=== Print String ===")
(println "Hello World")
(println "=== Add 2 values ===")
(println (+ 2 3))
(println "=== Add 2 values, Int, Double ===")
(println (+ 2 5.2))
(println "=== 2 * (3 - 2) / 5.3 == Should be double ===")
(println (* 2 (/ (- 3 2) 5.3)))
(println "=== Add number to variable ===")
(println (+ v 10))

(println "=== Do some string concatenation ===")
(println (+ (+ "Hello, " name) "!"))
(println (+ name " is beautiful woman!"))
(println (+ name 123))

(println "=== Sequence Test! ===")
(def single-element [1])
(def seq-plain [1 2 3 4 5 6 7 8 9 10])
(def seq-multi-types [1 2 3 name])
(def seq-multi-types-2 [1 2.0 #\3 "Hello" name])
(def seq-nested [1 2 3 4 5 [1 2 3 4 5]])

(println "=== Single element ===")
(println single-element)
(println "=== Multiple element ===")
(println seq-plain)
(println "=== multi-type valued sequence ===")
(println seq-multi-types)
(println "=== multi-type valued sequence ===")
(println seq-multi-types-2)
(println "=== Nested sequence ===")
(println seq-nested)

(fn print-seq (x y)
  (println [x y]))

(fn other-seq (x)
  (println x))

(fn concat (x y) [x y])

(print-seq "Hello" "World")
(println "Hello World!")
(print-seq "something" "World")
(println (concat 3 5))
(other-seq "something2")
(print-seq "something" "World")
(other-seq "something3")

(println "=== Closure test. Variable v is not given in argument, but declared in parent scope ===")
(fn concat-clojure (x y) [x y v])

(println (concat-clojure 3 5))
(println "=== if you see [3 5 3], it's success!! ===")

(println (take 3 [1 2 3 4 5]))
(println (drop 3 [1 2 3 4 5]))

(println (+ [] 3))
(println (+ [] "ABCDEF"))

(println (flatten seq-nested))

(println "Test Unit type")
(println (println "this would be printed first, and then ()"))

(println "Doing some Type casting")
(println (char (int #\c)))
(println (int 1.3))
(println (double 1))
(println (str 1234))

(def object {
  :id 12345
  :name "Complex Object!"
  :value "Yeah, you are looking at it"
})

(println "==== Object Test ====")
(println object)
(println (+ "ID: " (:id object)))
(println (+ "Name: " (:name object)))
(println (+ "Value: " (:value object)))

(println (- (now) start))
