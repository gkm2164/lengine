(module SetTest)

(def string-set (set ["abc" "def" "ghi"]))

(println (str string-set))

(assert-true  "Should have abc in the set" (has? string-set "abc"))
(assert-false "Should not have abc in the set" (has? string-set "abcde"))

