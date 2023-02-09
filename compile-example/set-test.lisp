(module SetTest)

(def string-set (set ["abc" "def" "ghi"]))
(def string-set-2 (set ["def" "jkl" "mno"]))

(println (str string-set))

(assert-true  "Should have abc in the set" (has? string-set "abc"))
(assert-false "Should not have abc in the set" (has? string-set "abcde"))

(assert-equals "Should be same set" (++ string-set string-set-2) (set ["abc" "def" "ghi" "jkl" "mno"]))
