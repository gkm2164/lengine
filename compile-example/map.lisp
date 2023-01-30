(module MapTest)
(def obj {
  :id 1
  :name "Gyeongmin Go"
  :age 33
})

(println obj)
(println (:id obj))
(println (:name obj))
(println (:age obj))

(println (:new-entry (+ obj (entry "new-entry" "new value"))))