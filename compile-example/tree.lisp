(module Tree)

(import SeqModule.flatten)

(fn node (v l r) (seq [l v r]))
(fn leaf (v) (node v (seq nil) (seq nil)))

(fn left-tree (n) (head n))
(fn right-tree (n) (head (drop 2 n)))
(fn node-value (n) (head (tail n)))

;;;
;;;         3
;;;       2   4
;;;

(println (node 3 (leaf 2) (leaf 4)))

(def some-tree (node 4 (node 2 (leaf 1) (leaf 3)) (node 5 (leaf 6) (leaf 7))))

(println some-tree)

(fn dfs-visit (acc tree)
    (if (nil? tree) acc
        (let ((v (node-value tree))
              (left ($ acc (left-tree tree)))
              (right ($ acc (right-tree tree))))
             (++ (+: (++ acc left) v) right))))

(println (flatten some-tree))

(assert-equals "Visit sequence should be" (flatten (dfs-visit (seq nil) some-tree)) (seq [1 2 3 4 5 6 7]))