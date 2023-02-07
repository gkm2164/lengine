(module Tree)

(fn node (v l r) [l v r])

(fn leaf (v) (node v nil nil))

;;;
;;;         3
;;;       2   4
;;;

(println (node 3 (leaf 2) (leaf 4)))

(def some-tree (node 4 (node 2 (leaf 1) (leaf 3)) (node 5 (leaf 6) (leaf 7))))

(println some-tree)

(fn dfs-visit (acc tree)
    (let ((left (head tree))
          (v (head (drop 1 tree)))
          (right (head (drop 2 tree)))
          (left-result (if (nil? left)
                           nil
                           (dfs-visit acc left)))
          (right-result (if (nil? right)
                            nil
                            (dfs-visit acc right))))
         (++ (+: (++ acc left-result) v) right-result)))

(println (dfs-visit (seq nil) some-tree))