(fn concat-loop (acc xs)
  (if (eq (len xs) 0)
    acc
    (concat-loop (++ acc (head xs)) (tail xs))))

(fn concat (xs*)
  (concat-loop "" xs*))

(concat "123" "456" "789")