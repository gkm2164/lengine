;;; Written by Gyeongmin Go
;;; 2019 Nov. 3

(fn reverse (xs)
  ((fn loop (acc xs)
    (if (eq (len xs) 0)
      acc
      (loop (cons (head xs) acc) (tail xs)))) xs))
