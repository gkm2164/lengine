;;; Sequence operator libraries
;;; Written by Gyeongmin Go

(import "libs/utils")

(fn concat-loop (acc xs)
  (if (empty? xs)
    acc
    (concat-loop (++ acc (head xs)) (tail xs))))

(fn concat (xs*)
  (concat-loop "" xs*))

;;; extract n-th item from list
(fn n-th (n list)
  (if (eq n 0)
    (head list)
    (n-th (- n 1) (tail list))))

(fn reverse-loop (acc nil) (do
   acc))
(fn reverse-loop (acc xs) (do
  (reverse-loop (cons (head xs) acc) (tail xs))))

(fn reverse (xs) (reverse-loop nil xs))

(fn empty? (seq) (= (len seq) 0))

;;; map sequence to given f
(fn map (seq f) (loop for x in seq (f x)))

;;; flatten sequence of sequences
(fn flatten (seq-of-seq)
  (if (empty? seq-of-seq)
    nil
    (++ (head seq-of-seq) (flatten (tail seq-of-seq)))))

(fn range (from to)
    (if (< from to)
        (cons from (range (inc from) to))
        (cons from nil)))