;;; Sequence operator libraries
;;; Written by Gyeongmin Go

(import "libs/utils")

(fn concat-loop (acc nil) acc)
(fn concat-loop (acc xs) (concat-loop (++ acc (head xs)) (tail xs)))

(fn concat (xs*) (concat-loop "" xs*))

;;; extract n-th item from list
(fn n-th (0 list) (head list))
(fn n-th (n list) (n-th (- n 1) (tail list)))

(fn reverse (acc nil) acc)
(fn reverse (acc xs) (reverse (cons (head xs) acc) (tail xs)))
(fn reverse (xs) (reverse nil xs))

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

(fn take (n seq)
  (if (or (empty? seq) (= n 0)) nil
   (cons (head seq) (take (dec n) (tail seq)))))

(fn drop (n seq)
  (if (or (empty? seq) (= n 0)) seq
   (drop (- n 1) (tail seq))))