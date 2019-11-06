;;; Sequence operator libraries
;;; Written by Gyeongmin Go

(fn concat-loop (acc xs)
  (if (eq (len xs) 0)
    acc
    (concat-loop (++ acc (head xs)) (tail xs))))

(fn concat (xs*)
  (concat-loop "" xs*))

;;; extract n-th item from list
(fn n-th (n list)
  (if (eq n 0)
    (head list)
    (n-th (- n 1) (tail list))))

(fn reverse-loop (acc xs) (if (eq (len xs) 0) acc (reverse-loop (cons (head xs) acc) (tail xs))))

(fn reverse (xs)
  (reverse-loop nil xs))

(fn empty? (seq) (eq (len seq) 0))

;;; map sequence to given f
(fn map (seq f)
  (if (empty? seq)
    nil
    (cons (f (head seq)) (map (tail seq) f))))

;;; flatten sequence of sequences
(fn flatten (seq-of-seq)
  (if (empty? seq-of-seq)
    nil
    (++ (head seq-of-seq) (flatten (tail seq-of-seq)))))

(fn range (from to)
    (if (< from to)
        (cons from (range (+ from 1) to))
        (cons from nil)))