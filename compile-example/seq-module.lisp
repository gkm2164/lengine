(module SeqModule)

(fn filter (xs p)
        (fold xs nil (lambda (acc elem)
                             (if (p elem) (+: acc elem)
                                          acc))))

(fn take-while-loop (acc p xs)
                    (case ((nil? xs) acc)
                          ((p (head xs)) ($ (+: acc (head xs)) p (tail xs)))
                          default acc))

(export filter)
(export take-while (lambda (p xs) (take-while-loop (seq nil) p xs)))
(export drop-while (lambda (p xs) (case ((nil? xs) xs)
                                        ((p (head xs)) ($ p (tail xs)))
                                        default xs)))

(fn lt-5 (x) (< x 5))

(println (str (take-while lt-5 (seq [1 2 3 4 5]))))
(println (str (drop-while lt-5 (seq [1 2 3 4 5]))))


(export contains (lambda (chs ch)
                     (if (nil? chs) false
                        (let ((h (head chs))
                              (t (tail chs)))
                             (if (= h ch)
                                 true
                                 ($ t ch))))))

(export flatten (lambda (xs)
  (if (nil? xs) nil
      (let ((h (head xs))
            (t (tail xs)))
               (if (seq? h) (++ ($ h) ($ t))
                   (++ (seq [h]) ($ t)))))))

(println (seq [1 2 3 (seq [1 2 3 (seq [1 2 3])])]))

(println (flatten (seq [1 2 3 (seq [1 2 3 (seq [1 2 3])])])))
