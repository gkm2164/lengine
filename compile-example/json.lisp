(module Json)

(import SeqModule.filter)
(import SeqModule.take-while)
(import SeqModule.drop-while)
(import SeqModule.contains)

;;; Finally! reached to JSON related library

;;; Below is actually, "Reduce" operation.
(fn join-recur (acc remains delim)
               (if (= (len remains) 0)
                   acc
                   (let ((elem (head remains))
                         (rest (tail remains)))
                         (join-recur (+ (+ acc delim) elem) rest delim))))

(fn join (strs delim)
    (join-recur (head strs) (tail strs) delim))

;;; Make str -> "str"
(fn with-double-quotes (s) (format "\"%s\"" [s]))

(fn to-json-object (obj to-json)
                   (let ((ks (keys obj))
                         (key-values (loop for k in ks
                                           (let ((value (k obj))
                                                 (object-key (+ (with-double-quotes (get k)) ":")))
                                                (+ object-key (to-json value))))))
                        (+ (+ "{"
                              (join key-values #\,) )
                           "}")))


;;; Actual logics
(fn to-json (obj)
            (case ((bool? obj) (str obj))
                  ((char? obj) (with-double-quotes (str obj)))
                  ((int? obj) (str obj))
                  ((string? obj) (with-double-quotes obj))
                  ((double? obj) (str obj))
                  ((object? obj) (to-json-object obj $))
                   default "null"))

(def obj {
           :id "Hello"
           :age 33
           :height 185.0
           :male true
           :logo #\,
         })

(def result (loop for x in (=range 1 10)
                  (let ((start (now))
                        (json-str (to-json obj)))
                       [(- (now) start) json-str])))

(loop for x in result
      (let ((fmt "%dms elapsed, and got result: [%s]\n"))
           (printf fmt x)))

(def json-file-stream (read-file-seq "./compile-example/json-example.json"))

(def json-chs (fold json-file-stream "" +))

(def json-ch-seq (list json-chs))

(fn empty? (sequence)
           (= 0 (len sequence)))


(fn fold-right (seq acc folder)
                (fold seq acc (lambda (elem acc) (folder acc elem))))

(fn seq-to-llist (xs)
                 (fold xs (seq nil) +:))

(def llist (seq-to-llist json-ch-seq))

(def space [#\Space])

(fn always-true  (any) true)
(fn always-false (any) false)

(fn space? (ch) (contains space ch))
(fn colon? (ch) (= #\: ch))
(fn and-combiner (p q) (lambda (x) (and (p x) (q x))))
(fn .and (p-seqs) (fold p-seqs always-true and-combiner))
(fn or-combiner  (p q)  (lambda (x) (or (p x) (q x))))
(fn .or (p-seqs) (fold p-seqs always-false or-combiner))

(fn parse-string (s) ["" s])
(fn parse-number (s) [0 s])
(fn parse-boolean (s) [false s])
(fn parse-null (s) ["null" s])

(fn parse-object (acc s pv)
    (case ((nil? s) acc)
          ((= (head s) #\{) ($ acc (tail s) pv))
          ((= (head s) #\,) ($ acc (tail s) pv))
          ((= (head s) #\")
            (let ((key-remains (parse-string s))    ;;; ["SomeString" REMAINS]
                  (ignore (println key-remains))
                  (key-name (head key-remains))     ;;; "SomeString"
                  (ignore (println key-name))
                  (remains (drop-while colon? (head (tail key-remains)))) ;;; REMAINS
                  (ignore (println remains))
                  (value-remains (pv remains))      ;;;
                  (ignore (println value-remains))
                  (value (head value-remains))
                  (ignore (println value))
                  (e (entry (key key-name) value))
                  (ignore (println e))
                  (remains-2 (drop-while space? (head (tail value-remains)))))
                 (parse-object (+ acc e) remains-2 pv)))
          default nil))

(fn parse-array  (s) [[] s])

(fn parse-value (json-str)
      (if (= 0 (len json-str)) ["" json-str]
      (let ((clean (drop-while space? json-str))
            (ignore (printf "ITER: %s\n" [(str clean)]))
            (first (head clean))
            (ignore (printf "ITER: %c\n" [first]))
           )

           (case ((= #\{ first) (parse-object {} (tail clean) $))
                 ((= #\[ first) (parse-array  clean))
                 ((= #\" first) (parse-string clean))
                 ((contains (list "1234567890") first) (parse-number clean))
                 ((= #\t first) (parse-boolean clean))
                 ((= #\f first) (parse-boolean clean))
                 ((= #\n first) (parse-null clean))
                 default nil))))

(printf "[%s]" [(str llist)])
(println (str (drop-while space? llist)))
(println (head (parse-value llist)))
