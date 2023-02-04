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
(fn double-quote? (ch) (= #\" ch))
(fn not-p (p) (lambda (x) (not (p x))))
(fn and-combiner (p q) (lambda (x) (and (p x) (q x))))
(fn .and (p-seqs) (fold p-seqs always-true and-combiner))
(fn or-combiner  (p q)  (lambda (x) (or (p x) (q x))))
(fn .or (p-seqs) (fold p-seqs always-false or-combiner))

(fn next (s ch)
         (let ((first (head s))
               (last (tail s)))
              (if (= first ch) s
                  ($ last ch))))

(fn join (xs str)
         (fold xs "" +))

(fn parse-string (s) [
    (join (take-while (not-p double-quote?) s) "")
    (tail (drop-while (not-p double-quote?) s))
])
(fn parse-number (s) [0 (tail s)])
(fn parse-boolean (s) [false (tail s)])
(fn parse-null (s) [nil (drop 4 s)])

(fn parse-object (acc s pv)
    (case ((nil? s) [acc s])
          ((= (head s) #\}) [acc (tail s)])
          ((= (head s) #\,) ($ acc (tail s) pv))
          ((= (head s) #\Space) ($ acc (tail s) pv))
          ((= (head s) #\")
            (let ((key-remains (parse-string (tail s)))       ;;; ["SomeString" REMAINS]
                  (key-name (head key-remains))            ;;; "SomeString"
                  (after-key (head (tail key-remains)))
                  (remains (drop 1 (next after-key #\Space))) ;;; REMAINS
                  (value-remains (pv remains))
                  (value (head value-remains))
                  (e (entry (key key-name) value))
                  (remains-2 (head (tail value-remains))))
                 ($ (+ acc e) remains-2 pv)))
          default [acc s]))

(fn parse-array (acc s pv)
    (case ((nil? s) [acc s])
          ((= (head s) #\Space) ($ acc (tail s) pv))
          ((= (head s) #\,) ($ acc (tail s) pv))
          ((= (head s) #\]) [acc (tail s)])
          default (let
            ((parsed (pv s))
             (value (head parsed))
             (remains (head (tail parsed))))
            ($ (+: acc value) remains pv))))

(fn parse-value (json-str)
      (if (= 0 (len json-str)) ["" json-str]
      (let (
            (first (head json-str))
           )

           (case ((= #\{ first) (parse-object {} (tail json-str) $))
                 ((= #\[ first) (parse-array (seq nil) (tail json-str) $))
                 ((= #\" first) (parse-string (tail json-str)))
                 ((contains (list "1234567890") first) (parse-number json-str))
                 ((= #\t first) (parse-boolean json-str))
                 ((= #\f first) (parse-boolean json-str))
                 ((= #\n first) (parse-null json-str))
                 ((= #\Space first) ($ (tail json-str)))
                 default ["" json-str]))))

(println (head (parse-value llist)))
