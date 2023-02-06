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
(export to-json (lambda (obj)
            (case ((bool? obj) (str obj))
                  ((char? obj) (with-double-quotes (str obj)))
                  ((int? obj) (str obj))
                  ((string? obj) (with-double-quotes obj))
                  ((double? obj) (str obj))
                  ((object? obj) (to-json-object obj $))
                   default "null")))

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

(def json-ch-seq (seq json-chs))
(def json-ch-list (list json-chs))

(fn empty? (sequence)
           (= 0 (len sequence)))


(fn fold-right (seq acc folder)
                (fold seq acc (lambda (elem acc) (folder acc elem))))

(fn not-p (p) (lambda (x) (not (p x))))

(fn join (xs str)
         (fold xs "" +))

(fn parse-string (acc s)
  (if (nil? s) [acc s]
  (if (= (head s) #\") [acc (tail s)]
  ($ (+ acc (head s)) (tail s)))))

(fn parse-number (acc s)
    (case ((nil? s) [(double (join acc "")) nil])
          ((contains (list ",]}) ") (head s)) [(double (join acc "")) s])
          default ($ (+: acc (head s)) (tail s))))

(fn parse-boolean (s)
    (if (= #\t (head s)) [true (drop 4 s)]
                         [false (drop 5 s)]))

(fn parse-null (s) [nil (drop 4 s)])

(fn parse-object (acc s pv)
    (case ((nil? s) [acc s])
          ((= (head s) #\}) [acc (tail s)])
          ((= (head s) #\,) ($ acc (tail s) pv))
          ((= (head s) #\Space) ($ acc (tail s) pv))
          ((= (head s) #\")
            (let ((key-remains (parse-string "" (tail s))) ;;; ["SomeString" REMAINS]
                  (key-name (head key-remains))            ;;; "SomeString"
                  (after-key (head (tail key-remains)))
                  (remains (drop 1 after-key))             ;;; REMAINS
                  (value-remains (pv remains))
                  (value (head value-remains))
                  (e (entry (key key-name) value))
                  (remains (head (tail value-remains))))
                 ($ (+ acc e) remains pv)))
          default [nil nil]))

(fn parse-array (acc s pv)
    (case ((nil? s) [acc s])
          ((= (head s) #\Space) ($ acc (tail s) pv))
          ((= (head s) #\,)  ($ acc (tail s) pv))
          ((= (head s) #\]) [acc (tail s)])
          default (let
            ((parsed (pv s))
             (value (head parsed))
             (remains (head (tail parsed))))
            ($ (+: acc value) remains pv))))

(fn num? (ch)
    (contains (seq "-0123456789") ch))

(fn parse-value (json-str)
      (if (nil? json-str) ["" json-str]
      (let ((first (head json-str)))
           (case ((= #\{ first) (parse-object {} (tail json-str) $))
                 ((= #\[ first) (parse-array (seq nil) (tail json-str) $))
                 ((= #\" first) (parse-string "" (tail json-str)))
                 ((contains (list "-1234567890.") first) (parse-number (seq nil) json-str))
                 ((= #\t first) (parse-boolean json-str))
                 ((= #\f first) (parse-boolean json-str))
                 ((= #\n first) (parse-null json-str))
                 ((= #\Space first) ($ (tail json-str)))
                 default ["" json-str]))))

(fn from-json (json-str)
    (head (parse-value (list json-str))))

(def parsed-value (head (parse-value json-ch-seq)))

;;; Let's see whether the parsing is done well...
(println parsed-value)
(println (:header (:menu parsed-value)))
(println (:items (:menu parsed-value)))

(def result (loop for x in (=range 1 10)
                  (let ((start (now))
                        (json-obj (from-json json-chs)))
                       [(- (now) start) json-obj])))

(loop for x in result
      (let ((fmt "%dms elapsed, and got result: [%s]\n"))
           (printf fmt x)))