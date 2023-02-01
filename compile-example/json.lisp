(module Json)

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
(fn with-double-quotes (s)
    (+ "\"" (+ s "\"")))

(fn to-json-object (obj to-json)
                   (let ((ks (keys obj))
                         (key-values (loop for k in ks
                                           (let ((value (k obj))
                                                 (object-key (+ "\"" (+ (get k) "\":"))))
                                                 (+ object-key (to-json value))))))
                        (+ (+ "{"
                              (join key-values #\,) )
                           "}")))


;;; Actual logics
(fn to-json (obj)
            (if (bool? obj)
                (str obj)
            (if (char? obj)
                (with-double-quotes (str obj))
            (if (int? obj)
                (str obj)
            (if (string? obj)
                (with-double-quotes obj)
            (if (double? obj)
                (str obj)
            (if (object? obj)
                (to-json-object obj $)
                "null")))))))

(def obj {
           :id "Hello"
           :age 33
           :height 185.0
           :male true
           :logo #\,
         })

(def start (now))

(def json-str (to-json obj))

(println (- (now) start))
(println json-str)
