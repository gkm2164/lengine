(module Json)

(fn join (strs delim)
    (fold (tail strs) (head strs) (lambda (acc elem)
                                          (+ (+ acc delim) elem))))

(fn with-double-quotes (s)
    (+ "\"" (+ s "\"")))

(fn to-json (obj)
            (let (to-json-object (lambda (obj)
                                         (let (ks (keys obj))
                                         (let (key-values (loop for k in ks
                                                                (let (value (k obj))
                                                                (let (object-key (+ "\"" (+ (get k) "\":")))
                                                                     (+ object-key (to-json value))))))
                                               (+ (+ "{" (join key-values #\,)) "}")))))
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
                     (to-json-object obj)
                     "null"))))))))

(println (to-json {
  :id "Hello"
  :age 33
  :height 185.0
  :male true
  :logo #\,
}))