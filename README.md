# Lisp Parser written in Scala

## How to run?

### Execute Code
```bash
$ git clone https://github.com/gkm2164/lisp-parser
$ sbt compile
$ sbt "run [filename]"
# ex) $ sbt "run testCode.lisp" 
 
```

### REPL
```bash
$ sbt run
GLisp > (println (+ 3 5))
8
res0 => ()
```

You can test List type also
```bash
GLisp > (println (cons 3 (cons 5 nil)))
(3 5)
res1 => ()
GLisp > (println (tail (cons 3 (cons 5 nil))))
(5)
res2 => ()
...
```