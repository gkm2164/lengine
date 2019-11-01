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
...
```