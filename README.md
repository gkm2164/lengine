# Lisp Parser written in Scala

## How to run?

### Execute Code
```bash
$ git clone https://github.com/gkm2164/lisp-parser
$ sbt compile
$ sbt "run [filename]"

# ex)
$ sbt "run testCode.lisp" 
 
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

### Features
- Function definition

```lisp
;;; Define function
;;; (fn [name] [symbols] [body])
;;; ex) adder takes a and b
(fn add (a b) (+ a b))

```

- Define value

```lisp
;;; Define variable
;;; (def [name] [body])
;;; ex) set x to be 3
(def x 3)
```

- Lambda

```lisp
;;; Lambda as value
;;; (lambda [args] [body])
;;; ex) Lambda which takes 2 variable and add them
(lambda (a b) (+ a b))

;;; same as fn...
(def add (lambda (a b) (+ a b)))
;;; == (fn add (+ a b))
```

### Remain issues, features

#### Feature
- Pattern match
- Lambda implemented

#### Issues
- Recovery from error while REPL execution error
