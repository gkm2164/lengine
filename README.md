# Lengine, another Lisp Engine

## Table of contents

- [How to run](#how-to-run)
- [Features](#features)
- [To be implemented](#to-be-implemented)


<a name="how-to-run" />
## How to run?

### Execute Code
```bash
$ git clone https://github.com/gkm2164/lengine
$ sbt compile
$ sbt "run [filename]"

# ex)
$ sbt "run ./examples/testCode.lisp" 
 
```

### REPL
```bash
$ sbt run
GLisp > (println (+ 3 5))
8
res0 => ()
```

- To quit REPL,

```bash
GLisp > (quit)
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

## Sample Code
- refer to ```examples``` folder
```lisp
;;; map list with 2 adder
(import "libs/sequence")

(def nums (list 1 2 3 4 5))
(map nums (lambda (x) (+ x 2))
```

<a name="features" />
## Features
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
x

;;; ex) define lazy symbol
(def x? 3)
;;; which equals to
(fn x? () 3)

;;; and you can use with
(x?)
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

- use list

```lisp
;;; use list as value
;;; (list values*)
;;; ex) List for 1 2 3 4 5
(list 1 2 3 4 5)
;;; == (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))
```

- operators

```lisp
;;; boolean operators
(and true true) ;;; == true
(and false true) ;;; == false
(>= 3 0) ;;; == true
(or (>= 3 0) (<= 0 1)) ;;; == true
;;; and, or, eq, >=, <=, >, <

;;; not allowed
(and true 3) ;;; ???
```

- Import library
```lisp
;;; Import library
;;; (import [path])
(import "libs/sequence") ;;; import sequence operations
;;; test
(map (list 1 2 3 4 5) (lambda (x) (+ x 1)))
;;; == (list 2 3 4 5 6)
```

<a name="to-be-implemented" />
### To be implemented

#### Feature
- Floating, Rational, Complex Number operations
- Function overriding
- Pattern match
- Namespace
- Stream
- History feature
