# Lengine, another Lisp Engine

## Table of contents

- [Envorinment](#environment)
- [How to run](#how-to-run)
- [Features](#features)
- [To be implemented](#to-be-implemented)


<a name="environment" />

## Environment
- Compiled at Scala 2.12.10
- [SBT](https://www.scala-sbt.org/) should be installed

<a name="how-to-run" />

## How to run?

### Execute Code
```bash
# SBT should be installed in your local machine
$ git clone https://github.com/gkm2164/lengine
$ ./install.sh
$ ./lengine [filename]?

# ex) run lisp code
$ ./lengine ./examples/testCode.lisp

# ex) run REPL
$ ./lengine
 
```

### REPL
```bash
$ sbt run
lengine > (println (+ 3 5))
8
res0 => ()
```

- To quit REPL,

```bash
lengine > (quit)
``` 

You can test List type also
```bash
lengine > (println (cons 3 (cons 5 nil)))
(3 5)
res0 => ()
lengine > (println (tail (cons 3 (cons 5 nil))))
(5)
res1 => ()
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

- Data types
```lisp
;;; integer
0
#2r100 ;;; binary number 100
#xabcd ;;; hex number abcd
#18rabcdefgh ;; base 18 number abcdefgh
#2r-100 ;;; negative number for binary 100

;;; Float number
0.0
-0.35

;;; Ratio number
3/5
-3/5

;;; Complex Number
#C(30 20) ;;; 30 + 20i
#C(3/5 1/4) ;;; (3/5) + (1/4)i
#C(0.3 2.4) ;;; 0.3 + 2.4i

;;; String
"Hello World!"
"\"This is escape example for double quote!\""
```

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

- REPL history

```lisp
;;; See history or use the history
;;; (history [index of history]?)
;;; ex) see history result
(history)        ;;; command history is also accumulated to history
0: (+ 3 5)       ;;; examples
1: (println 3)


;;; run 
(history 0)
res3 => 8
```

<a name="to-be-implemented" />

### To be implemented

#### Feature
- Char Type
- Function overriding
- Pattern match
- Namespace
- Stream
- Binary generation