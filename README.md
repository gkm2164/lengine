[![Gitpod Ready-to-Code](https://img.shields.io/badge/Gitpod-Ready--to--Code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/gkm2164/lengine) 

# Lengine, another Lisp Engine

## Table of contents

- [Environment](#environment)
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
#C(30 20)   ;;; 30 + 20i
#C(3/5 1/4) ;;; (3/5) + (1/4)i
#C(0.3 2.4) ;;; 0.3 + 2.4i

;;; Character
#\a         ;;; a
#\A         ;;; A
#\c         ;;; c
#\Backspace ;;; \b
#\Linefeed  ;;; \n
#\Page      ;;; \p
#\Return    ;;; \r
#\Rubout    ;;; like \b

;;; String
"Hello World!"
"\"This is escape example for double quote!\""

;;; Object
{
  :id 1
  :name "Gyeongmin Go"
  :email "gkm2164@gmail.com"
}
```

- Function definition

```lisp
;;; Define function
;;; (fn [name] [symbols] [body])
;;; ex) adder takes a and b
(fn add (a b) (+ a b))


;;; Function override
(fn add (a b) (+ a b))
(fn add (a b c) (+ (+ a b) c))

;;; Function pattern match
(fn fact (acc 0) acc)
(fn fact (acc n) (fact (* acc n) (- n 1)))
(println (fact 5)) ;;; 120
```

- Define value

```lisp
;;; Define variable
;;; (def [name] [body])
;;; ex) set x to be 3
(def x 3)
x

;;; ex) define lazy symbol
(def 'x (+ 3 3))

;;; and you can use with
'x
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

- do statement

```lisp
;;; Do stmt
;;; (do stmts*)
(do (println 3)
    (println 4)
    (println 5)
    return 0)
;;; print 3 4 5 line by line and return 0
```

- loop statement

```lisp
;;; Loop stmt with for comprehension
;;; (loop [for-stmt]+ [body])
;;; -- for stmt
;;; for [symbol] in [list]
;;; ex)

(loop for x in '(1 2 3 4 5)
      (do (println x)
          (+ x 1)))
;;; == '(2 3 4 5 6)

(loop for x in '(1 2 3 4 5)
      for y in '(2 3 4 5 6)
      (+ x y))
```

- Introduce local variable

```lisp
;;; introduct local variable
;;; (let ([name] [value]) [body])
;;; ex) define variable x and use it in that scope
(import "libs/sequence")  ;;; below example code require sequence library 

(let (name (read-line "Type your name: "))
  (println (concat "Hello, " name "!")))
```

- List

```lisp
;;; use list as value
;;; (list values*)
;;; ex) List for 1 2 3 4 5
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))
(list 1 2 3 4 5)
[1 2 3 4 5]
;;; List type of List(1, 2, 3, 4, 5)
```

- operators

```lisp
;;; boolean operators
(and true true) ;;; == true
(and false true) ;;; == false
(>= 3 0) ;;; == true
(or (>= 3 0) (<= 0 1)) ;;; == true
;;; and, or, =(equal), /=(not equal), >=, <=, >, <

;;; not allowed
(and true 3) ;;; ???
```

- Object refer
```lisp
;;;
({:hello 1} :hello) ;;; == 1

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
- Namespace
- Stream
- Binary generation
- Loop with only condition
- Macro implementation

### Friend Projects
- [KAND](https://github.com/eunmin/kand): Compilable LISP, type system enhanced, and could be run on Java Virtual Machine
