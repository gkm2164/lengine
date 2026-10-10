# Lengine Language Specification

Version 0.1 (draft, 2026-10-10). Normative: this document says what Lengine programs mean.
The compiler (`lenginec`, `lengine-compiler`) and the runtime (`lengine-runtime`, `lengine-code/*.lg`)
are measured against it, not the other way round.

**Every rule is checked.** Each ` ```lengine ` block below is a complete program body (the
`(module …)` line is added by the harness) and is followed by its expected result:

* ` ```output ` the exact text the program prints;
* ` ```compile-error ` a fragment of the compiler's error message;
* ` ```runtime-error ` a fragment of the uncaught exception (`ClassName: message`).

`lengine-compiler/src/test/scala/co/gyeongmin/lisp/compile/SpecConformanceTest.scala` extracts and
runs all of them. An example tagged `bug=<slug>` states the intended behavior, which the
implementation does not meet yet. Those are reported as known deviations, and the suite fails
once one starts passing, so the tag has to be removed. The deviations are the work list.

The old tree-walking interpreter (`lengine-repl`) is not covered and is to be replaced by a
compile-backed REPL that follows this spec.

## 0. Design decisions in this draft

These are choices the spec makes where the implementation was inconsistent or silent. Each is
open for review; changing one means changing its examples.

| # | Decision | Status today |
|---|---|---|
| D1 | `nil` is the empty list and is available without any `require`. | Needs `(require "collections")`. |
| D2 | Emptiness is a protocol, not a type: every collection has its own empty value, and `nil?` is true for all of them. `nil?` is false for non-collections instead of failing. | `nil?` fails on non-collections. |
| D3 | Collections of different kinds are never `=`, even when both are empty. | Matches. |
| D4 | `head` of any empty collection fails with an error mentioning `empty`; `tail` of any empty collection is that same empty collection. | Error types and messages differ per type; `tail` of an empty seq or string fails. |
| D5 | Numbers form one tower, Long < Ratio < Double < Complex. Operators promote to the higher type; `=` compares after promotion. Characters are not numbers for `=`, but arithmetic on them yields Longs. | Matches after the 2026-10 runtime fixes. |
| D6 | Exact division: `/` on two Longs gives a Ratio unless the result is whole. | Truncates. |
| D7 | Inner bindings shadow outer ones and the outer binding is visible again after the inner scope ends. | The outer binding is lost. |
| D8 | Macros are visible in every scope after their definition, including function bodies. | Not visible inside functions. |
| D9 | Source may use `\r\n` line endings; a comment is plain text up to the end of the line. | CRLF works; a comment ending in `\` swallows the next line. |
| D10 | An integer literal that does not fit in 64 bits is a compile error. | Wraps silently. |
| D11 | A type error (for example a non-Boolean condition) is a runtime error at that expression, never a class verification failure of the whole module. | `VerifyError` for some cases. |

---

## 1. Lexical structure

### 1.1 Whitespace and comments

Space, tab, newline and carriage return separate tokens. `;` starts a comment that runs to the
end of the line; its content is ignored, whatever characters it holds.

```lengine id=lex-comment
; a comment
(println 1) ; trailing comment
;; another one
(println 2)
```
```output
1
2
```

```lengine id=lex-comment-backslash
; C:\path\to\file
(println "after")
```
```output
after
```

```lengine id=lex-comment-trailing-backslash
(println 1) ; ends with a backslash \
(println 2)
```
```output
1
2
```

```lengine id=lex-crlf crlf
(println 1)
(println 2)
```
```output
1
2
```

(The `crlf` attribute makes the harness write this example with `\r\n` line endings.)

### 1.2 Integers

Decimal integers, optionally signed, are 64-bit (`java.lang.Long`). Radix forms are `#b`, `#o`,
`#x` and `#<base>r`. A literal outside the 64-bit range is a compile error (D10).

```lengine id=lex-int
(println 42)
(println -7)
(println +3)
(println #x-ff)
(println #b1010)
(println #16rFF)
(println #36rZ)
```
```output
42
-7
3
-255
10
255
35
```

```lengine id=lex-int-overflow
(println 9223372036854775808)
```
```compile-error
9223372036854775808
```

### 1.3 Floats, ratios, complex numbers

```lengine id=lex-float
(println 5.2)
(println 1e3)
(println 2d-3)
```
```output
5.2
1000.0
0.002
```

A ratio literal `a/b` is read in lowest terms, with the sign on the numerator. A zero
denominator is a lexing error.

```lengine id=lex-ratio
(println 3/5)
(println 2/4)
(println -6/4)
```
```output
3/5
1/2
-3/2
```

`#C(re im)` is a complex number; each part is a plain number literal.

```lengine id=lex-complex
(println #C(1 2))
(println #C(1/2 3.5))
```
```output
{1 + 2i}
{1/2 + 3.5i}
```

### 1.4 Characters

`#\c` is the character `c`, for any single character, including non-ASCII ones. Named
characters: `#\Space #\Tab #\Linefeed #\Return #\Backspace #\Page #\Rubout`.
`#\n` is the letter `n`, not a newline.

```lengine id=lex-char
(println #\a)
(println #\n)
(println (= #\Space (char 32)))
(println (int #\Linefeed))
```
```output
a
n
true
10
```

```lengine id=lex-char-rubout
(println (int #\Rubout))
```
```output
127
```

```lengine id=lex-char-unicode
(println #\가)
```
```output
가
```

### 1.5 Strings and booleans

Strings are `"…"` and may span lines. Escapes: `\\ \" \t \n \b \r`.

```lengine id=lex-string
(println "tab:\tend")
(println "quote: \"q\"")
(println "two
lines")
(println true)
(println false)
```
```output
tab:	end
quote: "q"
two
lines
true
false
```

### 1.6 Symbols and keys

A symbol is made of letters, digits and `$ . _ - + / * % < > = ? : ' & | ~`, and does not start
with a digit. `:name` is a map key. The reserved words are
`module import export require def fn defmacro var let ns lambda native loop for in true false do return case default try catch recover when otherwise`.

```lengine id=lex-symbols
(def n' 1)
(def ok? true)
(def <=> 3)
(println n')
(println ok?)
(println <=>)
(println :name)
```
```output
1
true
3
:name
```

---

## 2. Program structure

### 2.1 Modules

A source file is one module and starts with `(module qualified.name)`. A module compiles to a JVM
class of that name. Top-level forms run in order when the module is first loaded.

```lengine id=mod-order
(println "first")
(println "second")
```
```output
first
second
```

### 2.2 Prelude

The prelude (`lengine-code/prelude.lg`) is imported into every module: arithmetic, comparison,
`println`/`printf`/`format`, conversions, assertions, `exit`, concurrency. `nil` is available
everywhere (D1).

```lengine id=mod-prelude
(println (+ 1 2))
(printf "%s-%s\n" [1 2])
```
```output
3
1-2
```

```lengine id=mod-nil-everywhere
(println (nil? nil))
```
```output
true
```

### 2.3 `require`, `import`, `export`

`(require "path")` imports every export of another module. `collections` holds the list,
sequence, set, map, range and stream functions; `std` (`stdlib.lg`) and `math` build on it.

```lengine id=mod-require
(require "collections")
(println (len [1 2 3]))
```
```output
3
```

`(import module.symbol)` imports one export, bound to the last segment.

```lengine id=mod-import
(require "collections")
(import math.pow)
(println (pow 2 10))
```
```output
1024
```

`(export name value)` defines and exports `name`; `(export name)` exports an existing binding.
Export is only allowed at top level.

```lengine id=mod-export
(export answer 42)
(def x 1)
(export x)
(println (+ answer x))
```
```output
43
```

### 2.4 Native bindings

`(native pkg.Class.FIELD fn)` binds a static field that holds a Lengine function; `var` binds any
static value. This is how the prelude reaches the runtime.

```lengine id=mod-native
(def add (native lengine.Prelude.ADD fn))
(println (add 1 2))
```
```output
3
```

---

## 3. Expressions and special forms

Every expression yields a value. A clause `(f a b …)` whose head is not a special form or macro
is a function call: the head and then the arguments are evaluated left to right.

### 3.1 `def`

`(def name expr)` binds `name` in the current scope and yields the value. Defining a name that is
already bound in the same scope is a compile error. `def` is not recursive; use `fn` for
recursion.

```lengine id=def-basic
(def x 3)
(println (def y (+ x 1)))
(println y)
```
```output
4
4
```

```lengine id=def-twice
(def x 1)
(def x 2)
```
```compile-error
Can't define symbol twice
```

### 3.2 `fn`

`(fn name (params…) body)` defines a function and binds `name`, visible inside `body` for
recursion. Functions take a fixed number of parameters, at most 11.

```lengine id=fn-basic
(fn square (x) (* x x))
(println (square 7))
```
```output
49
```

```lengine id=fn-recursion
(fn fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))
(println (fact 10))
```
```output
3628800
```

Two functions with the same name in different scopes are different functions.

```lengine id=fn-same-name-scopes
(def a (let ((k 1)) (do (fn helper (x) (+ x k)) return (helper 10))))
(def b (let ((k 2)) (do (fn helper (x) (* x k)) return (helper 10))))
(println a)
(println b)
```
```output
11
20
```

Calling a function with the wrong number of arguments is a runtime error that names the
expected and actual argument counts.

```lengine id=fn-arity bug=arity-error-message
(fn f (x) x)
(f 1 2)
```
```runtime-error
expects 1 argument
```

### 3.3 Lambdas and closures

`(lambda (params…) body)`, `(^ (params…) body)` and `^((params…) body)` create anonymous
functions. A lambda captures the values of the free variables it uses when it is created.
Inside a lambda, `$` is the lambda itself.

```lengine id=lambda-forms
(def add1 (lambda (x) (+ x 1)))
(def add2 (^ (x) (+ x 2)))
(def add3 ^((x) (+ x 3)))
(println (add1 0))
(println (add2 0))
(println (add3 0))
(println (^((x y) (* x y)) 6 7))
```
```output
1
2
3
42
```

```lengine id=lambda-closure
(fn adder (n) ^((x) (+ x n)))
(def add10 (adder 10))
(println (add10 5))
```
```output
15
```

```lengine id=lambda-self
(def countdown ^((n) (if (= n 0) "done" ($ (- n 1)))))
(println (countdown 5))
```
```output
done
```

A lambda body may contain any expression, including ratio and complex literals and nested
functions.

```lengine id=lambda-any-literal bug=closure-analyzer-gaps
(def half ^((x) (* x 1/2)))
(println (half 3))
```
```output
3/2
```

```lengine id=lambda-nested-fn bug=closure-analyzer-gaps
(def outer ^((x) (do (fn inner (y) (+ y 1)) return (inner x))))
(println (outer 1))
```
```output
2
```

### 3.4 Tail calls

A call to the enclosing function (by name or `$`) in tail position does not grow the stack. All
arguments are evaluated before any parameter is rebound.

```lengine id=tail-deep
(fn count (n acc) (if (= n 0) acc (count (- n 1) (+ acc 1))))
(println (count 1000000 0))
```
```output
1000000
```

```lengine id=tail-args-simultaneous bug=tail-call-args
(fn fib (a b n) (if (= n 0) a (fib b (+ a b) (- n 1))))
(println (fib 0 1 10))
```
```output
55
```

### 3.5 `let`

`(let ((name expr)…) body)` binds names in order (each binding sees the previous ones) and yields
`body`. Inner bindings shadow outer ones; after the `let`, the outer binding is visible again
(D7).

```lengine id=let-sequential
(println (let ((x 3) (y (+ x 1))) (* x y)))
```
```output
12
```

```lengine id=let-shadow-restores bug=shadowing-loses-outer
(println (let ((x 1)) (do (let ((x 2)) x) return x)))
```
```output
1
```

### 3.6 `do`

`(do e1 … return en)` evaluates the expressions in order and yields `en`. `return` is required
before the last expression.

```lengine id=do-basic
(println (do (println "a") (println "b") return 3))
```
```output
a
b
3
```

### 3.7 Conditionals

Conditions must be Booleans; there is no truthiness of other values.

`(if cond then else)`; both branches are required and only one is evaluated.

```lengine id=if-basic
(println (if (< 1 2) "yes" "no"))
(println (if false (println "never") "else"))
```
```output
yes
else
```

```lengine id=if-without-else bug=if-arity-message
(if true 1)
```
```compile-error
if
```

```lengine id=if-non-boolean bug=verify-error-on-type-mismatch
(println "before")
(def n 5)
(if n 1 2)
```
```runtime-error
ClassCastException
```

`(case (test value)… default value)` yields the value of the first true test.

```lengine id=case-basic
(fn sign (n) (case ((< n 0) "negative") ((= n 0) "zero") default "positive"))
(println (sign -3))
(println (sign 0))
(println (sign 9))
```
```output
negative
zero
positive
```

`(for target when (v result)… otherwise result)` compares `target` with each `v` using `=`.

```lengine id=for-when
(fn name (d) (for d when (1 "one") (2 "two") otherwise "many"))
(println (name 2))
(println (name 7))
```
```output
two
many
```

### 3.8 `loop`

`(loop for x in source … body)` maps `body` over the source and collects the results into a
collection of the same kind (a range or map collects into a sequence). Several `for` clauses
nest.

```lengine id=loop-list
(require "collections")
(println (loop for x in [1 2 3] (* x x)))
```
```output
(cons 1 (cons 4 (cons 9 nil)))
```

```lengine id=loop-range
(require "collections")
(println (loop for x in (range 0 4) (* x 10)))
```
```output
(seq 0 10 20 30)
```

```lengine id=loop-map
(require "collections")
(println (len (loop for e in {:a 1 :b 2} e)))
```
```output
2
```

A recursive call inside a loop body is an ordinary call, not a tail call.

```lengine id=loop-body-not-tail bug=loop-body-tail
(require "collections")
(fn walk (xs) (loop for x in xs (if (list? x) (walk x) x)))
(println (walk [1 [2 3]]))
```
```output
(cons 1 (cons (cons 2 (cons 3 nil)) nil))
```

### 3.9 Errors: `try` / `recover`

`(try expr (recover e handler))` yields `expr`, or `handler` with `e` bound to the error if
`expr` raised one. `(:message e)` and `(:type e)` describe it.

```lengine id=try-recover
(println (try (/ 3 0) (recover e (:type e))))
(println (try 42 (recover e 0)))
```
```output
divide-by-zero(java.lang.ArithmeticException)
42
```

### 3.10 Lazy values

`#(expr)` delays `expr`; `!(x)` or `(x)` forces it, once.

```lengine id=lazy-once
(def v #(do (println "computing") return 7))
(println "before")
(println !(v))
(println (v))
```
```output
before
computing
7
7
```

### 3.11 Macros

`(defmacro name (params…) template)` defines a syntactic macro. Plain parameters bind argument
forms, `,word` requires the literal word `word`, and a final `rest*` collects the remaining
forms as a list.

```lengine id=macro-basic
(defmacro unless (c then else) (if c else then))
(println (unless false "ran" "skipped"))
```
```output
ran
```

```lengine id=macro-keywords
(defmacro if' (c then ,else alt) (if c then alt))
(println (if' (= 1 2) "same" else "different"))
```
```output
different
```

Macros are visible in every scope after their definition (D8).

```lengine id=macro-in-fn bug=macros-invisible-in-fn
(defmacro twice (x) (+ x x))
(fn f (y) (twice y))
(println (f 21))
```
```output
42
```

### 3.12 `exit`

`(exit n)` ends the program with status `n`; output printed before it is kept.

```lengine id=exit-basic
(println "bye")
(exit 0)
(println "not printed")
```
```output
bye
```

---

## 4. Values

### 4.1 Booleans and logic

`and`, `or`, `not` take Booleans. They are functions, so both arguments are evaluated.
`&` and `|` also work on Booleans, and bitwise on integers.

```lengine id=bool-logic
(println (and true false))
(println (or true false))
(println (not false))
(println (& true true))
(println (| false false))
(println (& 6 3))
```
```output
false
true
true
true
false
2
```

### 4.2 Numbers

The tower is Long < Ratio < Double < Complex (D5). Binary arithmetic and comparison promote the
lower operand. Exact results stay exact; a ratio that becomes whole is a Long.

```lengine id=num-promotion
(println (+ 1 2))
(println (+ 1 0.5))
(println (+ 1/2 1/3))
(println (+ 1/2 1/2))
(println (* 2/3 3))
(println (+ 1 #C(1 1)))
(println (+ 1.5 #C(1 2)))
```
```output
3
1.5
5/6
1
2
{2 + 1i}
{2.5 + 2i}
```

```lengine id=num-division-exact
(println (/ 6 3))
(println (/ 1 2))
```
```output
2
1/2
```

```lengine id=num-div-zero
(/ 1 0)
```
```runtime-error
ArithmeticException
```

```lengine id=num-compare
(println (< 1 2))
(println (< 1/3 1/2))
(println (>= 2.0 2))
(println (< 1/2 1))
```
```output
true
true
true
true
```

```lengine id=num-complex-inverse
(println (/ 1 #C(0 1)))
(println (/ 1 #C(1 1)))
```
```output
{0 + -1i}
{1/2 + -1/2i}
```

Characters are not numbers for `=`, but arithmetic on characters gives Longs.

```lengine id=num-char-arith
(println (+ #\a 1))
(println (+ (+ #\a #\b) 1))
(println (= #\a 97))
```
```output
98
196
false
```

### 4.3 Equality

`=` and `/=` compare numbers by value after promotion and everything else structurally.
Collections of different kinds are never equal (D3).

```lengine id=eq-numbers
(println (= 1 1.0))
(println (= 1/2 0.5))
(println (= 1/2 2/4))
(println (/= 1 2))
(println (= #C(1 0) 1))
```
```output
true
true
true
true
true
```

```lengine id=eq-collections
(require "collections")
(println (= [1 2] [1 2]))
(println (= (seq [1 2]) (seq [1 2])))
(println (= (set [1 2]) (set [2 1])))
(println (= {:a 1} {:a 1}))
(println (= [1 2] (seq [1 2])))
(println (= "ab" "ab"))
```
```output
true
true
true
true
false
true
```

### 4.4 Emptiness and nil

Each collection kind has its own empty value; `nil` is the empty list. `nil?` is true for an
empty value of any kind and false for anything that is not a collection (D2). Empty values of
different kinds are not equal to each other (D3).

```lengine id=nil-protocol
(require "collections")
(println (nil? nil))
(println (nil? (seq [])))
(println (nil? (set [])))
(println (nil? {}))
(println (nil? (range 1 1)))
(println (nil? ""))
(println (nil? [1]))
(println (= (seq []) nil))
```
```output
true
true
true
true
true
true
false
false
```

```lengine id=nil-non-collection
(require "collections")
(println (nil? 0))
(println (nil? false))
```
```output
false
false
```

`head` of an empty collection is an error mentioning `empty`; `tail` of an empty collection is
the same empty value (D4).

```lengine id=nil-head-empty
(require "collections")
(head (seq []))
```
```runtime-error
empty
```

```lengine id=nil-tail-empty
(require "collections")
(println (nil? (tail nil)))
(println (nil? (tail (seq []))))
(println (nil? (tail "")))
```
```output
true
true
true
```

### 4.5 Lists

`[a b c]` and `(list …)` build lists; `cons` prepends. Lists are immutable: adding returns a new
list.

```lengine id=list-basic
(require "collections")
(def xs [1 2 3])
(println (head xs))
(println (tail xs))
(println (cons 0 xs))
(println (len xs))
```
```output
1
(cons 2 (cons 3 nil))
(cons 0 (cons 1 (cons 2 (cons 3 nil))))
3
```

```lengine id=list-immutable
(require "collections")
(def xs [1 2])
(def ys (+: xs 3))
(println xs)
(println ys)
```
```output
(cons 1 (cons 2 nil))
(cons 1 (cons 2 (cons 3 nil)))
```

### 4.6 Sequences, sets, ranges

```lengine id=seq-basic
(require "collections")
(def s (seq [1 2 3]))
(println s)
(println (head s))
(println (len (+: s 4)))
```
```output
(seq 1 2 3)
1
4
```

Sets keep one copy of each element and print in insertion order.

```lengine id=set-basic
(require "collections")
(def s (set [3 1 3 2]))
(println s)
(println (has? s 1))
(println (len s))
(println (len (tail s)))
```
```output
(set 3 1 2)
true
3
2
```

`(range a b)` is `a` up to but excluding `b`; `(=range a b)` includes `b`. A range whose end is
not after its start is empty.

```lengine id=range-basic
(require "collections")
(println (len (range 0 5)))
(println (len (=range 1 5)))
(println (head (range 3 9)))
(println (+ (head (range 1 5)) 1))
(println (nil? (range 5 1)))
```
```output
5
5
3
2
true
```

### 4.7 Maps and keys

`{:k v …}` builds a map. A key is also a function: `(:k m)` looks up `k`, and a missing key gives
`nil`. Maps are immutable.

```lengine id=map-basic
(require "collections")
(def m {:name "Lengine" :age 7})
(println (:name m))
(println (nil? (:missing m)))
(println (len (keys m)))
```
```output
Lengine
true
2
```

```lengine id=map-update
(require "collections")
(def m {:a 1})
(def m2 (+: m (entry :a 2)))
(println (:a m2))
(println (len m2))
(println (:a m))
```
```output
2
1
1
```

### 4.8 Strings

Strings are sequences of characters. `+` with a string concatenates the printed form of the
other operand.

```lengine id=string-basic
(require "collections")
(println (+ "n=" 3))
(println (len "hello"))
(println (head "hello"))
(println (:upper "hello"))
```
```output
n=3
5
h
HELLO
```

### 4.9 Streams

`('cons x #(rest))` builds a lazy stream; elements are computed on demand, and `take n` computes
only `n` of them.

```lengine id=stream-infinite
(require "collections")
(fn from (n) ('cons n #(from (+ n 1))))
(println (take 3 (from 1)))
```
```output
('cons 1 ('cons 2 ('cons 3 'nil)))
```

---

## 5. Library

The prelude and `collections` exports are listed in `lengine-code/prelude.lg` and
`lengine-code/collections.lg`; `std` and `math` add functions written in Lengine. Representative
checks:

```lengine id=lib-fold
(require "collections")
(println (fold [1 2 3 4] 0 +))
```
```output
10
```

```lengine id=lib-std
(require "collections")
(import std.map)
(import std.filter)
(import std.reverse)
(println (map ^((x) (* x 2)) [1 2 3]))
(println (filter ^((x) (= 0 (rem x 2))) [1 2 3 4]))
(println (reverse [1 2 3]))
```
```output
(cons 2 (cons 4 (cons 6 nil)))
(cons 2 (cons 4 nil))
(cons 3 (cons 2 (cons 1 nil)))
```

```lengine id=lib-math
(require "collections")
(import math.fact)
(import math.max)
(println (fact 5))
(println (max 3 9))
```
```output
120
9
```

```lengine id=lib-format
(println (format "%d+%d" [1 2]))
```
```output
1+2
```

```lengine id=lib-assert
(assert-equals "one equals one" 1 1)
```
```output
PASSED: one equals one
```

```lengine id=lib-assert-fail
(assert-equals "one equals two" 1 2)
```
```runtime-error
Failed to assert: one equals two
```

---

## 6. Errors

* A **compile error** reports the file, line and column, and nothing is generated.
* A **runtime error** is a JVM exception; `try`/`recover` catches it as described in §3.9.
* An unresolved name is a compile error.

```lengine id=err-unresolved
(println undefined-name)
```
```compile-error
Unable to resolve
```
