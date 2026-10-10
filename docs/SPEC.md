# Lengine Language Specification (compiler path, as implemented)

Status: descriptive spec of what the code does today. It is based on `lengine-core` (tokenizer and parser),
`lengine-compiler` (ASM code generation), `lengine-runtime` (Java runtime) and `lengine-code/*.lg`.
Where the code and the READMEs disagree, this spec follows the code. **The compiler is the reference.**
Notes on the older tree-walking interpreter (`lengine-repl`) are marked **[REPL]**.

Path abbreviations used below:

| Abbrev | Path |
|---|---|
| `TOK` | `lengine-core/src/main/scala/co/gyeongmin/lisp/lexer/Tokenizer.scala` |
| `LTOK` | `lengine-core/.../lexer/tokens/LispToken.scala` |
| `SPT` | `lengine-core/.../lexer/tokens/SpecialToken.scala` |
| `PARSER` | `lengine-core/src/main/scala/co/gyeongmin/lisp/parser/package.scala` |
| `CMAIN` | `lengine-compiler/src/main/scala/co/gyeongmin/lisp/compile/Main.scala` |
| `CPKG` | `lengine-compiler/.../compile/package.scala` |
| `VAW` | `lengine-compiler/.../compile/asmwriter/LispValueAsmWriter.scala` |
| `RMV` | `lengine-compiler/.../asmwriter/RuntimeMethodVisitor.scala` |
| `FNW` | `lengine-compiler/.../asmwriter/LispFnAsmWriter.scala` |
| `PI` | `lengine-runtime/src/main/java/lengine/PreludeImpl.java` |

---

## 1. Lexical structure

### 1.1 Source text and token boundaries

* The source is read as a single string. The tokenizer (`TOK`) accumulates characters until it reaches a delimiter.
* **Whitespace** is only the space, tab (`\t`) and newline (`\n`) characters. **Carriage return (`\r`) and comma are not whitespace.** (See §7.)
* **Delimiters**: `(` `)` `[` `]` `{` `}` `"` `;`.
  * An opening `(`, `[` or `{` that follows non-empty accumulated text is *glued* to that text, which produces the compound openers `#(`, `!(`, `^(`, `'(` and `#C(`. Any other prefix, such as `foo(`, becomes an unknown token.
  * A closing `)`, `]` or `}` ends the current token and is emitted as its own token.
  * After `#\`, any delimiter character is taken literally, so `#\(`, `#\)`, `#\[`, `#\"` and `#\;` are character literals.
* **Comments**: `;` starts a comment that runs to the end of the line. `;;;` is the conventional style. The comment body goes through the same escape scanner as strings, so a backslash in a comment behaves like a string escape (§7).
* Line and column numbers are tracked and used in compile errors and in JVM line-number tables.

### 1.2 Token table

| Lexeme | Token / value | Notes |
|---|---|---|
| `(` `)` `[` `]` `{` `}` | punctuation | `[` builds a list literal and `{` builds an object (map) literal |
| `#(` | lazy opener | `#(e ...)` is read as `(lazy (e ...))` |
| `!(` | force opener | `!(x)` is read as `(force x)` |
| `^(` | lambda opener | `^((params) body)` |
| `'(` | token-list opener | `'(a b 1)` becomes a list of token *strings* (§1.6) |
| `#C(` | complex opener | `#C(re im)` |
| `^` | same as `lambda` | `(^ (x) body)` |
| keywords | see §1.3 | reserved and cannot be used as identifiers |
| `true` `false` | Boolean | |
| `#...` | special token | radix integers, characters and type hints (§1.5) |
| numbers | Integer / Float / Ratio | §1.4 |
| `"..."` | String | §1.5 |
| `:name` | object-refer symbol (key) | evaluates to a `LengineMapKey` |
| `,name` | syntactic symbol | only meaningful in `defmacro` parameter lists |
| `name*` | list symbol | rest parameter in `defmacro` |
| `name` | variable symbol | |

### 1.3 Keywords (reserved words)

`module import export require def fn defmacro var let ns lambda native loop for in true false do return case default try catch recover when otherwise`.

Some of these are lexed and parsed but have **no compiler support**: `ns` (parsed to `LispNamespace`), `catch`, and `var` outside `native`. Using them is a compile crash or a parse error.

A second group of names is not reserved by the lexer but is **intercepted by the compiler when it appears in operator position** (`RMV.supportedOps`): `if export import native exit lazy force eval`. A local binding with one of these names is still treated as the special form when it is called.

### 1.4 Numbers (`LTOK`, `SPT`)

| Form | Regex / example | Value |
|---|---|---|
| Integer | `[+-]?\d+`, e.g. `42` `-7` `+3` | `java.lang.Long`. Overflow wraps silently. |
| Float | `[+-]?\d+(\.\d*)?([esfdlESFDL][+-]?\d+)?`, e.g. `5.2` `1.` `1e10` `2d-3` | `java.lang.Double`. Every exponent marker means `E`. `.5` is **not** a number; it is read as a symbol. |
| Ratio | `[+-]?\d+/\d+`, e.g. `3/5` | `lengine.types.RatioNumber`. Not normalized. A zero denominator is a lexing error. |
| Radix integer | `#<base>r[+-]digits`, `#b…` `#o…` `#x…`; e.g. `#2r1010` `#16rFF` `#x-ff` | Long. Digits are `0-9a-zA-Z`, and a digit not below the base is an error. |
| Radix ratio | `#x1/2`, `#2r1/10` | RatioNumber |
| Complex | `#C(3 5)` `#C(4.2 7.6)` `#C(3/5 6/7)` | `lengine.types.ComplexNumber`. Each part must be a plain number token, so radix forms are not allowed. |

### 1.5 Characters, strings, booleans, nil, type hints

* **Characters** use the form `#\c`, where `c` is any single character. Named characters are listed below. **`#\n` is the letter `n`, not a newline.** Use `#\Linefeed` for a newline.

  | Name | Char |
  |---|---|
  | `#\Backspace` | `\b` |
  | `#\Tab` | `\t` |
  | `#\Linefeed` | `\n` |
  | `#\Page` | `\f` |
  | `#\Return` | `\r` |
  | `#\Space` | `' '` |
  | `#\Rubout` | `0x08`. This is a bug: Rubout should be DEL (0x7F). |

  The value is a `java.lang.Character`.
* **Strings** are delimited by `"…"` and may span lines. Supported escapes are `\\ \" \t \n \b \r`. Any other escape is a lexing error. The value is a `lengine.types.LengineString`.
* **Booleans**: `true` and `false` produce `java.lang.Boolean`.
* **nil** is *not* a literal. It is an ordinary variable exported by `collections.lg`, bound to the `Nil` empty list, so `(require "collections")` is needed before using it. The empty stream is `'nil`, also exported by `collections`.
* **Type hints**: `#Bool #Char #Int #Double #String #Fn #Fn1 … #Fn10` are lexed into `LispTypeHint`. The compiler has no case for them, so using one crashes the compiler.
* `()`, the empty clause, parses to `LispUnit`, but the compiler cannot emit it (§7).

### 1.6 Identifiers and symbols

```
VarSymbol      [$.a-zA-Z_\-+/*%<>=?:'&|~][$.a-zA-Z0-9_\-+/*%<>=?:'&|~]*
ListSymbol     same character set, at least 2 chars, ending in '*'     e.g. args*  (but '*' alone is a VarSymbol)
ObjectRefer    :[.a-zA-Z_\-+/*%<>=?][.a-zA-Z0-9_\-+/*%<>=?]*\*?        e.g. :id  :Content-Type  :/home
Syntactic      ,<symbol-chars>                                         e.g. ,else
```

* Identifiers may contain `.`, `'`, `:`, `$`, `?`, `<`, `>`, `|`, `&`, `~`, `%`, `/`. For example `n'`, `+:`, `nil?`, `'cons`, `=range` and `a.b.c` are all ordinary symbols.
* A dotted symbol in `import` is a qualified name: `(import math.pow)` binds the local name `pow`.
* `$` is the **self-reference** of the innermost enclosing lambda (§3.3).
* **Quote, backquote and unquote do not exist.** A leading `'` is just a symbol character, so `'cons` is a symbol named `'cons`. A leading `,` only matters in macro parameter lists.
* `'(…)` is a *token list*. Every token inside becomes a **string** of its textual form, so `'(println "x" 3)` evaluates to the list `["'println'" "\"x\"" "3"]` (`LispTokenValue.toString`). It is used only for the `eval` debugging form (§3.12).

---

## 2. Program structure

### 2.1 Compilation unit = module = JVM class

```lisp
(module gben.tests.hello)          ; MUST be the first form
(require "collections")            ; import all exports of another module
(import math.pow)                  ; import one symbol
(def x 3)
(export answer 42)                 ; export a new binding
(export x)                         ; export an existing binding
```

* The first top-level form **must** be `(module <qualified-name>)`. Otherwise the compiler fails with `unable to determine module's name…` (`CMAIN`). Any other `module` forms in the file are dropped.
* `a.b.C` compiles to the class `C` in package `a.b`, written to `./a/b/C.class`, relative to the **current working directory**. Each function or lambda becomes a separate class file, `C$<fnName>.class` for named `fn`s or `C$lambda$<hash>.class` for anonymous ones, in the same directory.
* The generated class (`CPKG.writeClass`) is `public class C implements lengine.types.LengineObject` and has these members:
  * a private field `export-map : java.util.Map`
  * `public void scriptMain()`, which runs every top-level form in order. **Top-level `def`s are JVM locals of `scriptMain`**, not fields.
  * `public static void main(String[])`, which does `new C().scriptMain()`
  * `private void export(LengineString, Object)`, which stores a value in `export-map`
  * `public Object importSymbol(LengineString)`, which returns the exported value or throws `RuntimeException("No such exported value found: <name>")`
* CLI: `lenginec <file.lg>`, which runs `co.gyeongmin.lisp.compile.Main`. The `--className X` option is parsed but ignored. Run the result with `java -cp lengine-runtime.jar:. a.b.C`. Every module it imports, including `prelude`, must also be compiled and on the classpath.

### 2.2 Prelude inclusion

The compiler prepends `(require "./lengine-code/prelude.lg")` to every file except the prelude itself (`CMAIN`). The path is relative to the CWD, so the compiler must run from the repository root. The prelude's module name is `prelude`, so the class `prelude` must be on the runtime classpath. List and collection functions are **not** in the prelude. They live in `collections.lg` (module `collections`) and must be required explicitly.

### 2.3 `require`

`(require "<path>")`
* This is resolved **at compile time**. The compiler tries `<path>`, `<path>.lg`, `./lengine-code/<path>` and `./lengine-code/<path>.lg`, in that order. If none exists the error is `no such file: <path>`.
* The required file is only *parsed*. The compiler reads its `(module M)` name and its `(export s …)` forms. For each export it emits `(import M.s)`, so the required module must already be compiled.
* If the required file has no `module` form, the require silently does nothing.
* Later imports of the same name overwrite earlier ones. For example, `stdlib` re-exports `take` and `drop`, which shadow the versions from `collections`.

### 2.4 `import`

`(import a.b.M.sym)` compiles to `LengineClassLoader.importSymbol("a.b.M.sym")`:
1. Load class `a.b.M` through the thread context class loader. On the first load of a module in the process, instantiate it and run its `scriptMain()`, which runs any side effects of that module. Modules are cached in a static map.
2. Call `importSymbol("sym")` on the module instance.
3. Bind the local name `sym`, the last dotted segment, in the current scope.

### 2.5 `export`

* `(export sym value)` evaluates `value`, stores it in `export-map` under `"sym"`, **and** binds `sym` locally.
* `(export sym)` exports an existing binding.
* Export is only valid at top level. Inside a lambda, local slot 0 is the lambda object, not the module (§7).
* Export produces no value.

### 2.6 `native`: Java interop

`(native pkg.Class.FIELD fn)` or `(native pkg.Class.FIELD var)` compiles to `GETSTATIC pkg/Class.FIELD`. With `fn` the field type is `LengineLambdaCommon`; with `var` it is `Object`. This is how `prelude.lg` binds runtime functions, for example `(export + (native lengine.Prelude.ADD fn))`. No other Java interop exists.

---

## 3. Special forms

General rules:
* Every expression yields a JVM `Object` reference.
* A clause whose head is not a special form or macro is a **function call** (§3.4).
* Bodies are a **single expression**. Use `do` to sequence several.
* Scoping is lexical. Bindings are JVM locals, and lambdas capture by value (§3.3).

### 3.1 `def`

```lisp
(def name expr)
```
* Evaluates `expr`, binds `name` in the current scope (a top-level, lambda or `let` frame), and returns the value.
* Redefining a name that is **already bound in the same compile environment** fails at compile time with `Can't define symbol twice: name`. Imported names count as bound, so `(def take …)` after requiring `collections` is an error.
* `def` is not recursive. `(def f ^((x) … (f …)))` fails with `Unable to resolve: f`; use `$` inside the lambda instead.

### 3.2 `fn`

```lisp
(fn name (p1 p2 ...) body)
```
* Defines a named function and binds it as `name`. The form itself evaluates to the function object.
* Inside `body`, `name` refers to the function itself (slot 0), so direct recursion works.
* Defining the same `fn` name again does **not** raise an error. The new binding shadows the old one, but both definitions write the same class file `C$name.class` (§7).
* Parameters must be plain symbols. There is no variadic `fn`: `xs*` in a parameter list is just a parameter named `xs*`.
* The maximum arity is **11** (`LengineLambda0` through `LengineLambda11`).

### 3.3 `lambda`, `^`, and closures

```lisp
(lambda (x y) (+ x y))
(^ (x y) (+ x y))
^((x y) (+ x y))
^(() (now))                       ; zero-arg
(^((acc n) ...) 1 n)              ; immediately-invoked
```
* Each lambda is compiled to a class that implements `LengineLambdaN`, where N is the parameter count, with `Object invoke(Object…)`.
* **Captures.** Free variables are found at compile time (`FunctionAnalyzer.captureUnknownVariables`), copied into constructor fields **when the closure is created**, and are immutable. Each captured name must already be bound at that point, otherwise the error is `Unable to resolve: x`. As a result, **forward references and mutual recursion between top-level functions are impossible**.
* **`$`** is bound to the innermost enclosing lambda, including the synthetic lambdas created by `try` (§3.9). Calling `($ args…)` recurses into that lambda.
* **Tail calls.** If any call to the function's own name or to `$` is in tail position (`FunctionAnalyzer.isTailRecursion`), then *every* tail-position self-call is compiled into argument re-assignment followed by `GOTO start`, a loop that uses constant stack. Tail positions are:
  * the `if` then and else branches
  * `case` results and the `default` branch
  * the last expression of `do`
  * a `let` body, if the let does not rebind the name
  * `for…when` results
  * a `loop` body
  
  A self-call in any other position is an ordinary recursive call. `(fn recursion () ($))` is therefore an infinite loop that does not grow the stack (`compile-example/infinity-loop.lg`).

### 3.4 Function application

```lisp
(f a b)            ; f evaluated, cast to LengineLambda2, invoke(a, b)
((:writer res) "x")
((f-3depth 1) 2)
```
* The head can be any expression. Arguments are evaluated left to right, and the head is evaluated first.
* The call site chooses `LengineLambdaN` from the **number of arguments**. An arity mismatch is a runtime `ClassCastException`. All prelude functions have a fixed arity, so `(+ 1 2 3)` fails at runtime.
* Callable runtime values are:
  * lambdas
  * map keys (`(:id obj)`; §4.6)
  * lazy values (`(x)` forces them)
  * unresolved streams (`(xs)` forces them)
  * any native object that implements `LengineLambdaN`
* The call result is check-cast to the type the context expects. For example, an `if` condition expects `Boolean`.

### 3.5 `let`

```lisp
(let ((x 3)
      (y (+ x 1)))     ; sequential: later bindings see earlier ones (let*)
     body)
```
* Bindings are evaluated in order and each is visible to the ones that follow, so this behaves like `let*`. A name may be rebound within the same `let`.
* The form returns the value of `body`.
* When the `let` ends, each bound name is **removed** from the compile environment. If it shadowed an outer binding, **that outer binding is gone** for the rest of the enclosing scope (§7).

### 3.6 `do`

```lisp
(do e1 e2 ... return eN)
(do return eN)
```
* `return` is a **mandatory keyword** before the last expression. The values of `e1 … eN-1` are discarded and the form returns `eN`.

### 3.7 Conditionals

**`if`** is a compiler-intercepted operator:
```lisp
(if cond then else)        ; exactly 3 operands; else is mandatory
```
`cond` must evaluate to a `java.lang.Boolean`. Any other value is a `ClassCastException`, and `null` causes an `NPE`. Only one branch is evaluated. An `if` with 2 or 4 operands crashes the compiler with a Scala `MatchError`.

**`case`** is a multi-way conditional, similar to `cond`:
```lisp
(case ((nil? xs) acc)
      ((= 0 n)   acc)
      default    ($ (+: acc (head xs)) (- n 1) (tail xs)))
```
Each clause is `(test value)`, and the tests must be Booleans. The first true test wins. **`default` is mandatory.**

**`for … when … otherwise`** is a switch on equality:
```lisp
(for (head xs)
 when (#\0 (println "It's 0"))
      (#\1 (println "It's 1"))
 otherwise (println "other"))
```
The target is evaluated once. Each `(v result)` compares `Objects.equals(target, v)` with no numeric promotion. **`otherwise` is mandatory.**

### 3.8 `loop`: comprehension

```lisp
(loop for x in xs body)
(loop for x in (range 0 10)
      for y in (=range 0 x)      ; inner sources may use outer vars
      (seq [x y]))
```
* `loop` needs at least one `for v in src` clause. Without one the compile error is `unable to determine returning sequence`.
* Each source must be both `LengineIterable` and `Buildable`: a list, seq, set, stream, string or range. **A map is not Buildable** and fails with `ClassCastException`.
* The result collection has the **same kind as the source**, taken from `src.BUILDER()`:
  * list to list
  * seq or range to seq
  * set to set
  * stream to stream
  * string to string (the `toString` of each result is concatenated)
* **Nested `for` clauses produce nested collections, not a flattened one.** The outer collection holds one inner collection per outer element.
* Loop variables are removed from scope when the loop ends, with the same caveat as `let`.
* There is no `recur`. Iteration is done with `loop`, `fold` or tail calls.

### 3.9 Error handling: `try` / `recover`

```lisp
(try (/ 3 0)
     (recover e (do (println (:message e))
                    (println (:type e))
                    return 10)))
```
* There is exactly one `recover` clause. It catches **`java.lang.Exception`**, so JVM `Error`s such as `StackOverflowError` are not caught.
* `e` is bound to `LengineException.CONVERT(ex)`, a fresh `LengineException` that wraps the original exception. Its keys are:
  * `:message`: the message string
  * `:type`: `"divide-by-zero(<class>)"` for an `ArithmeticException`, `"runtime-error(<class>)"` for a `LengineRuntimeException`, `"type-mismatch(<class>)"` for a `LengineTypeMismatchException`, and `"unclassified-exception(<class>)"` otherwise
  * `:stack-trace`: the stack trace as a string
  
  Any other key throws.
* The form returns the try value, or the recover value if an exception was caught.
* **Implementation:** when `try` is not the direct body of a function, it is compiled as an immediately-invoked zero-argument lambda. Inside it, `$` therefore refers to that synthetic lambda, and a `def` is local to it.
* There is **no `throw`**. Code can only raise errors indirectly, for example with a failing `assert`.

### 3.10 Lazy values: `#(…)`, `!(…)`, `lazy`, `force`

```lisp
(def x1 #(now))          ; (lazy (now)) -> LengineLazyValue wrapping ^(() (now))
!(x1)                    ; (force x1)  -> evaluates once, caches
(x1)                     ; same as force (LengineLazyValue is a Lambda0)
('cons n #(inf-stream (+ n 1)))   ; lazy tail => infinite stream
```
* `#( … )` wraps the parenthesized form, which may be any clause or special form such as `let`, in a thunk.
* `!(x)` requires exactly one symbol or expression inside the parentheses. `!(f a)` crashes the compiler. `force` casts its operand to `LengineLazyValue`.
* The cached result is only kept when it is non-null. A thunk that yields `null` is re-evaluated on every force.

### 3.11 Macros: `defmacro`

```lisp
(defmacro if' (cond then-clause ,else else-clause)
    (if cond then-clause else-clause))
(if' (= x 20) (println "X is 20!") else (println "X is not 20!"))

(defmacro print-fmt (fmt args*) (printf fmt args))   ; args* = rest -> list literal
(defmacro hello (,my ,name ,is name) (printf "Hello, %s!\n" [name]))
(hello my name is "Gyeongmin!")
```
* Expansion happens at compile time and is a purely syntactic substitution (`LispMacroReplace`). It is **non-hygienic**, there is no compile-time evaluation, and the body is a single template expression.
* Parameter kinds:
  * A plain symbol binds the argument's AST.
  * `,word` requires the literal symbol `word` at that position and consumes it. A mismatch is a compile error.
  * `name*` must be last. It binds `name` to a **`LispList` of the remaining argument ASTs**, which compiles as a `[...]` list literal.
* Macros are registered in the compile environment of the scope where they are defined. **Each lambda body gets a fresh environment with no macros**, so a top-level macro cannot be used inside a `fn` or lambda body (§7).
* The template may not contain `true`/`false`, `{…}` map literals, `def`, or `'(…)`. Any of these causes `Unsupported replacement of macro`.
* Too few arguments for a macro crashes the compiler (`head of empty list`).

### 3.12 Other compiler-intercepted operators

| Form | Semantics |
|---|---|
| `(exit n)` | Compiles to `lengine.runtime.ExitHandler.exit((int) n)`, which calls `System.exit` unless a host (REPL, test) installed another handler. The `exit` exported by the prelude goes through the same handler and is only used when `exit` is passed as a value. |
| `(eval '(…))` | **Debug only.** At *compile time* it prints the token list and the parsed clause. It emits no code and pushes no value (§7). |
| `(import …)`, `(export …)`, `(native …)` | See §2. |

---

## 4. Values and types

### 4.1 Runtime representation

| Lengine value | JVM class | Literal / constructor | `println` form |
|---|---|---|---|
| Boolean | `java.lang.Boolean` | `true` `false` | `true` |
| Character | `java.lang.Character` | `#\a` | `a` |
| Integer | `java.lang.Long` | `42` `#xFF` | `42` |
| Float | `java.lang.Double` | `5.2` | `5.2` |
| Ratio | `lengine.types.RatioNumber` | `3/5` | `3/5` |
| Complex | `lengine.types.ComplexNumber` | `#C(1 2)` | `{1 + 2i}` |
| String | `lengine.types.LengineString` | `"…"` | raw text |
| Unit | `lengine.types.LengineUnit` | returned by `println` and similar | `()` |
| List | `lengine.types.collections.LengineList` (`Cons` / `Nil`) | `[1 2 3]`, `cons`, `nil`, `list` | `(cons 1 (cons 2 nil))` |
| Sequence | `LengineSequence` (`LeafSequence` / `NonLeafSequence`) | `(seq [..])`, `range` results in a loop | `(seq 1 2 3)` |
| Set | `LengineSet` | `(set [..])` | `(set a b)` |
| Map ("object") | `LengineMap` | `{ :k v … }` | `{:k => v, …}` |
| Map key | `LengineMapKey` | `:k`, `(key "k")` | `:k` |
| Map entry | `LengineMapEntry` | `(entry :k v)` | `(entry :k -> v)` |
| Range | `RangeSequence` | `range`, `=range` | `[Ranged sequence from a to b]` |
| Stream | `LengineStream` (`StreamCons` / `StreamNil` / `UnresolvedStream`) | `'cons`, `'nil`, `stream` | `('cons 1 ...)` |
| Lazy value | `LengineLazyValue` | `#(…)` | Java default `toString` |
| Function | generated class that implements `LengineLambdaN` | `fn`, `lambda`, `^` | Java default `toString` |
| Future | `lengine.concurrency.LengineFuture` | `async` | |
| Channel | `lengine.concurrency.LengineChannel` | `chan` | |
| Exception | `LengineException` | the `recover` binding | |
| File reader | `LengineFileReader` | `read-file` | |

There are no static types. Every value is an `Object`, and the compiler inserts `CHECKCAST`s only where a specific runtime type is required: `Boolean` for conditions, `LengineLambdaN` for calls, `LengineIterable` for `loop` sources.

### 4.2 Truthiness and booleans

There is no generalized truthiness. Conditions in `if` and `case` **must** be `java.lang.Boolean`. `and`, `or` and `not` are ordinary *functions*, so they are not short-circuiting, and they accept only Booleans.

### 4.3 Numeric tower and promotion (`PI.getRank`, `PI.cast`)

The binary operators `+ - * / rem` and the comparisons `< <= > >=` first convert both operands to the **higher-ranked** type:

| Rank | 0 | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|---|
| Type | Character | Long | Ratio | Double | Complex | String |

Any other operand class, including Boolean, Integer and collections, raises `RuntimeException("Unable to decide rank for type …")`.

| Operation | Result |
|---|---|
| Long op Long | Long. `/` truncates (`(/ 3 5)` is `0`). Dividing by zero raises `ArithmeticException`. |
| Long op Double | Double |
| Long or Char op Ratio | Ratio (not normalized). Use `norm` or `(:norm r)` to normalize. |
| Ratio op Double | Double |
| x op Complex | Complex. Long, Char and Ratio convert to `#C(x 1)`, **with an imaginary part of 1, not 0**. Double cannot be converted to Complex and raises a type mismatch. |
| anything `+` String | String concatenation using `toString`: `(+ name 123)` gives `"YoonJung123"`. `- * /` on strings throws. |
| Char op Char | Java `int` arithmetic, which yields a **`java.lang.Integer`**. That value has no rank afterwards (§7). |
| `rem` | Defined for Char, Long and Double only (Java `%`). |
| Comparisons | Defined for Char, Long and Double. String, Ratio and Complex do not implement `Comparable` and throw. |

### 4.4 Equality

`=` and `/=` use `java.util.Objects.equals`, **with no numeric promotion**. `(= 1 1.0)` is `false`, and so is `(= 1 #\1)`. The same rule applies to `for … when` matching and to `assert-equals`.

| Type | Equality |
|---|---|
| Strings, chars, Longs, Doubles, Booleans, map keys | by value |
| `Cons` lists | structural. `Nil` is a singleton. |
| Sequences | element-wise. A `LeafSequence` only equals another `LeafSequence` (§7). |
| Sets | set equality |
| Maps | same keys and the same values |
| Ratio, Complex | **identity only**. They have no `equals`. |
| List vs seq | never equal |

### 4.5 Collections

All collections implement `LengineIterable` (`iterator`, `len`, `head`, `tail`, `IS_NIL`). Most collection operations return a new collection. The exceptions are noted below.

* **List** (`Cons`/`Nil`) is a singly linked list.
  * `[a b c]` builds a list, `nil` is the empty list, and `cons` prepends.
  * `head` of `nil` throws `nil can't have a value`. `tail` of `nil` returns `nil`.
  * **`+:` / `+<` (append) on a non-empty list mutates the existing cons cells in place** and returns the same object.
* **Sequence** is a persistent, array-backed sequence created with `seq` or produced by a loop over a range or seq. Appending creates a `NonLeafSequence` tree.
* **Set** is a hash set. `+:`, `>+` and `+<` all add. Order is unspecified.
* **Map** comes from `{ :k v }` literals. Its keys are `LengineMapKey`s. It is immutable: `(+: m (entry :k v))` returns a new map. Iterating a map, as `fold` does, yields entries. A map is not usable as a `loop` source.
* **Range**:
  * `(range a b)` is the half-open range `[a, b)`, and `(=range a b)` is the inclusive range `[a, b]`.
  * Bounds are truncated to `int`.
  * The iterator yields Longs, but **`head` returns an `Integer`** (§7).
  * A range is not Addable, so the native `take` rejects it.
* **String** is iterable over its characters. `head` returns a Character and `tail` returns a String. The empty string counts as nil for `nil?`.
* **Stream** is a lazy cons stream.
  * `('cons v tail)` takes a tail that is a lazy value (`#(…)`), a stream or `'nil`.
  * `(stream x)` converts a lazy value to an unresolved stream. An existing stream is returned unchanged, and any other value becomes a one-element stream.
  * Iterating forces unresolved tails. `take` stops after n elements, so taking from an infinite stream terminates.
  * `println` shows the resolved prefix followed by `...`.

### 4.6 Objects and keys

`:key` evaluates to a `LengineMapKey`. A key is also a one-argument function that calls `obj.get(key)` on any `LengineObjectType`:

| Receiver | Supported keys |
|---|---|
| Map | Any key. A missing key returns **`nil`** (the empty list), not `null`. |
| String | `:length` `:lower` `:upper` `:trim` |
| Ratio | `:over` `:under` `:norm` |
| Complex | `:real` `:imagine` `:inverse` |
| Exception | `:message` `:type` `:stack-trace` |
| File reader | `:get-char` returns the next char, or `(char -1)` at EOF. `:close` returns a 0-arg function that closes the reader. |
| HTTP request | `:path` `:query` `:method` `:headers` `:request-body` (a stream that can be read once) |
| HTTP response | `:set-status-code` `:set-headers` `:writer` (all return functions) |

An unknown key on a non-map object throws `RuntimeException`.

### 4.7 Concurrency

* **Futures:**
  * `(async f)` runs the zero-argument function `f` on a shared cached thread pool and returns a `LengineFuture`.
  * `(await fut)` blocks and returns the result. A failure is rethrown as a `CompletionException`.
* **Channels:**
  * `(chan)` creates an unbounded queue.
  * `(send ch v)` enqueues `v`. Sending to a closed channel throws.
  * `(receive ch)` spin-waits with `Thread.yield` until a message arrives. After `close`, the next `receive` returns `nil` and marks the channel closed. A further `receive` throws.
  * `(close ch)` enqueues a close marker.
* `(wait ms)` calls `Thread.sleep`.

---

## 5. Built-in functions

Arity is exact unless noted otherwise. Argument order follows the Java lambda signatures in `PI`. Entries marked `?` were inferred and not exercised.

### 5.1 Prelude (`lengine-code/prelude.lg`, always available)

**Arithmetic and bitwise**

| Name | Arity | Description |
|---|---|---|
| `+` | 2 | Add with promotion (§4.3). Concatenates when either operand is a string. |
| `-` | 2 | Subtract |
| `*` | 2 | Multiply |
| `/` | 2 | Divide. Truncates for Longs and throws `LengineRuntimeException` for unsupported types. |
| `rem` | 2 | Remainder (Char, Long, Double) |
| `norm` | 1 | Normalize a ratio. Only correct for positive values (§7). |
| `&` | 2 | Bitwise AND on Long or Char. Throws on Booleans (§7). |
| `\|` | 2 | Bitwise OR on Long or Char |
| `~` | 1 | Bitwise NOT on Long or Char. Logical NOT on a Boolean. |

**Logic and comparison**

| Name | Arity | Description |
|---|---|---|
| `and`, `or` | 2 | Boolean AND / OR. Not short-circuiting. |
| `not` | 1 | Boolean NOT |
| `<` `<=` `>` `>=` | 2 | Compare after promotion (Char, Long, Double only) |
| `=` `/=` | 2 | `Objects.equals` and its negation |

**I/O and assertions**

| Name | Arity | Description |
|---|---|---|
| `println` | 1 | Print `toString` and a newline. Returns Unit. |
| `print` | 1 | Print without a newline. Returns Unit. |
| `printf` | 2 | `(printf fmt iterable-args)` uses Java `printf`. Returns Unit. |
| `format` | 2 | `(format fmt iterable-args)` returns a String. |
| `assert` / `assert-true` | 2 | `(assert msg bool)`. Prints `PASSED: msg`, or throws `RuntimeException("Failed to assert: msg")`. |
| `assert-false` | 2 | Asserts that the value is false |
| `assert-equals` | 3 | `(assert-equals msg a b)` using `=` |
| `assert-not-equals` | 3 | Asserts that `a` and `b` are not equal |

**Types and conversion**

| Name | Arity | Description |
|---|---|---|
| `bool` | 1 | Parses `toString()` equal to `"true"` or `"false"`. Anything else throws. |
| `int` | 1 | Converts Char, Long, Double (truncated) or String (`Long.parseLong`) to Long |
| `double` | 1 | Converts Char, Long, Ratio, Double or String to Double |
| `char` | 1 | Converts Char, Long or Double to Char. `(char -1)` is `'￿'`, the EOF sentinel. |
| `string` | 1 | `toString` as a LengineString |
| `bool?` `int?` `double?` `char?` `string?` | 1 | Type tests. `int?` checks for `Long`. |
| `nan` | var | `Double.NaN` |
| `nan?` | 1 | Takes a Double |

**System**

| Name | Arity | Description |
|---|---|---|
| `now` | 0 | `System.currentTimeMillis()` as a Long |
| `exit` | 1 | Exit the process. The call form is intercepted by the compiler (§3.12). |
| `wait` | 1 | Sleep for the given number of milliseconds |

**Concurrency**

| Name | Arity | Description |
|---|---|---|
| `async` | 1 | `(async thunk)` returns a future |
| `await` | 1 | Block on a future and return its result |
| `chan` | 0 | Create a channel |
| `send` | 2 | `(send ch v)` |
| `receive` | 1 | Blocking receive. Returns `nil` after `close`. |
| `close` | 1 | Close a channel |

**Network and database**

| Name | Arity | Description |
|---|---|---|
| `listen` | 1 | `(listen {:host s :port n :handlers {:/path {:GET f :POST g :ALL h}}})` starts a JDK `HttpServer`. Handlers are `(fn (req res))`. Returns a 0-arg stop function. A missing method handler responds 404, and an exception responds 500. |
| `db-conn` | 3 | `(db-conn url user pass)` loads the MySQL driver and returns a map `{:exec-select (q args) → seq of maps, :exec-update (q args) → Long}`. **?** The Java signature expects `java.lang.String` and Lengine passes a `LengineString` (§7). |
| `help` | 1 | Seq of the supported keys of an object with help (Ratio, Complex, HTTP request/response) |

### 5.2 `collections.lg` (requires `(require "collections")`)

**Sequence operations**

| Name | Arity | Description |
|---|---|---|
| `len` | 1 | Element count. Forces a stream completely. |
| `head` | 1 | First element |
| `tail` | 1 | Everything after the first element |
| `take` | 2 | `(take n coll)` returns the same collection kind. The collection must be Nillable and Addable, so a range or map is rejected. |
| `drop` | 2 | `(drop n coll)` |
| `fold` | 3 | `(fold coll init (fn (acc elem)))` is a left fold |
| `+:` | 2 | `(+: coll item)` appends; on a map, adds an entry. **Mutates lists.** |
| `+<` | 2 | `(+< coll item)` appends at the rear (same caveat) |
| `>+` | 2 | `(>+ item coll)` prepends |
| `++` | 2 | Merges string + string, list + iterable, seq + iterable, or set + set/iterable |

**Collection constructors and tests**

| Name | Arity | Description |
|---|---|---|
| `list` | 1 | From a string (to chars) or a list |
| `list?` | 1 | Is a list |
| `seq` | 1 | From a string or any iterable (`(seq nil)` is an empty seq) |
| `seq?` | 1 | Is a seq |
| `cons` | 2 | `(cons x list)` |
| `cons?` | 1 | Is a `Cons` |
| `nil` | var | The empty list |
| `nil?` | 1 | `IS_NIL` of an iterable (also true for `null`). A non-iterable throws `ClassCastException`. |
| `nil>` | 1 | Empty collection of the same kind (`Nillable.NIL`) |
| `set` | 1 | From a string or an iterable |
| `set?` | 1 | Is a set |
| `has?` | 2 | `(has? set x)` or `(has? map :k)`. False for anything else. |
| `range` | 2 | `[a, b)` |
| `=range` | 2 | `[a, b]` |

**Objects and keys**

| Name | Arity | Description |
|---|---|---|
| `object?` | 1 | Is a map |
| `key` | 1 | String to key |
| `get` | 1 | Key to String name |
| `keys` | 1 | Map to set of keys |
| `entry` | 2 | `(entry :k v)` |
| `entries` | 1 | Map to seq of entries |

**Streams**

| Name | Arity | Description |
|---|---|---|
| `'cons` | 2 | Stream cons. The tail is a lazy value, stream or `'nil`. |
| `'cons?` | 1 | Is a stream cons |
| `'nil` | var | Empty stream |
| `'nil?` | 1 | Is the empty stream |
| `'unresolved?` | 1 | Is an unforced lazy stream |
| `stream` | 1 | Coerce to a stream |
| `stream?` | 1 | Is a stream |

**Input**

| Name | Arity | Description |
|---|---|---|
| `read-file` | 1 | Path to a reader object (`:get-char`, `:close`) |
| `read-char` | 0 | Reads one char from stdin. **Broken**: it uses the key `:read`, which the reader does not support (§7). |

### 5.3 `stdlib.lg` (module `std`) and `math.lg` (module `math`)

These are written in Lengine and need `(require "stdlib")` or `(require "math")`.

**stdlib: sequences**

| Name | Arity | Description |
|---|---|---|
| `take`, `drop` | 2 | Recursive versions that work on any nil-able iterable, including streams. They shadow the native ones. |
| `map` | 2 | `(map f xs)`. Lazy for unresolved streams. |
| `filter` | 2 | `(filter p xs)`. Lazy for unresolved streams. |
| `filter-not` | 2 | Complement of `filter` |
| `take-while` | 2 | `(take-while p xs)` |
| `fold-right` | 3 | `(fold-right xs init f)` where `f` is `(elem acc)` |
| `reduce` | 2 | `(reduce xs f)` |
| `reverse` | 1 | Reverse a collection |
| `flatten` | 1 | Flatten nested seqs |
| `contains` | 2 | `(contains xs x)` |
| `n-th` | 2 | `(n-th xs n)` |

**stdlib: strings and output**

| Name | Arity | Description |
|---|---|---|
| `to-string` | 1 | Folds with `+` into a string |
| `split` | 2 | `(split str delim-char)` |
| `join` | 2 | `(join xs delim)` |
| `debug` | 1 | Prints the value and returns it |

**stdlib: functions and math**

| Name | Arity | Description |
|---|---|---|
| `compose` | 2 | `(compose f g)` returns `x → f(g(x))` |
| `fact` | 1 | Factorial |

**stdlib: streams and input**

| Name | Arity | Description |
|---|---|---|
| `file-stream` | 1 | Path to a lazy char stream |
| `stdin-stream` | 0 | Lazy char stream from stdin. Depends on the broken `read-char`. |
| `read-through` | 1 | Stream to string, up to EOF |
| `read-line` | 1 | `(read-line stream-thunk)` |

**math**

| Name | Arity | Description |
|---|---|---|
| `fact` | 1 | Factorial |
| `sum` | 1 | Sum of a sequence |
| `product` | 1 | Product of a sequence |
| `max`, `min` | 2 | Larger or smaller of two values |
| `pow` | 2 | `(pow a n)` for integer `n` |
| `sqrt` | 1 | 10 Newton iterations |
| `floor`, `ceil` | 1 | Return Long |

### 5.4 Runtime functions not exported by any `.lg` file

`Prelude.HELP_KEYWORD` (`help` with a key) exists, but `prelude.lg` does not export it, and it is package-private in `PI`. `lengine-compiler/src/main/java/lengine/runtime/ExportSymbols.java` is an unused reflection table. It is not part of the language.

---

## 6. Errors

### 6.1 Compile time (`lenginec`)

`CMAIN` catches `RuntimeException`, prints `[ERROR]: <message>` and exits with code 1. No class file is written for the failing module, although lambda class files emitted before the failure may already be on disk.

| Phase | Error / message | Cause |
|---|---|---|
| Lex | `[ERROR] Lexing error: …` printed to stdout. **Compilation continues** without the token. | Unknown token (`what is it? [...]`), bad escape, zero ratio denominator. A bad radix digit is reported later, at parse. |
| Parse | `while parse: <msg>` (`UnexpectedTokenError`, `EmptyTokenListError`, `DeniedKeywordError`) | Malformed forms: a missing `return`, `default` or `otherwise`, an unclosed paren, `{` with a non-key, etc. |
| Module | `unable to determine module's name…` | The first form is not `(module …)` |
| Require | `no such file: <path>` | |
| Codegen | `CompileException` — `<msg> at <file>:<line>:<col>` | `Unable to resolve the symbol: x`; `Unable to resolve: x` (capture); `Can't define symbol twice: x`; `functions placeholders should be symbol`; `unable to determine returning sequence.` (`loop` with no `for`); macro errors (`list symbol … should be located at last`, `… is not matched with …`, `Unsupported replacement of macro`) |
| Codegen (crash) | Scala `MatchError`, `IndexOutOfBoundsException`, `NoSuchElementException` | An `if` without exactly 3 operands; `!(f a)`; `()`; `ns`; `#Int`; more than 11 call arguments; too few macro arguments |

### 6.2 Run time

All runtime errors are unchecked Java exceptions. An uncaught exception terminates the program with a JVM stack trace. `try`/`recover` catches any `java.lang.Exception`.

| Exception | Typical source |
|---|---|
| `lengine.runtime.exceptions.LengineException` (extends `RuntimeException`, implements `LengineObjectType`) | The wrapper bound by `recover` |
| `LengineRuntimeException` (extends `LengineException`) | `/` on unsupported types |
| `LengineTypeMismatchException` (extends `LengineException`) | Failed numeric or collection casts (`int`, `char`, `list`, promotion) |
| `ArithmeticException` | Long division by zero; `RatioNumber` with denominator 0; complex inverse with a zero divisor |
| `ClassCastException` | A non-Boolean condition; calling a non-function or calling with the wrong arity; a map as a `loop` source; `nil?` on a non-iterable |
| `RuntimeException` (plain) | Prelude type errors (`Can't add …`, `Unable to decide rank …`); `Failed to assert: …`; `No such exported value found: x`; a missing module class (wrapped `ClassNotFoundException`); unknown object key; channel misuse |
| `NullPointerException` | Java `null` reaching an operator, for example a missing JDBC column value |

---

## 7. Known gaps, ambiguities and surprises

1. **Lexing errors are not fatal.** `Tokenizer.streamLoop` prints the error, inserts a no-op and keeps going (`TOK`). A typo can silently delete a token.
2. **CR and comma are not whitespace.** CRLF sources produce lexing errors at line ends, and so do tokens followed by `\r` (`TOK`, the whitespace match lists only space, tab and newline).
3. **Comments run through the string-escape scanner.** A comment containing `\x` raises an escape error, after which the remainder of the comment is lexed as code. A comment ending in `\` continues onto the next line (`TOK.parseString` called with the `'\n'` terminator).
4. **`#\n` is the letter n, and `#\Rubout` is 0x08**, not DEL (`SPT.realize`).
5. **Scope removal on `let` and `loop` exit.** `deregisterVariable` removes the name entirely, so an outer binding with the same name becomes unresolvable after the inner scope (`VAW` LispLetDef; `LispLoopAsmWriter`). In `compile-example/macro.lg`, the top-level `x` stops being visible after `(for' x …)`. A `for` variable is also registered *before* its source expression is compiled, so in `for x in (f x)` the inner `x` refers to the new binding.
6. **Macros are invisible inside lambdas.** `LengineRuntimeEnvironment` creates a fresh `macros` map for each function (`FNW.createLambdaClass`). Macro templates also cannot contain `true`/`false`, maps or `def` (`LispMacroReplace`).
7. **No forward references and no mutual recursion.** Captures are resolved and copied when a closure is created (`FNW.writeValue`).
8. **Fixed arity everywhere.** `(+ 1 2 3)` compiles but fails at runtime with `ClassCastException`. A call with more than 11 arguments crashes the compiler (`LengineType.LengineLambdaClass`). `fn` has no varargs.
9. **The numeric tower is incomplete or buggy** (`PI`, `RatioNumber.java`, `ComplexNumber.java`):
   * Char ± Char yields a `java.lang.Integer`, and `~` and `&` on chars do too. `RangeSequence.head` returns an `int`. These Integers then fail every operation with "Unable to decide rank".
   * Long, Char and Ratio convert to Complex with an imaginary part of **1**.
   * `ComplexNumber.inverse` divides by a² − b² instead of a² + b², so `#C(1 1)` cannot be inverted. `compile-example/error-handling.lg` relies on this.
   * Ratio and Complex have no `equals` or `compareTo`, and ratios are never normalized automatically. `RatioNumber.normalize` does not work for negative values.
   * `&` and `|` on Booleans throw before they reach the Boolean branch.
10. **Mutation in a "functional" API.** `+:`/`+<` on a `Cons` list mutates it in place (`LengineList.add`). `Nil.append` adds the whole `ys` once per element instead of each element (`Nil.java`). `LeafSet.tail` keeps only the head instead of removing it (`LeafSet.java`).
11. **Asymmetric sequence equality.** `LeafSequence.equals` requires both sides to be `LeafSequence`, but `LengineSequence.equals` compares elements, so `(= a b)` can differ from `(= b a)`.
12. **Unbalanced top-level stack.** Each `require` or `import` leaves imported values on the `scriptMain` operand stack, because they are not popped (`CPKG.writeExecute`, `RMV.visitImport` dup). `defmacro` and `eval` push nothing but are still followed by a `POP`. Programs work only because the leftover import values absorb those pops. A file with no `require` whose first form is `defmacro` probably fails JVM verification. **?** This was not tested.
13. **`export` inside a function body** loads slot 0, which is the lambda rather than the module, so it would produce invalid bytecode (`RMV.visitExport`).
14. **Class-file name collisions.** Each named `fn` is written to `<Module>$<name>.class`, so two `fn`s with the same name in one module overwrite each other, even in different scopes (`FNW.createLambdaClass`). Names containing `/` or `.` produce invalid paths.
15. **Parsed but uncompilable forms**: `()`, `ns`, `catch`, `#Int`-style type hints, `var` outside `native`, and a bare `fn` used as a value all hit a `MatchError` in `VAW`. `if` with 2 operands also crashes, because `else` is mandatory.
16. **I/O helpers are broken.**
    * `read-char` asks the reader for `:read`, which `LengineFileReader` does not support, so `stdin-stream` and `read-line` in stdlib cannot work.
    * `(:close h)` returns a closer function but does not call it, so the `file-stream` reader in stdlib never closes its file.
    * **?** `db-conn` lambdas are typed `String`, but they receive `LengineString` query and argument values, which probably causes a `ClassCastException` or "Unknown type". `compile-example/sqls.lg` also uses `nil` without requiring collections.
17. **Lazy values cache only non-null results** (`LengineLazyValue.force`).
18. **`LengineClassLoader` type check is inverted.** It calls `cls.isAssignableFrom(LengineObject.class)`, so the "not a LengineObject" guard never fires correctly.
19. **Environment assumptions.** The prelude path `./lengine-code/prelude.lg` and the output paths are relative to the CWD. Self-detection of the prelude uses `File.equals`, which compares path strings, so compiling the prelude by another path makes it require itself (`CMAIN`).
20. **`eval` does not evaluate.** It prints the parsed form at compile time and emits nothing.
21. **`try` changes the meaning of `$`.** Inside a non-body `try`, `$` refers to the synthetic thunk. A tail self-call inside a `try` is not optimized.
22. **`loop` bodies count as tail positions** in `isTailRecursion`, so a `$` call inside a loop body would `GOTO` the function start while the iterator is still on the operand stack. **?** This has not been verified at runtime.

### 7.1 Differences in the old interpreter (`lengine-repl`)

* It evaluates Scala `LispValue`s directly and has no `module`, `require`, `export`, `native`, `try`, `case`, `for…when` or `loop` result typing. Unsupported forms report `value is not handlable yet`.
* `if` is a builtin function with lazy parameters, not a special form.
* `loop` results are **flattened** into a single list, whereas the compiler produces nested collections.
* `fn` definitions are overloadable: several definitions with literal-pattern parameters are tried in order, and `name*` rest parameters are supported in `fn`.
* `(import "path")` takes a file path string.
* Builtin names differ from the compiler's prelude. The REPL has `%` (not `rem`), `float` (not `double`), `neg`, `quit` and `history`, and has no channel, async, HTTP or DB functions.
