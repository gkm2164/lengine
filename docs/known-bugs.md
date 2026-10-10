# Known bugs (review of 2026-10-10)

Found by reading the code and running small programs against the compiler and runtime at
`upgrade/2026-10`. "Confirmed" means a repro was run. File references are to that tree.
Check items off as they are fixed, with the commit that fixes them.

## Compiler and parser

- [ ] **Self tail calls clobber arguments.** `LispClauseWriter.scala:19-23` stores each new argument
  before computing the next, so later arguments see new values.
  `(fn fib (a b n) (if (= n 0) a (fib b (+ a b) (- n 1))))` gives 512 for `(fib 0 1 10)`, not 55. Confirmed.
  Fix: evaluate all arguments onto the stack, then store in reverse.
- [ ] **Same-named functions share one class file.** `LispFnAsmWriter.scala:56-59,87,298` names the
  class `<Cls>$<fnName>`; two `fn helper` in different scopes overwrite each other and mix captures.
  Names containing `/` produce invalid paths. Confirmed.
- [ ] **Closure analyzer has no case for many values.** `FunctionAnalyzer.scala:48-103`: ratio/complex
  literals, quoted lists, nested `fn`, `()`, `defmacro`, `import`, `native`, type hints inside a lambda
  give `scala.MatchError`. `LispValueAsmWriter` also has no case for `()`. Confirmed.
- [ ] **Macros are not visible inside function bodies.** Each function gets a fresh
  `LengineRuntimeEnvironment` with an empty macro map (`LengineRuntimeEnvironment.scala:20`,
  `LispFnAsmWriter.scala:227`). Confirmed.
- [ ] **Shadowing deletes the outer binding.** `let`/`loop` remove their names on exit instead of
  restoring the outer binding (`LispValueAsmWriter.scala:107-114`, `LispLoopAsmWriter.scala:91`).
  `(let ((x 1)) (do (let ((x 2)) x) return x))` fails to compile. Confirmed.
- [ ] **A loop body is treated as a tail position.** `FunctionAnalyzer.scala:131-135`; a self call inside
  `loop` jumps to the function start with the iterator still on the stack. Compiler crash
  ("Index 0 out of bounds"). Confirmed.
- [ ] **Char literals above U+7FFF crash at runtime.** `AsmHelper.scala:119` uses a signed 16-bit push;
  `(println #\가)` throws `ArrayIndexOutOfBoundsException`. Confirmed.
- [ ] **Tokenizer:** comments go through the string reader with escapes (`Tokenizer.scala:65-68`), so a
  comment ending in `\` swallows the next line and `; C:\foo` breaks lexing; `\r` is not whitespace
  (CRLF files fail, `Tokenizer.scala:53`); integer literals wrap on overflow (`LispToken.scala:93-99`,
  radix parser in `SpecialToken`). Confirmed.
- [ ] **Type mismatches fail class verification, not at runtime.** `AsmHelper.scala:306` only casts when
  the known type is `Object`; `(def n 5) (if n 1 2)` gives `VerifyError` and nothing in the module runs.
  Confirmed. Matters for a REPL: one bad line poisons the whole unit.

## Runtime

- [ ] **Complex numbers:** int-to-complex sets imaginary part 1 (`PreludeImpl.java:514-518`); no
  double-to-complex; `ComplexNumber.inverse` (`:108`) divides by a²−b² instead of a²+b². Confirmed.
- [ ] **Equality and ratios:** `=` is `Objects.equals`, and `RatioNumber`/`ComplexNumber`/
  `LengineMapEntry` lack `equals`, so `(= 1/2 1/2)` and `(= 1 1.0)` are false; ratios are never reduced;
  `normalize` skips negatives; `(< 1/3 1/2)` throws. Confirmed.
- [ ] **`Integer` leaks where `Long` is expected:** char arithmetic, `(head (range 1 5))`
  (`RangeSequence.java:46`); later arithmetic throws "Unable to decide rank". `&`/`|` on booleans always
  throw (rank lookup before the Boolean check, `PreludeImpl.java:396-402`). Confirmed.
- [ ] **Lists:** `LengineList.add` mutates in place (`LengineList.java:106-113`); `Nil.append` adds the
  whole collection once per element (`Nil.java:31`). Confirmed.
- [ ] **Map update keeps the old value** (`LeafMap.putEntry` with `NonLeafMap.get` preferring the left
  side). Confirmed.
- [ ] **equals/hashCode disagree:** `LeafSequence` vs `NonLeafSequence`; `LengineSet` has no `hashCode`;
  equal maps hash differently; `LeafSet.tail` filter is inverted (`LeafSet.java:65`). Confirmed.
- [ ] **`take n` forces one extra element** (`LengineIterator.forEachN` checks `hasNext()` first). Confirmed.
- [ ] **Backward ranges:** `(range 5 1)` has `len` 4 and is not `nil?`, but iterates nothing. Confirmed.
- [ ] **Looping over a map throws `ClassCastException`** (maps are not `Buildable`). Confirmed.

## Risky, not proven

- `require` leaves a value on the stack per import (`LispCompileDirectiveAsmWriter.scala:72-83`).
- `eval` pushes no value, so its statement pop underflows (`RuntimeMethodVisitor.scala:223-242`).
- `denySymbolIfContains` skips the forbidden-name check on separators (`parser/package.scala:146`).
- Module loading: import cycles recurse forever; the loaded-module cache is static (stale in a REPL);
  the `isAssignableFrom` check in `LengineClassLoader.java:34` is backwards.
- `for … when` with no `when` crashes; macro called with too few args throws; macro bodies with
  `true`/`false`/`{…}` are rejected; `#x1/0` skips the zero check; `Rubout` maps to 0x08;
  `LengineLazyValue` re-runs when the result is null; `(if c x)` crashes with a raw `MatchError`.
- For a REPL: function class files are written to the current directory through a stream that is
  never closed, and redefining a name is a compile error.

Fixed on `upgrade/2026-10`: compiled `(exit n)` called `System.exit` directly (now `ExitHandler`);
compiling the prelude without a leading `./` made it require itself.
