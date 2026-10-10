# Known bugs

Bugs are tracked as examples in `docs/SPEC.md`: an example tagged `bug=<slug>` states the intended
behavior and fails today. `scripts/spec-status.sh` lists the open ones. As of 2026-10-10 there are
none.

The 2026-10-10 review found 18 confirmed bugs (runtime, library, lexer, compiler). All are fixed
and each is covered by a spec example or by `lengine-runtime/src/test/scala/lengine/RuntimeTest.scala`.

Not yet covered by an example, because the single-module harness cannot express them or they
only matter for a REPL:

- A module's loaded instance is cached for the life of the JVM (`LengineClassLoader`), so a
  REPL that redefines a module would see the stale one.
- `LengineLazyValue` recomputes when its body yields Java `null` (Lengine code yields `nil`, not
  `null`, so only native values can hit this).
- `(eval '(…))` is a compile-time debug form that emits no value.
- Import cycles now fail with "import cycle: a -> b -> a" instead of a stack overflow; there is
  no multi-module test for it yet.
