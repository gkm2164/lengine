# Lengine Compiler

The purpose of this Lengine project to have dedicated compiler along with the engine.
Target machine is JVM.
For now, it's experimental, as it is under development.
So far, all the implements allowed are in `hello.lisp` file.

## How to test?

Create your own `lisp` file.
Refer `./test/scala/co/gyeongmin/lisp/compile/MainTest.scala` to see how to run.

For now, thinking about this interface.

`$ lenginec hello.lisp --className Hello`

## How it works?

### 1. Running your compiled binary

With given class name, it will create main which is static method. At the command line, you can execute your binary with
following;

`$ java Hello`

### 2. Dynamic Type System

Since the JVM is dealing with both primitive types, and object types, here's how lengine's data types are mapped to each
JVM.

```
#\n     ;;; char ch = '\n';     // Also known as java.lang.Character
3       ;;; long lng = 3;       // Also known as java.lang.Long
10.0    ;;; double dbl = 10.0;  // Also known as java.lang.Double
"Str"   ;;; String str = "Str"; // Also known as java.lang.String
```

Will support below types with object type.

```
#C(10 30) ;;; ComplexNumber cn = new ComplexNumber(10, 30);
```

The compiler will decide a variable's type with resolving value's type without actual calculation.
Since the lisp originally doesn't have the types, it might be tricky when dealing with user's inputs.
So user's input should be basically, "String" type, and should use proper type casting inside lisp languages.

#### 2.1. Decide type for clause.

`(+ a b)` this case, the variable a and b are possible with `LengineChar`, `LengineInteger`, `LengineDouble`, `LengineString`.

If 2 given types are same, then there's no type casting. However, when there are type variances between a and b, it will, cast to larger data type.

For example,

```
Char + Integer => (Integer)Char + Integer
Integer + Double => (Double)Integer + Double
Double + String => Double.toString() + String // At this point, actually it's not arithmatic calculation.
```