# Lengine Compiler

The purpose of this Lengine project is to have dedicated compiler along with the engine(REPL).
Currently, target machine is JVM, and LLVM is a future works.
For now, it's experimental. Compiler's structure is now stabilized, but, still grammar & library parts are in development.

So far, all the implements allowed are under [../compiler-example](https://github.com/gkm2164/lengine/tree/master/compile-example)

## How to test?

Create your own `lg` file. 
Refer `./test/scala/co/gyeongmin/lisp/compile/MainTest.scala` to see how to run.

From the project root(`../`), compile the lengine with below commands.

```shell
$ ./install.sh
$ ./lenginec hello.lg
$ ./leng gben.tests.hello # you can find the module name on each files. 
```

To build entire compiler-examples(located in [../compiler-example](https://github.com/gkm2164/lengine/tree/master/compile-example))

```bash
$ ./install.sh
$ ./build-examples.sh
```

You may see a bunch of classes are generated. To clean up, don't be frustrated, simply remove the classes that is
generated.

```bash
$ rm *.class
```

To execute each examples, `leng` script is to execute the classes. This class is simply including `lengine-runtime.jar`
as CLASSPATH, and execute the java code.

```bash
$ ./leng gben.tests.hello
$ ./leng gben.tests.lambda
$ ./leng gben.tests.boolean
```

The name of class is given in the first line of scripts, with `module` declaration.
Please note that, when you write your own lengine script, you should write a sentence with `(module ...)` clause.

Each examples... didn't prepare explanation yet, but, what kind of directives, methods are being used.
And plus, if you have IntelliJ, or some byte code decompiler, you can check how the java binary code is generated.

## How it works?

### 1. Running your compiled binary

With given class name, it will create main which is static method. At the command line, you can execute your binary with
following;

`$ CLASSPATH=$CLASSPATH:./lengine-runtime.jar java Hello ## or, $ ./leng Hello`

### 2. Dynamic Type System

Since the JVM is dealing with both primitive types, and object types, here's how lengine's data types are mapped to each
JVM.

Primitive types are belows.
```
#\n        ;;; char ch = 'n';      // Also known as java.lang.Character
#\Linefeed ;;; char ch = '\n';
3          ;;; long lng = 3;       // Also known as java.lang.Long
#2r1010    ;;; long lng = 0xA;
#16rA      ;;; long lng = 0xA;
10.0       ;;; double dbl = 10.0;  // Also known as java.lang.Double
"Str"      ;;; String str = "Str"; // Also known as java.lang.String
```

Also support for following types
```
#C(10 30) ;;; ComplexNumber cn = new ComplexNumber(10, 30);
5/3       ;;; RatioNumber rn = new RatioNumber(5L, 3L);
```

The compiler will decide a variable's type with resolving value's type without actual calculation.
Since the lisp originally doesn't have the types, it might be tricky when dealing with user's inputs.
So user's input should be basically, "String" type, and should use proper type casting inside lisp languages.

#### 2.1. Decide type for clause.

`(+ a b)` this case, the variable a and b are possible
with `LengineChar`, `LengineInteger`, `LengineDouble`, `LengineString`.

If 2 given types are same, then there's no type casting. However, when there are type variances between a and b, it
will, cast to larger data type.

For example,

```
Char + Integer => (Integer)Char + Integer
Integer + Double => (Double)Integer + Double
Double + String => Double.toString() + String // At this point, actually it's not arithmatic calculation.
```

### 3. Collections

All collections in Lengine are immutable.
List/Sequence/Set/Map/Stream

#### 3.1. List: a single linked list
- Behaves like stack. 

```lengine
(list ?)                == convert ? into list type
(cons 1 nil)            == [1]
(cons 1 (cons 2 nil))   == [1 2]
(list? xs)              == is list?
(nil? xs)               == check xs is nil type
(+: (cons 1 nil) 12)    == Append 12 at the end of list, so, (cons 1 (cons 12 nil))
(++ (cons 1 nil) (cons 2 nil)) == merge to list [1], [2] => (cons 1 (cons 2 nil))
```
- Pros: really fast to append, head/tail/take/drop operations
- Cons
  - It's really slow when put items at the end of the list
  - No support on random access

#### 3.2. Sequence: Doubly linked list
- Can put data from front/back
- 
```lengine
(seq nil)              == conver nil into sequence type
(nil> (seq [1 2 3]))   == get nil type of (seq [1 2 3]) == (seq nil)
(+< (seq [1 2]) 3)     == (seq [1 2 3])
(>+ 12 (seq [1 2]))    == (seq [12 1 2])
```

- Pros: fast at append item in front/back both

#### 3.3. Set: Unordered list

```lengine
(def s (set [1 2 3 4 5]))

(has? s 3) ;;; == true
```

This structure doesn't have orders, so efficient at checking the element is there or not. 

#### 3.4. Map: Object type - Key-value collections

```lengine
{ :key-name values :key-name-2 values-2 } 
```

Hash based key-value storage.

#### 3.5. Stream: Lazy evaluation allowed stream
- This is needed when you want to define/assume a infinite stream.

On the finite world, even time is not infinite. But, we can assume there is an infinite.
```lengine
(`cons 3 nil)

(fn fib-stream (this next)
    ('cons this #(fib-stream next (+ this next))))
    
(def fibs (fib-stream 0 1))

(println fibs)
=> ('cons 0 nil)

(take 10 fibs)
(println fibs)
=> ('cons 0 ('cons 1 ('cons 1 ('cons 2 ('cons 3 ('cons 5 ('cons 8 ('cons 13 ('cons 21 ('cons 33 ...)))))))))) 
```

- Pros: can encapsulate many things related to the user's infinite inputs (from stdin, files, networks)
- Cons: the way it operates is single linked list, also need to use different collection operations. And, this means that you should avoid using same stream over time, as the evaluated values are persisted until the reference is gone.

### 4. Runtime library

TODO - need some description for how LengineRuntime is working, including Prelude.

#### 4.1. Async/Await process

For improving multi-threading, happily announce that this language can do async/await calls.
Behind, Java's future is used under CachedThreadPool executor service.

```lisp
(fn run () (do (println "1234")
               (wait 100) ;; 100ms
               return (println "Finished!")))
               
(def future (async run))

(println "Hello!")

(await future)

(exit 0) 
```

#### 4.2 HTTP Server

![img](https://images.gben.me/images/d1b40b7f-a8e0-4262-8cba-bb31b6e2fa06)
