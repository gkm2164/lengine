package lengine;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiPredicate;

import lengine.functions.LengineLambda0;
import lengine.functions.LengineLambda1;
import lengine.functions.LengineLambda2;
import lengine.functions.LengineLambda3;
import lengine.functions.LengineLambdaCommon;
import lengine.runtime.Cons;
import lengine.runtime.CreateIterator;
import lengine.runtime.FileSequence;
import lengine.runtime.LengineIterator;
import lengine.runtime.LengineList;
import lengine.runtime.LengineListIterator;
import lengine.runtime.LengineMap;
import lengine.runtime.LengineMapEntry;
import lengine.runtime.LengineUnit;
import lengine.runtime.Nil;
import lengine.runtime.RangeSequence;
import lengine.runtime.Sequence;

public class Prelude {
  private static final Set<String> alreadyLoadedClass = new HashSet<>();

  private final static LengineUnit UNIT = LengineUnit.create();

  private static Object cast(Object from, Class<?> to) {
    if (to.equals(Character.class)) {
      return castChar(from);
    } else if (to.equals(Long.class)) {
      return castLong(from);
    } else if (to.equals(Double.class)) {
      return castDouble(from);
    } else if (to.equals(String.class)) {
      return castString(from);
    } else if (to.equals(Sequence.class)) {
      return castSequence(from);
    }

    throw new RuntimeException("unable to cast");
  }

  private static Sequence castSequence(Object from) {
    if (from instanceof Sequence) {
      return (Sequence) from;
    } else if (from instanceof String) {
      return toSeq((String) from);
    }
    Sequence newSeq = new Sequence();
    newSeq.add(from);
    return newSeq;
  }

  private static Character castChar(Object from) {
    if (from instanceof Character) {
      return (Character) from;
    } else if (from instanceof Long) {
      return (char) ((Long) from).intValue();
    } else if (from instanceof Double) {
      return (char) ((Double) from).intValue();
    }

    throw new RuntimeException(String.format("unable to cast from %s to Character", from.getClass()));
  }

  private static Long castLong(Object from) {
    if (from instanceof Character) {
      return (long) (Character) from;
    } else if (from instanceof Long) {
      return (Long) from;
    } else if (from instanceof Double) {
      return ((Double) from).longValue();
    } else if (from instanceof String) {
      return Long.parseLong((String) from);
    }

    throw new RuntimeException(String.format("unable to cast from %s to Character", from.getClass()));
  }

  private static Double castDouble(Object from) {
    if (from instanceof Character) {
      return (double) (Character) from;
    } else if (from instanceof Long) {
      return ((Long) from).doubleValue();
    } else if (from instanceof Double) {
      return (Double) from;
    } else if (from instanceof String) {
      return Double.parseDouble((String) from);
    }

    throw new RuntimeException(String.format("unable to cast from %s to Character", from.getClass()));
  }

  private static String castString(Object from) {
    return from.toString();
  }

  private static Class<?> getLargerType(Class<?> a, Class<?> b) {
    return getRank(a) > getRank(b) ? a : b;
  }

  private static int getRank(Class<?> a) {
    if (a.equals(Character.class)) {
      return 0;
    } else if (a.equals(Long.class)) {
      return 1;
    } else if (a.equals(Double.class)) {
      return 2;
    } else if (a.equals(String.class)) {
      return 3;
    } else if (a.equals(Sequence.class)) {
      return 4;
    }

    throw new RuntimeException("Unable to decide rank for type: " + a.getName());
  }

  private static Sequence toSeq(String str) {
    Sequence seq = new Sequence();
    char[] charArr = str.toCharArray();
    for (char c : charArr) {
      seq.add(c);
    }
    return seq;
  }

  private static LengineUnit assertTrue(String message, Boolean value) {
    if (!value) {
      throw new RuntimeException("Failed to assert: " + message);
    }
    System.out.println("PASSED: " + message);
    return UNIT;
  }

  private static final LengineLambda2<Object, Object, Object> _ADD = (a, b) -> {
    if (a instanceof LengineMap && b instanceof LengineMapEntry) {
      return ((LengineMap) a).add((LengineMapEntry) b);
    }

    Class<?> largerType = getLargerType(a.getClass(), b.getClass());
    Object x = cast(a, largerType);
    Object y = cast(b, largerType);

    if (x instanceof Character) {
      return (Character) x + (Character) y;
    } else if (x instanceof Long) {
      return (Long) x + (Long) y;
    } else if (x instanceof Double) {
      return (Double) x + (Double) y;
    } else if (x instanceof String) {
      return x + (String) y;
    } else if (x instanceof Sequence) {
      Sequence newSeq = new Sequence();
      newSeq.append((Sequence) x);
      newSeq.append((Sequence) y);
      return newSeq;
    }

    throw new RuntimeException("Can't add");
  };
  private static final LengineLambda2<Object, Object, Object> _SUB = (a, b) -> {
    Class<?> largerType = getLargerType(a.getClass(), b.getClass());
    Object x = cast(a, largerType);
    Object y = cast(b, largerType);

    if (x instanceof Character) {
      return (Character) x - (Character) y;
    } else if (x instanceof Long) {
      return (Long) x - (Long) y;
    } else if (x instanceof Double) {
      return (Double) x - (Double) y;
    }

    throw new RuntimeException("Can't subtract");
  };
  public static final LengineLambda2<Object, Object, Object> _MULT = (a, b) -> {
    Class<?> largerType = getLargerType(a.getClass(), b.getClass());
    Object x = cast(a, largerType);
    Object y = cast(b, largerType);

    if (x instanceof Character) {
      return (Character) x * (Character) y;
    } else if (x instanceof Long) {
      return (Long) x * (Long) y;
    } else if (x instanceof Double) {
      return (Double) x * (Double) y;
    }

    throw new RuntimeException("Can't multiply");
  };
  public static final LengineLambda2<Object, Object, Object> _DIV = (a, b) -> {
    Class<?> largerType = getLargerType(a.getClass(), b.getClass());
    Object x = cast(a, largerType);
    Object y = cast(b, largerType);

    if (x instanceof Character) {
      return (Character) x / (Character) y;
    } else if (x instanceof Long) {
      return (Long) x / (Long) y;
    } else if (x instanceof Double) {
      return (Double) x / (Double) y;
    }

    throw new RuntimeException("Can't divide");
  };
  public static final LengineLambda2<Object, Object, Object> _REM = (a, b) -> {
    Class<?> largerType = getLargerType(a.getClass(), b.getClass());
    Object x = cast(a, largerType);
    Object y = cast(b, largerType);

    if (x instanceof Character) {
      return (Character) x % (Character) y;
    } else if (x instanceof Long) {
      return (Long) x % (Long) y;
    } else if (x instanceof Double) {
      return (Double) x % (Double) y;
    }

    throw new RuntimeException("Can't divide");
  };
  private static final LengineLambda1<Long, Object> _LEN = (obj) -> {
    if (obj instanceof CreateIterator) {
      return ((CreateIterator) obj).len();
    } else if (obj instanceof String) {
      return (long) ((String) obj).length();
    }

    throw new RuntimeException("unable to decide the size for " + obj);
  };
  private static final LengineLambda2<Sequence, Long, CreateIterator> _TAKE = (n, seq) -> {
    int i = 0;
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (i++ < n && it.hasNext()) {
      ret.add(it.next());
    }
    return ret;
  };
  private static final LengineLambda2<CreateIterator, Long, CreateIterator> _DROP = (n, seq) -> {
    int i = 0;
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (i++ < n && it.hasNext()) {
      it.next();
    }

    if (it instanceof LengineListIterator) {
      return ((LengineListIterator)it)._this();
    }
    it.forEachRemaining(ret::addObject);
    return ret;
  };

  private static <T> Boolean isInstanceOf(Class<T> cls, Object value) {
    return cls.isInstance(value);
  }

  private static final LengineLambda1<Object, CreateIterator> _HEAD = (seq) -> seq.iterator().peek();
  private static final LengineLambda1<CreateIterator, CreateIterator> _TAIL = (seq) -> _DROP.invoke(1L, seq);
  private static final LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> _TAKE_WHILE = (test, seq) -> {
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (it.hasNext()) {
      Object elem = it.next();
      if (!test.invoke(elem)) {
        return ret;
      }

      ret.add(elem);
    }

    return ret;
  };
  private static final LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> _DROP_WHILE = (test, seq) -> {
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (it.hasNext()) {
      Object elem = it.next();
      if (!test.invoke(elem)) {
        ret.add(elem);
        break;
      }
    }
    it.forEachRemaining(ret::add);

    return ret;
  };
  private static final LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> _FILTER = (test, seq) -> {
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (it.hasNext()) {
      Object elem = it.next();
      if (test.invoke(elem)) {
        ret.add(elem);
      }
    }

    return ret;
  };
  private static final LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> _SPLIT_AT = (test, seq) -> {
    Sequence ret = new Sequence();
    Sequence head = new Sequence();
    Sequence tail = new Sequence();
    LengineIterator it = seq.iterator();
    while (it.hasNext()) {
      Object elem = it.next();
      if (test.invoke(elem)) {
        break;
      }
      head.add(elem);

    }

    it.forEachRemaining(tail::add);
    ret.add(head);
    ret.add(tail);

    return ret;
  };
  private static final LengineLambda3<Object, CreateIterator, Object, LengineLambda2<Object, Object, Object>> _FOLD = (seq, acc, fn) -> {
    Object ret = acc;
    LengineIterator it = seq.iterator();
    while (it.hasNext()) {
      ret = fn.invoke(ret, it.next());
    }

    return ret;
  };
  private static final LengineLambda1<Sequence, Sequence> _FLATTEN = Sequence::flatten;
  private static final LengineLambda2<Boolean, Object, Object> _LESS_THAN = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) < 0);
  private static final LengineLambda2<Boolean, Object, Object> _LESS_EQUALS = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) <= 0);
  private static final LengineLambda2<Boolean, Object, Object> _GREATER_THAN = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) > 0);
  private static final LengineLambda2<Boolean, Object, Object> _GREATER_EQUALS = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) >= 0);
  private static final LengineLambda2<Boolean, Object, Object> _EQUALS = Objects::equals;
  private static final LengineLambda2<Boolean, Object, Object> _NOT_EQUALS = (a, b) -> !Objects.equals(a, b);
  private static final LengineLambda2<Boolean, Boolean, Boolean> _AND = (a, b) -> a && b;
  private static final LengineLambda2<Boolean, Boolean, Boolean> _OR = (a, b) -> a || b;
  private static final LengineLambda1<Boolean, Boolean> _NOT = (a) -> !a;
  private static final LengineLambda1<LengineUnit, Object> _PRINTLN = (str) -> {
    System.out.println(str);
    return UNIT;
  };
  private static final LengineLambda1<LengineUnit, Object> _PRINT = (str) -> {
    System.out.print(str);
    return UNIT;
  };
  private static final LengineLambda2<LengineUnit, String, CreateIterator> _PRINTF = (fmt, args) -> {
    final LinkedList<Object> items = new LinkedList<>();
    LengineIterator argsIt = args.iterator();
    while (argsIt.hasNext()) {
      items.add(argsIt.next());
    }

    System.out.printf(fmt, items.toArray());
    return UNIT;
  };
  private static final LengineLambda2<String, String, CreateIterator> _FORMAT = (fmt, args) -> {
    final LinkedList<Object> items = new LinkedList<>();
    LengineIterator argsIt = args.iterator();
    while (argsIt.hasNext()) {
      items.add(argsIt.next());
    }

    return String.format(fmt, items.toArray());
  };
  private static final LengineLambda2<RangeSequence, Long, Long> _RANGE = RangeSequence::createRange;
  private static final LengineLambda2<RangeSequence, Long, Long> _INCLUSIVE_RANGE = RangeSequence::createInclusiveRange;
  private static final LengineLambda2<LengineUnit, String, Boolean> _ASSERT_TRUE = Prelude::assertTrue;
  private static final LengineLambda2<LengineUnit, String, Boolean> _ASSERT_FALSE = (message, value) -> assertTrue(message, !value);
  private static final LengineLambda3<LengineUnit, String, Object, Object> _ASSERT_EQUALS = (message, a, b) -> assertTrue(message, _EQUALS.invoke(a, b));
  private static final LengineLambda3<LengineUnit, String, Object, Object> _ASSERT_NOT_EQUALS = (message, a, b) -> assertTrue(message, _NOT_EQUALS.invoke(a, b));
  private static final LengineLambda1<String, Object> _CAST_STR = Prelude::castString;
  private static final LengineLambda1<Long, Object> _CAST_INT = Prelude::castLong;
  private static final LengineLambda1<Double, Object> _CAST_DOUBLE = Prelude::castDouble;
  private static final LengineLambda1<Character, Object> _CAST_CHARACTER = Prelude::castChar;
  private static final LengineLambda1<Sequence, Object> _CAST_SEQUENCE = Prelude::castSequence;

  private static final LengineLambda1<Boolean, Object> _IS_BOOL = (obj) -> isInstanceOf(Boolean.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_CHAR = (obj) -> isInstanceOf(Character.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_INT = (obj) -> isInstanceOf(Long.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_DOUBLE = (obj) -> isInstanceOf(Double.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_STR = (obj) -> isInstanceOf(String.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_SEQUENCE = (obj) -> isInstanceOf(CreateIterator.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_OBJECT = (obj) -> isInstanceOf(LengineMap.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_CONS = (obj) -> isInstanceOf(Cons.class, obj);
  private static final LengineLambda1<Boolean, Object> _IS_NIL = (obj) -> isInstanceOf(Nil.class, obj);
  private static final LengineLambda0<Long> _NOW = System::currentTimeMillis;
  private static final LengineLambda1<CreateIterator, String> _OPEN_FILE = (path) -> {
    File file = new File(path);
    final long lengthOfFile = file.length();
    final BufferedReader reader;
    try {
      reader = new BufferedReader(new FileReader(file));
    } catch (FileNotFoundException e) {
      throw new RuntimeException(e);
    }

    return new CreateIterator() {
      @Override
      public LengineIterator iterator() {
        return new LengineIterator() {
          @Override
          public boolean hasNext() {
            try {
              return reader.ready();
            } catch (IOException e) {
              throw new RuntimeException(e);
            }
          }

          @Override
          public Object peek() {
            try {
              reader.mark(1);
              char read = (char) reader.read();
              reader.reset();
              return read;
            } catch (IOException e) {
              throw new RuntimeException(e);
            }
          }

          @Override
          public Object next() {
            try {
              return (char) reader.read();
            } catch (IOException e) {
              throw new RuntimeException(e);
            }
          }
        };
      }

      @Override
      public Long len() {
        return lengthOfFile;
      }
    };
  };
  private static final LengineLambda2<LengineList, Object, LengineList> _CONS = LengineList::cons;


  public static final LengineLambdaCommon ADD = _ADD;
  public static final LengineLambdaCommon SUB = _SUB;
  public static final LengineLambdaCommon MULT = _MULT;
  public static final LengineLambdaCommon DIV = _DIV;
  public static final LengineLambdaCommon REM = _REM;
  public static final LengineLambdaCommon LEN = _LEN;
  public static final LengineLambdaCommon TAKE = _TAKE;
  public static final LengineLambdaCommon DROP = _DROP;
  public static final LengineLambdaCommon HEAD = _HEAD;
  public static final LengineLambdaCommon TAIL = _TAIL;
  public static final LengineLambdaCommon TAKE_WHILE = _TAKE_WHILE;
  public static final LengineLambdaCommon DROP_WHILE = _DROP_WHILE;
  public static final LengineLambdaCommon FILTER = _FILTER;
  public static final LengineLambdaCommon SPLIT_AT = _SPLIT_AT;
  public static final LengineLambdaCommon FOLD = _FOLD;
  public static final LengineLambdaCommon FLATTEN = _FLATTEN;
  public static final LengineLambdaCommon LESS_THAN = _LESS_THAN;
  public static final LengineLambdaCommon LESS_EQUALS = _LESS_EQUALS;
  public static final LengineLambdaCommon GREATER_THAN = _GREATER_THAN;
  public static final LengineLambdaCommon GREATER_EQUALS = _GREATER_EQUALS;
  public static final LengineLambdaCommon EQUALS = _EQUALS;
  public static final LengineLambdaCommon NOT_EQUALS = _NOT_EQUALS;
  public static final LengineLambdaCommon AND = _AND;
  public static final LengineLambdaCommon OR = _OR;
  public static final LengineLambdaCommon NOT = _NOT;
  public static final LengineLambdaCommon PRINTLN = _PRINTLN;
  public static final LengineLambdaCommon PRINT = _PRINT;
  public static final LengineLambdaCommon PRINTF = _PRINTF;
  public static final LengineLambdaCommon FORMAT = _FORMAT;
  public static final LengineLambdaCommon RANGE = _RANGE;
  public static final LengineLambdaCommon INCLUSIVE_RANGE = _INCLUSIVE_RANGE;
  public static final LengineLambdaCommon ASSERT = _ASSERT_TRUE;
  public static final LengineLambdaCommon ASSERT_TRUE = _ASSERT_TRUE;
  public static final LengineLambdaCommon ASSERT_FALSE = _ASSERT_FALSE;
  public static final LengineLambdaCommon ASSERT_EQUALS = _ASSERT_EQUALS;
  public static final LengineLambdaCommon ASSERT_NOT_EQUALS = _ASSERT_NOT_EQUALS;
  public static final LengineLambdaCommon CAST_STR = _CAST_STR;
  public static final LengineLambdaCommon CAST_INT = _CAST_INT;
  public static final LengineLambdaCommon CAST_DOUBLE = _CAST_DOUBLE;
  public static final LengineLambdaCommon CAST_CHARACTER = _CAST_CHARACTER;
  public static final LengineLambdaCommon CAST_SEQUENCE = _CAST_SEQUENCE;
  public static final LengineLambdaCommon IS_BOOL = _IS_BOOL;
  public static final LengineLambdaCommon IS_CHAR = _IS_CHAR;
  public static final LengineLambdaCommon IS_INT = _IS_INT;
  public static final LengineLambdaCommon IS_DOUBLE = _IS_DOUBLE;
  public static final LengineLambdaCommon IS_STR = _IS_STR;
  public static final LengineLambdaCommon IS_SEQUENCE = _IS_SEQUENCE;
  public static final LengineLambdaCommon IS_OBJECT = _IS_OBJECT;
  public static final LengineLambdaCommon IS_CONS = _IS_CONS;
  public static final LengineLambdaCommon IS_NIL = _IS_NIL;
  public static final LengineLambdaCommon OPEN_FILE = _OPEN_FILE;
  public static final LengineLambdaCommon NOW = _NOW;
  public static final LengineLambdaCommon CONS = _CONS;
  public static final Object NIL = Nil.get();

  public static String readLine() throws IOException {
    return new BufferedReader(new InputStreamReader(System.in)).readLine();
  }

  public static String readEof() throws IOException {
    Reader sr = new InputStreamReader(System.in);
    StringBuilder ret = new StringBuilder();
    int ch;
    while ((ch = sr.read()) != 0xFFFF) {
      ret.append((char) ch);
    }

    return ret.toString();
  }

  public static String readFile(String fileName) throws IOException {
    return new String(Files.readAllBytes(Paths.get(fileName)), StandardCharsets.UTF_8);
  }

  public static FileSequence readFileSeq(String fileName) {
    return FileSequence.create(fileName);
  }

  private static Boolean compareFunction(Object a, Object b, BiPredicate<Comparable, Comparable> predicate) {
    Class<?> largerType = getLargerType(a.getClass(), b.getClass());

    Object aobj = cast(a, largerType);
    Object bobj = cast(b, largerType);

    if (!(aobj instanceof Comparable) || !(bobj instanceof Comparable)) {
      throw new RuntimeException("Unable to compare the given object: " + a + ", " + b);
    }

    return predicate.test((Comparable) a, (Comparable) b);
  }

  public static void loadClass(String clsName) {
    try {
      if (!alreadyLoadedClass.contains(clsName)) {
        Class<?> cls = ClassLoader.getSystemClassLoader().loadClass(clsName);
        Optional<Method> foundMethod = Arrays.stream(cls.getMethods())
            .filter(x -> x.getName().equals("main")).findFirst();
        if (!foundMethod.isPresent()) {
          throw new RuntimeException("Unable to find method main!");
        } else {
          Method mainMethod = foundMethod.get();
          mainMethod.invoke(null, new Object[]{new String[]{}});
        }
        alreadyLoadedClass.add(clsName);
      }

    } catch (ClassNotFoundException | InvocationTargetException | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
  }
}
