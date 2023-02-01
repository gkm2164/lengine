package lengine;

import java.io.BufferedReader;
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

import lengine.functions.LengineLambda1;
import lengine.functions.LengineLambda2;
import lengine.runtime.CreateIterator;
import lengine.runtime.FileSequence;
import lengine.runtime.LengineIterator;
import lengine.runtime.LengineMap;
import lengine.runtime.LengineMapEntry;
import lengine.runtime.LengineUnit;
import lengine.runtime.Sequence;

public class Prelude {
  private static final Set<String> alreadyLoadedClass = new HashSet<>();

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

  public static Object cast_str(Object from) {
    return castString(from);
  }

  public static Object cast_int(Object from) {
    return castLong(from);
  }

  public static Object cast_double(Object from) {
    return castDouble(from);
  }

  public static Object cast_char(Object from) {
    return castChar(from);
  }

  public static Object cast_seq(Object from) {
    return castSequence(from);
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


  public static final LengineLambda2<Object, Object, Object> ADD = (a, b) -> {
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

  public static final LengineLambda2<Object, Object, Object> SUB = (a, b) -> {
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

  public static final LengineLambda2<Object, Object, Object> MULT = (a, b) -> {
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


  public static final LengineLambda2<Object, Object, Object> DIV = (a, b) -> {
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

  public static LengineLambda1<Long, Object> LEN = (obj) -> {
    if (obj instanceof CreateIterator) {
      return ((CreateIterator) obj).len();
    } else if (obj instanceof String) {
      return (long) ((String) obj).length();
    }

    throw new RuntimeException("unable to decide the size for " + obj);
  };

  private static Sequence toSeq(String str) {
    Sequence seq = new Sequence();
    char[] charArr = str.toCharArray();
    for (char c : charArr) {
      seq.add(c);
    }
    return seq;
  }

  public static LengineLambda2<Sequence, Long, CreateIterator> TAKE = (n, seq) -> {
    int i = 0;
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (i++ < n && it.hasNext()) {
      ret.add(it.next());
    }
    return ret;
  };
  public static LengineLambda2<Sequence, Long, CreateIterator> DROP = (n, seq) -> {
    int i = 0;
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (i++ < n && it.hasNext()) {
      it.next();
    }
    it.forEachRemaining(ret::addObject);
    return ret;
  };
  public static LengineLambda1<Object, CreateIterator> HEAD = (seq) -> seq.iterator().next();
  public static LengineLambda1<Sequence, CreateIterator> TAIL = (seq)-> DROP.invoke(1L, seq);
  public static LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> TAKE_WHILE = (test, seq) -> {
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
  public static LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> DROP_WHILE = (test, seq) -> {
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

  public static LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> FILTER = (test, seq) -> {
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
  public static LengineLambda2<Sequence, LengineLambda1<Boolean, Object>, CreateIterator> SPLIT_AT = (test, seq) -> {
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
  public static LengineLambda1<Sequence, Sequence> FLATTEN = Sequence::flatten;

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

  public static LengineLambda2<Boolean, Object, Object> LESS_THAN = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) < 0);
  public static LengineLambda2<Boolean, Object, Object> LESS_EQUALS = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) <= 0);
  public static LengineLambda2<Boolean, Object, Object> GREATER_THAN = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) > 0);
  public static LengineLambda2<Boolean, Object, Object> GREATER_EQUALS = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) >= 0);

  public static LengineLambda2<Boolean, Object, Object> EQUALS = Objects::equals;
  public static LengineLambda2<Boolean, Object, Object> NOT_EQUAL = (a, b) -> !Objects.equals(a, b);
  public static LengineLambda2<Boolean, Boolean, Boolean> AND = (a, b) -> a && b;
  public static LengineLambda2<Boolean, Boolean, Boolean> OR = (a, b) -> a || b;
  public static LengineLambda1<Boolean, Boolean> NOT = (a) -> !a;
  private final static LengineUnit UNIT = LengineUnit.create();

  public static LengineLambda1<LengineUnit, Object> PRINTLN = (str) -> {
    System.out.println(str);
    return UNIT;
  };

  public static LengineLambda1<LengineUnit, Object> PRINT = (str) -> {
    System.out.print(str);
    return UNIT;
  };

  public static LengineLambda2<LengineUnit, String, CreateIterator> PRINTF = (fmt, args) -> {
    final LinkedList<Object> items = new LinkedList<>();
    LengineIterator argsIt = args.iterator();
    while (argsIt.hasNext()) {
      items.add(argsIt.next());
    }

    System.out.printf(fmt, items.toArray());
    return UNIT;
  };

  public static LengineLambda2<String, String, CreateIterator> FORMAT = (fmt, args) -> {
    final LinkedList<Object> items = new LinkedList<>();
    LengineIterator argsIt = args.iterator();
    while (argsIt.hasNext()) {
      items.add(argsIt.next());
    }

    return String.format(fmt, items.toArray());
  };

  public static void assertTrue(Object message, Boolean value) {
    if (!value) {
      throw new RuntimeException("Failed to assert: " + message);
    }
    System.out.println("PASSED: " + message);
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
