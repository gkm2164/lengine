package lengine;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiPredicate;

import lengine.functions.LengineLambda1;
import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;
import lengine.runtime.LengineMap;
import lengine.runtime.LengineMapEntry;
import lengine.runtime.Sequence;

public class Prelude {
  private static final Set<String> alreadyLoadedClass = new HashSet<>();

  public static Object cast(Object from, Class<?> to) {
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
    return cast(from, String.class);
  }

  public static Object cast_int(Object from) {
    return cast(from, Long.class);
  }

  public static Object cast_double(Object from) {
    return cast(from, Double.class);
  }

  public static Object cast_char(Object from) {
    return cast(from, Character.class);
  }

  public static Object cast_seq(Object from) {
    return cast(from, Sequence.class);
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

  public static Object add(Object a, Object b) {
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
  }

  public static Object sub(Object a, Object b) {
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
  }

  public static Object mult(Object a, Object b) {
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
  }

  public static Object div(Object a, Object b) {
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

  public static Object len(Object seq) {
    if (seq instanceof CreateIterator) {
      return ((CreateIterator) seq).len();
    } else if (seq instanceof String) {
      return (long)((String) seq).length();
    }

    throw new RuntimeException("unable to decide the size for " + seq);
  }

  private static Sequence toSeq(String str) {
    Sequence seq = new Sequence();
    char[] charArr = str.toCharArray();
    for (char c : charArr) {
      seq.add(c);
    }
    return seq;
  }

  public static Object head(CreateIterator seq) {
    return seq.iterator().next();
  }

  public static Object tail(CreateIterator seq) {
    return drop(1L, seq);
  }

  public static Sequence take(Long n, CreateIterator seq) {
    int i = 0;
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (i++ < n && it.hasNext()) {
      ret.add(it.next());
    }
    return ret;
  }

  public static Sequence drop(Long n, CreateIterator seq) {
    int i = 0;
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (i++ < n && it.hasNext()) {
      it.next();
    }
    it.forEachRemaining(ret::addObject);
    return ret;
  }

  public static Sequence takeWhile(LengineLambda1<Boolean, Object> test, CreateIterator seq) {
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
  }

  public static Sequence dropWhile(LengineLambda1<Boolean, Object> test, CreateIterator seq) {
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
  }

  public static Sequence filter(LengineLambda1<Boolean, Object> test, CreateIterator seq) {
    Sequence ret = new Sequence();
    LengineIterator it = seq.iterator();
    while (it.hasNext()) {
      Object elem = it.next();
      if (test.invoke(elem)) {
        ret.add(elem);
      }
    }

    return ret;
  }

  public static Sequence splitAt(LengineLambda1<Boolean, Object> test, CreateIterator seq) {
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
  }

  public static Object flatten(Sequence seq) {
    return seq.flatten();
  }

  public static String readLine() throws IOException {
    return new BufferedReader(new InputStreamReader(System.in)).readLine();
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

  public static Boolean lt(Object a, Object b) {
    return compareFunction(a, b, (x, y) -> x.compareTo(y) < 0);
  }

  public static Boolean le(Object a, Object b) {
    return compareFunction(a, b, (x, y) -> x.compareTo(y) <= 0);
  }

  public static Boolean gt(Object a, Object b) {
    return compareFunction(a, b, (x, y) -> x.compareTo(y) > 0);
  }

  public static Boolean ge(Object a, Object b) {
    return compareFunction(a, b, (x, y) -> x.compareTo(y) >= 0);
  }

  public static Boolean eq(Object a, Object b) {
    return Objects.equals(a, b);
  }

  public static Boolean neq(Object a, Object b) {
    return !Objects.equals(a, b);
  }

  public static Boolean and(Object a, Object b) {
    if (!(a instanceof Boolean)) {
      throw new RuntimeException("first parameter is not boolean");
    }
    if (!(b instanceof Boolean)) {
      throw new RuntimeException("second parameter is not boolean");
    }

    return ((Boolean) a) && ((Boolean) b);
  }

  public static Boolean or(Object a, Object b) {
    if (!(a instanceof Boolean)) {
      throw new RuntimeException("first parameter is not boolean");
    }
    if (!(b instanceof Boolean)) {
      throw new RuntimeException("second parameter is not boolean");
    }

    return ((Boolean) a) || ((Boolean) b);
  }

  public static Boolean not(Object a) {
    if (!(a instanceof Boolean)) {
      throw new RuntimeException("first parameter is not boolean");
    }

    return !((Boolean) a);
  }

  public static void assertTrue(Object message, Object value) {
    if (!(Boolean)value) {
      throw new RuntimeException("Failed to assert: " + message);
    }
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