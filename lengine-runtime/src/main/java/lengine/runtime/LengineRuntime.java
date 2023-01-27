package lengine.runtime;

import scala.Char;

public class LengineRuntime {

    public static Object cast(Object from, Class to) {
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

    private static Sequence castSequence(Object from) {
        if (from instanceof Sequence) {
            return (Sequence) from;
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
        }

        throw new RuntimeException(String.format("unable to cast from %s to Character", from.getClass()));
    }

    private static Double castDouble(Object from) {
        if (from instanceof Character) {
            return (double) (Character) from;
        } else if (from instanceof Long) {
            return ((Long)from).doubleValue();
        } else if (from instanceof Double) {
            return (Double)from;
        }

        throw new RuntimeException(String.format("unable to cast from %s to Character", from.getClass()));
    }

    private static String castString(Object from) {
        return from.toString();
    }

    public static Object add(Object a, Object b) {
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

    public static Object take(Long n, Sequence seq) {
        return seq.take(n.intValue());
    }

    public static Object drop(Long n, Sequence seq) {
        return seq.drop(n.intValue());
    }

    public static Object flatten(Sequence seq) {
        return seq.flatten();
    }
}