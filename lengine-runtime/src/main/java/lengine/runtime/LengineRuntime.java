package lengine.runtime;

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
        }

        throw new RuntimeException("unable to cast");
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
        }

        throw new RuntimeException("Unable to decide rank for type: " + a.getName());
    }
}