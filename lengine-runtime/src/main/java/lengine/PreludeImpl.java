package lengine;

import lengine.concurrency.LengineChannel;
import lengine.concurrency.LengineFuture;
import lengine.functions.LengineLambda0;
import lengine.functions.LengineLambda1;
import lengine.functions.LengineLambda2;
import lengine.functions.LengineLambda3;
import lengine.https.HttpServerBuilder;
import lengine.runtime.ComplexNumber;
import lengine.runtime.CreateIterator;
import lengine.runtime.FileSequence;
import lengine.runtime.LengineIterator;
import lengine.runtime.LengineLazyValue;
import lengine.runtime.LengineObjectHasHelp;
import lengine.runtime.LengineSequenceIterator;
import lengine.runtime.LengineStreamIterator;
import lengine.runtime.LengineUnit;
import lengine.runtime.RangeSequence;
import lengine.runtime.RatioNumber;
import lengine.runtime.exceptions.LengineTypeMismatchException;
import lengine.sqls.DBConn;
import lengine.util.Cons;
import lengine.util.LengineList;
import lengine.util.LengineListIterator;
import lengine.util.LengineMap;
import lengine.util.LengineMapEntry;
import lengine.util.LengineMapKey;
import lengine.util.LengineSequence;
import lengine.util.LengineSet;
import lengine.util.LengineStream;
import lengine.util.Nil;
import lengine.util.StreamCons;
import lengine.util.StreamNil;
import lengine.util.UnresolvedStream;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.Objects;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiPredicate;

import static java.lang.String.format;

public class PreludeImpl {
    private final static LengineUnit UNIT = LengineUnit.create();

    public static final LengineLambda2<Object, Object, Object> _ADD = (a, b) -> {
        Class<?> largerType = getLargerType(a.getClass(), b.getClass());
        Object x = cast(a, largerType);
        Object y = cast(b, largerType);

        if (x instanceof Character) {
            return (Character) x + (Character) y;
        } else if (x instanceof Long) {
            return (Long) x + (Long) y;
        } else if (x instanceof RatioNumber) {
            return ((RatioNumber) x).add((RatioNumber) y);
        } else if (x instanceof Double) {
            return (Double) x + (Double) y;
        } else if (x instanceof ComplexNumber) {
            return ((ComplexNumber) x).add((ComplexNumber) y);
        } else if (x instanceof String) {
            return x + (String) y;
        }

        throw new RuntimeException("Can't add");
    };
    public static final LengineLambda2<Object, Object, Object> _SUB = (a, b) -> {
        Class<?> largerType = getLargerType(a.getClass(), b.getClass());
        Object x = cast(a, largerType);
        Object y = cast(b, largerType);

        if (x instanceof Character) {
            return (Character) x - (Character) y;
        } else if (x instanceof Long) {
            return (Long) x - (Long) y;
        } else if (x instanceof RatioNumber) {
            return ((RatioNumber) x).sub((RatioNumber) y);
        } else if (x instanceof Double) {
            return (Double) x - (Double) y;
        } else if (x instanceof ComplexNumber) {
            return ((ComplexNumber) x).sub((ComplexNumber) y);
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
        } else if (x instanceof RatioNumber) {
            return ((RatioNumber) x).mult((RatioNumber) y);
        } else if (x instanceof Double) {
            return (Double) x * (Double) y;
        } else if (x instanceof ComplexNumber) {
            return ((ComplexNumber) x).mult((ComplexNumber) y);
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
        } else if (x instanceof RatioNumber) {
            return ((RatioNumber) x).div((RatioNumber) y);
        } else if (x instanceof Double) {
            return (Double) x / (Double) y;
        } else if (x instanceof ComplexNumber) {
            return ((ComplexNumber) x).div((ComplexNumber) y);
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

    public static final LengineLambda1<RatioNumber, RatioNumber> _NORM = RatioNumber::normalize;
    public static final LengineLambda1<Long, Object> _LEN = (obj) -> {
        if (obj instanceof CreateIterator) {
            return ((CreateIterator) obj).len();
        } else if (obj instanceof String) {
            return (long) ((String) obj).length();
        }

        throw new RuntimeException("unable to decide the size for " + obj);
    };
    public static final LengineLambda2<CreateIterator, Long, CreateIterator> _TAKE = (n, seq) -> {
        LengineIterator it = seq.iterator();
        AtomicReference<LengineList> ret = new AtomicReference<>(Nil.get());
        it.forEachN(elem -> {
            LengineList _this = ret.get();
            ret.set(_this.add(elem));
        }, n);

        return ret.get();
    };
    public static final LengineLambda2<CreateIterator, Long, CreateIterator> _DROP = (n, seq) -> {
        LengineIterator it = seq.iterator();
        it.forEachN(x -> {
        }, n);
        if (it instanceof LengineListIterator) {
            return ((LengineListIterator) it)._this();
        } else if (it instanceof LengineSequenceIterator) {
            return LengineSequence.create(it);
        } else if (it instanceof LengineStreamIterator) {
            return ((LengineStreamIterator) it)._this();
        }

        throw new RuntimeException("Unsupported");
    };

    public static <T> Boolean isInstanceOf(Class<T> cls, Object value) {
        return cls.isInstance(value);
    }

    public static final LengineLambda1<Object, CreateIterator> _HEAD = CreateIterator::head;
    public static final LengineLambda1<CreateIterator, CreateIterator> _TAIL = (seq) -> _DROP.invoke(1L, seq);
    public static final LengineLambda3<Object, CreateIterator, Object, LengineLambda2<Object, Object, Object>> _FOLD = (seq, acc, fn) -> {
        AtomicReference<Object> ret = new AtomicReference<>(acc);
        seq.iterator().forEachRemaining(elem -> ret.set(fn.invoke(ret.get(), elem)));
        return ret.get();
    };
    public static final LengineLambda2<Boolean, Object, Object> _LESS_THAN = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) < 0);
    public static final LengineLambda2<Boolean, Object, Object> _LESS_EQUALS = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) <= 0);
    public static final LengineLambda2<Boolean, Object, Object> _GREATER_THAN = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) > 0);
    public static final LengineLambda2<Boolean, Object, Object> _GREATER_EQUALS = (a, b) -> compareFunction(a, b, (x, y) -> x.compareTo(y) >= 0);
    public static final LengineLambda2<Boolean, Object, Object> _EQUALS = Objects::equals;
    public static final LengineLambda2<Boolean, Object, Object> _NOT_EQUALS = (a, b) -> !Objects.equals(a, b);
    public static final LengineLambda2<Boolean, Boolean, Boolean> _AND = (a, b) -> a && b;
    public static final LengineLambda2<Boolean, Boolean, Boolean> _OR = (a, b) -> a || b;
    public static final LengineLambda1<Boolean, Boolean> _NOT = (a) -> !a;
    public static final LengineLambda1<LengineUnit, Object> _PRINTLN = (str) -> {
        System.out.println(str);
        return UNIT;
    };
    public static final LengineLambda1<LengineUnit, Object> _PRINT = (str) -> {
        System.out.print(str);
        return UNIT;
    };
    public static final LengineLambda2<LengineUnit, String, CreateIterator> _PRINTF = (fmt, args) -> {
        final LinkedList<Object> items = new LinkedList<>();
        LengineIterator argsIt = args.iterator();
        while (argsIt.hasNext()) {
            items.add(argsIt.next());
        }

        System.out.printf(fmt, items.toArray());
        return UNIT;
    };
    public static final LengineLambda2<String, String, CreateIterator> _FORMAT = (fmt, args) -> {
        final LinkedList<Object> items = new LinkedList<>();
        LengineIterator argsIt = args.iterator();
        while (argsIt.hasNext()) {
            items.add(argsIt.next());
        }

        return format(fmt, items.toArray());
    };
    public static final LengineLambda2<RangeSequence, Long, Long> _RANGE = RangeSequence::createRange;
    public static final LengineLambda2<RangeSequence, Long, Long> _INCLUSIVE_RANGE = RangeSequence::createInclusiveRange;
    public static final LengineLambda2<LengineUnit, String, Boolean> _ASSERT_TRUE = PreludeImpl::assertTrue;
    public static final LengineLambda2<LengineUnit, String, Boolean> _ASSERT_FALSE = (message, value) -> assertTrue(message, !value);
    public static final LengineLambda3<LengineUnit, String, Object, Object> _ASSERT_EQUALS = (message, a, b) -> {
        if (!_EQUALS.invoke(a, b)) {
            throw new RuntimeException(a.toString() + " /= " + b.toString());
        }

        return UNIT;
    };
    public static final LengineLambda3<LengineUnit, String, Object, Object> _ASSERT_NOT_EQUALS = (message, a, b) -> assertTrue(message, _NOT_EQUALS.invoke(a, b));
    public static final LengineLambda1<String, Object> _CAST_STR = PreludeImpl::castString;
    public static final LengineLambda1<Long, Object> _CAST_INT = PreludeImpl::castLong;
    public static final LengineLambda1<Double, Object> _CAST_DOUBLE = PreludeImpl::castDouble;
    public static final LengineLambda1<Character, Object> _CAST_CHARACTER = PreludeImpl::castChar;
    public static final LengineLambda1<LengineList, Object> _CAST_LIST = PreludeImpl::castList;
    public static final LengineLambda1<LengineSequence, Object> _CAST_SEQ = PreludeImpl::castSeq;
    public static final LengineLambda1<LengineSet, Object> _CAST_SET = PreludeImpl::castSet;
    public static final LengineLambda1<Boolean, Object> _IS_BOOL = (obj) -> isInstanceOf(Boolean.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_CHAR = (obj) -> isInstanceOf(Character.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_INT = (obj) -> isInstanceOf(Long.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_DOUBLE = (obj) -> isInstanceOf(Double.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_STR = (obj) -> isInstanceOf(String.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_LIST = (obj) -> isInstanceOf(LengineList.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_SEQ = (obj) -> isInstanceOf(LengineSequence.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_OBJECT = (obj) -> isInstanceOf(LengineMap.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_CONS = (obj) -> isInstanceOf(Cons.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_NIL = (obj) -> isInstanceOf(Nil.class, obj) || _LEN.invoke(obj) == 0;
    public static final LengineLambda1<Boolean, Object> _IS_STREAM_NIL = (obj) -> isInstanceOf(StreamNil.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_STREAM_CONS = (obj) -> isInstanceOf(StreamCons.class, obj);
    public static final LengineLambda1<Boolean, Object> _IS_SET = (obj) -> isInstanceOf(LengineSet.class, obj);

    public static final LengineLambda2<Boolean, Object, Object> _DOES_HAVE = (set, obj) -> {
        if (set instanceof LengineSet) {
            return ((LengineSet) set).contains(obj);
        } else if (set instanceof LengineMap && obj instanceof LengineMapKey) {
            return ((LengineMap) set).contains((LengineMapKey) obj);
        }

        return false;
    };
    public static final LengineLambda0<Long> _NOW = System::currentTimeMillis;
    public static final LengineLambda1<CreateIterator, String> _OPEN_FILE = (path) -> {
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

            @Override
            public Object head() {
                throw new RuntimeException("unsupported operation");
            }

            @Override
            public CreateIterator tail() {
                return null;
            }
        };
    };
    public static final LengineLambda2<LengineList, Object, LengineList> _CONS = LengineList::cons;
    public static final LengineLambda2<LengineStream, Object, Object> _STREAM_CONS = (value, tail) -> {
        if (tail instanceof LengineLazyValue) {
            return LengineStream.cons(value, UnresolvedStream.create((LengineLazyValue) tail));
        } else if (tail instanceof StreamCons) {
            return LengineStream.cons(value, (StreamCons) tail);
        } else if (tail instanceof StreamNil) {
            return LengineStream.cons(value, (StreamNil) tail);
        }

        throw new RuntimeException("Unable to concatenate to stream:" + tail);
    };
    public static final LengineLambda1<LengineMapKey, String> _KEY = LengineMapKey::create;
    public static final LengineLambda1<LengineSet, LengineMap> _KEYS = LengineMap::keys;
    public static final LengineLambda2<LengineMapEntry, LengineMapKey, Object> _ENTRY = LengineMapEntry::create;
    public static final LengineLambda1<LengineSequence, LengineMap> _ENTRIES = LengineMap::entries;
    public static final LengineLambda1<String, LengineMapKey> _GET = LengineMapKey::getKey;

    public static final LengineLambda0<String> _READ_LINE = () -> new Scanner(System.in).next("\n");

    public static final LengineLambda0<String> _READ_EOF = () -> {
        Reader sr = new InputStreamReader(System.in);
        StringBuilder ret = new StringBuilder();
        int ch;
        while (true) {
            try {
                if ((ch = sr.read()) == 0xFFFF) break;
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            ret.append((char) ch);
        }

        return ret.toString();
    };


    public static final LengineLambda1<String, String> _READ_FILE = (fileName) -> {
        try {
            return new String(Files.readAllBytes(Paths.get(fileName)), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    };

    public static final LengineLambda1<FileSequence, String> _READ_FILE_SEQ = FileSequence::create;

    public static final LengineLambda2<CreateIterator, CreateIterator, Object> _APPEND_ITEM = (coll, elem) -> {
        if (coll instanceof LengineList) {
            LengineList head = (LengineList) coll;
            return head.add(elem);
        } else if (coll instanceof LengineSequence) {
            return ((LengineSequence) coll).add(elem);
        } else if (coll instanceof LengineMap) {
            return ((LengineMap) coll).putEntry((LengineMapEntry) elem);
        } else if (coll instanceof LengineSet) {
            return ((LengineSet) coll).add(elem);
        }

        throw new RuntimeException("currently not supporting the operation");
    };

    public static final LengineLambda2<Object, Object, Object> _MERGE = (xs, ys) -> {
        if (xs instanceof String) {
            if (ys instanceof String) {
                return xs + ((String) ys);
            }

            throw new RuntimeException("Unable to determine the 2nd parameter to be String");
        } else if (xs instanceof LengineList) {
            if (ys instanceof CreateIterator) {
                return ((LengineList) xs).append((CreateIterator) ys);
            }
        } else if (xs instanceof LengineSequence) {
            if (ys instanceof CreateIterator) {
                return ((LengineSequence) xs).append((CreateIterator) ys);
            }
        } else if (xs instanceof LengineSet) {
            if (ys instanceof LengineSet) {
                return ((LengineSet) xs).append((LengineSet) ys);
            } else if (ys instanceof CreateIterator) {
                return ((LengineSet) xs).append(LengineSet.create((CreateIterator) ys));
            }
        }

        throw new RuntimeException("merge operation not supported.");
    };
    public static final LengineLambda1<LengineFuture, LengineLambda0<?>> _ASYNC = LengineFuture::new;
    public static final LengineLambda1<Object, LengineFuture> _AWAIT = LengineFuture::await;
    public static final LengineLambda1<LengineUnit, Long> _WAIT = (milliseconds) -> {
        try {
            Thread.sleep(milliseconds);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return UNIT;
    };
    public static final LengineLambda0<LengineChannel> _CHANNEL = LengineChannel::create;

    public static final LengineLambda2<LengineUnit, LengineChannel, Object> _SEND = (channel, msg) -> {
        try {
            channel.sendMessage(msg);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        return UNIT;
    };
    public static final LengineLambda1<Object, LengineChannel> _RECEIVE = (channel) -> {
        try {
            return channel.receiveMessage();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    };
    public static final LengineLambda1<Object, LengineChannel> _CLOSE = (channel) -> {
        try {
            channel.close();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        return UNIT;
    };

    static final LengineLambda1<LengineSequence, LengineObjectHasHelp> _HELP = LengineObjectHasHelp::help;
    static final LengineLambda2<String, LengineObjectHasHelp, LengineMapKey> _HELP_KEYWORD = LengineObjectHasHelp::help;
    static final LengineLambda1<LengineLambda0<LengineUnit>, LengineMap> _LISTEN = HttpServerBuilder::listen;
    static final LengineLambda3<LengineMap, String, String, String> _DB_CONN = DBConn::connect;



    public static Object cast(Object from, Class<?> to) {
        if (to.equals(Character.class)) {
            return castChar(from);
        } else if (to.equals(Long.class)) {
            return castLong(from);
        } else if (to.equals(RatioNumber.class)) {
            return castRatioNumber(from);
        } else if (to.equals(Double.class)) {
            return castDouble(from);
        } else if (to.equals(ComplexNumber.class)) {
            return castComplexNumber(from);
        } else if (to.equals(String.class)) {
            return castString(from);
        }

        throw new LengineTypeMismatchException(from, to);
    }

    private static Character castChar(Object from) {
        if (from instanceof Character) {
            return (Character) from;
        } else if (from instanceof Long) {
            return (char) ((Long) from).intValue();
        } else if (from instanceof Double) {
            return (char) ((Double) from).intValue();
        }

        throw new LengineTypeMismatchException(from, Character.class);
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

        throw new LengineTypeMismatchException(from, Long.class);
    }

    private static Double castDouble(Object from) {
        if (from instanceof Character) {
            return (double) (Character) from;
        } else if (from instanceof Long) {
            return ((Long) from).doubleValue();
        } else if (from instanceof RatioNumber) {
            return ((RatioNumber) from).doubleValue();
        } else if (from instanceof Double) {
            return (Double) from;
        } else if (from instanceof String) {
            return Double.parseDouble((String) from);
        }

        throw new LengineTypeMismatchException(from, Double.class);
    }

    private static RatioNumber castRatioNumber(Object from) {
        if (from instanceof Character) {
            return RatioNumber.create((Character) from, 1L);
        } else if (from instanceof Long) {
            return RatioNumber.create((Long) from, 1L);
        } else if (from instanceof RatioNumber) {
            return (RatioNumber) from;
        }

        throw new LengineTypeMismatchException(from, RatioNumber.class);
    }

    private static ComplexNumber castComplexNumber(Object from) {
        if (from instanceof Character) {
            return ComplexNumber.create((long) (Character) from, 1L);
        } else if (from instanceof Long) {
            return ComplexNumber.create((Long) from, 1L);
        } else if (from instanceof RatioNumber) {
            return ComplexNumber.create((RatioNumber) from, 1L);
        } else if (from instanceof ComplexNumber) {
            return (ComplexNumber) from;
        }

        throw new LengineTypeMismatchException(from, ComplexNumber.class);
    }

    private static String castString(Object from) {
        if (from instanceof LengineList) {
            return from.toString();
        } else if (from instanceof LengineSequence) {
            return from.toString();
        }

        return from.toString();
    }

    private static LengineList castList(Object o) {
        if (o instanceof String) {
            return LengineList.create(o.toString());
        } else if (o instanceof LengineList) {
            return (LengineList) o;
        }

        throw new LengineTypeMismatchException(o, LengineList.class);
    }

    private static LengineSequence castSeq(Object o) {
        if (o instanceof String) {
            return LengineSequence.create(o.toString());
        } else if (o instanceof LengineSequence) {
            return (LengineSequence) o;
        } else if (o instanceof CreateIterator) {
            return LengineSequence.create((CreateIterator) o);
        }

        throw new LengineTypeMismatchException(o, LengineSequence.class);
    }

    private static LengineSet castSet(Object o) {
        if (o instanceof String) {
            return LengineSet.create(o.toString());
        } else if (o instanceof LengineSet) {
            return (LengineSet) o;
        } else if (o instanceof CreateIterator) {
            return LengineSet.create((CreateIterator) o);
        }

        throw new LengineTypeMismatchException(o, LengineSet.class);
    }

    private static Class<?> getLargerType(Class<?> a, Class<?> b) {
        return getRank(a) > getRank(b) ? a : b;
    }

    private static int getRank(Class<?> a) {
        if (a.equals(Character.class)) {
            return 0;
        } else if (a.equals(Long.class)) {
            return 1;
        } else if (a.equals(RatioNumber.class)) {
            return 2;
        } else if (a.equals(Double.class)) {
            return 3;
        } else if (a.equals(ComplexNumber.class)) {
            return 4;
        } else if (a.equals(String.class)) {
            return 5;
        } else if (a.equals(CreateIterator.class)) {
            return 6;
        }

        throw new RuntimeException("Unable to decide rank for type: " + a.getName());
    }

    private static LengineUnit assertTrue(String message, Boolean value) {
        if (!value) {
            throw new RuntimeException("Failed to assert: " + message);
        }
        System.out.println("PASSED: " + message);
        return UNIT;
    }


    private static Boolean compareFunction(Object a, Object b, BiPredicate<Comparable<Object>, Comparable<Object>> predicate) {
        Class<?> largerType = getLargerType(a.getClass(), b.getClass());

        Object _a = cast(a, largerType);
        Object _b = cast(b, largerType);

        if (!(_a instanceof Comparable)) {
            throw new RuntimeException("Unable to compare the given object: " + a + ", " + b);
        }

        if (!(_b instanceof Comparable)) {
            throw new RuntimeException("Unable to compare the given object: " + a + ", " + b);
        }

        Comparable<Object> _aComp = (Comparable<Object>) _a;
        Comparable<Object> _bComp = (Comparable<Object>) _b;

        return predicate.test(_aComp, _bComp);
    }
}
