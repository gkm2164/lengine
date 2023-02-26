package lengine.runtime.exceptions;

import lengine.collections.traits.LengineObjectType;
import lengine.runtime.LengineString;
import lengine.collections.LengineMapKey;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

public class LengineException extends RuntimeException implements LengineObjectType {
    private static final Map<Class<? extends Throwable>, String> remappedException = new HashMap<>();

    static {
        remappedException.put(ArithmeticException.class, "divide-by-zero(%s)");
        remappedException.put(LengineRuntimeException.class, "runtime-error(%s)");
        remappedException.put(LengineTypeMismatchException.class, "type-mismatch(%s)");
    }

    public LengineException(Throwable e, String msg) {
        super(msg, e);
    }

    public LengineException(String msg) {
        super(msg);
    }

    public static LengineException CONVERT(Exception e) {
        return new LengineException(e, e.getMessage());
    }

    @Override
    public Object get(LengineMapKey key) {
        switch (key.getKey().toString()) {
            case "message":
                return LengineString.create(this.getMessage());
            case "type":
                String remappedName = remappedException.getOrDefault(this.getCause().getClass(),
                        "unclassified-exception(%s)");
                return LengineString.create(String.format(remappedName, this.getCause().getClass().getName()));
            case "stack-trace":
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                this.getCause().printStackTrace(pw);
                return LengineString.create(sw.toString());
            default:
                throw new RuntimeException("Unknown message");
        }
    }
}
