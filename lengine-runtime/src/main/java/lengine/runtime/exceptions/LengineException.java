package lengine.runtime.exceptions;

import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineString;
import lengine.util.LengineMapKey;

import java.io.PrintWriter;
import java.io.StringWriter;

public class LengineException extends RuntimeException implements LengineObjectType {
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
                return LengineString.create(this.getCause().toString());
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
