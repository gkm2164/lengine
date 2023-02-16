package lengine.runtime.exceptions;

import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineString;
import lengine.util.LengineMapKey;

public class LengineRuntimeException extends LengineException implements LengineObjectType {
    public LengineRuntimeException(String msg) {
        super(msg);
    }

    @Override
    public Object get(LengineMapKey key) {
        if (key.getKey().toString().equals("message")) {
            return LengineString.create(this.getMessage());
        }

        throw new LengineRuntimeException("unknown key: " + key);
    }
}
