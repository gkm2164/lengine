package lengine.runtime.exceptions;

import lengine.collections.traits.LengineObjectType;

public class LengineRuntimeException extends LengineException implements LengineObjectType {
    public LengineRuntimeException(String msg) {
        super(msg);
    }
}
