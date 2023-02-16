package lengine.runtime.exceptions;

import lengine.runtime.LengineObjectType;
import lengine.runtime.LengineString;
import lengine.util.LengineMapKey;

public class LengineRuntimeException extends LengineException implements LengineObjectType {
    public LengineRuntimeException(String msg) {
        super(msg);
    }
}
