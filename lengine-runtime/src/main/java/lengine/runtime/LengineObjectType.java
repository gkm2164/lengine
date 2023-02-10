package lengine.runtime;

import lengine.util.LengineMapKey;

public interface LengineObjectType {
    Object get(LengineMapKey key);
}
