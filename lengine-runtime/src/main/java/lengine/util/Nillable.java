package lengine.util;

import lengine.runtime.LengineIterable;

public interface Nillable<T extends LengineIterable> {
    T NIL();
}
