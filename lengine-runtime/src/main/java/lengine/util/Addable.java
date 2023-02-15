package lengine.util;

import lengine.runtime.LengineIterable;

public interface Addable<T extends LengineIterable> {
    T ADD(Object item);
}
