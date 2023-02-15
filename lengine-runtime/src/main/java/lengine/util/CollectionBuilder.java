package lengine.util;

import lengine.runtime.LengineIterable;

public interface CollectionBuilder<T extends LengineIterable> {
    void ADD(Object elem);
    T BUILD();
}
