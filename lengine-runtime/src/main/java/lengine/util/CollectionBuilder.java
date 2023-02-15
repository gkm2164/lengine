package lengine.util;

import lengine.runtime.CreateIterator;

public interface CollectionBuilder<T extends CreateIterator> {
    void ADD(Object elem);
    T BUILD();
}
