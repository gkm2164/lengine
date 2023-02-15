package lengine.util;

import lengine.runtime.CreateIterator;

public interface CollectionBuilder<T extends CreateIterator> {
    CollectionBuilder<T> ADD(Object elem);
    T BUILD();
}
