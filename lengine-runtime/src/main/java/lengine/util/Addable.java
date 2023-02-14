package lengine.util;

import lengine.runtime.CreateIterator;

public interface Addable<T extends CreateIterator> {
    T ADD(Object item);
}
