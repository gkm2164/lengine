package lengine.util;

import lengine.runtime.CreateIterator;

public interface Nillable<T extends CreateIterator> {
    T NIL();
}
