package lengine.util;

import lengine.runtime.CreateIterator;

public interface Wrap<T extends CreateIterator> {
    T WRAP();
}
