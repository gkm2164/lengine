package lengine.util;

import lengine.runtime.CreateIterator;

public interface Buildable<T extends CreateIterator, U extends CollectionBuilder<T>> {
    U BUILDER();
}
