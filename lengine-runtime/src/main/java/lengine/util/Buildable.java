package lengine.util;

import lengine.runtime.LengineIterable;

public interface Buildable<T extends LengineIterable, U extends CollectionBuilder<T>> {
    U BUILDER();
}
