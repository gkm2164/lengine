package lengine.util;

import lengine.runtime.LengineIterable;

public interface Wrap<T extends LengineIterable> {
    T WRAP();
}
