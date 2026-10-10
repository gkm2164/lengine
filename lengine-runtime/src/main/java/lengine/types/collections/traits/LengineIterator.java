package lengine.types.collections.traits;

import java.util.function.Consumer;

public interface LengineIterator {
  boolean hasNext();
  Object next();

  default void forEachN(Consumer<Object> o, long n) {
    // Check the count first: hasNext() may force a lazy stream tail.
    for (long i = 0; i < n && hasNext(); i++) {
      o.accept(next());
    }
  }

  default void forEachRemaining(Consumer<Object> o) {
    while (hasNext()) {
      o.accept(next());
    }
  }
}
