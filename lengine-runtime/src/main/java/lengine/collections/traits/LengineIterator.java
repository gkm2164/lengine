package lengine.collections.traits;

import java.util.function.Consumer;

public interface LengineIterator {
  boolean hasNext();
  Object next();

  default void forEachN(Consumer<Object> o, long n) {
    int i = 0;
    while (hasNext() && i++ < n) {
      o.accept(next());
    }
  }

  default void forEachRemaining(Consumer<Object> o) {
    while (hasNext()) {
      o.accept(next());
    }
  }
}
