package lengine.runtime;

import java.util.function.Consumer;

public interface LengineIterator {
  boolean hasNext();
  Object next();

  default void forEachRemaining(Consumer<Object> o) {
    while (hasNext()) {
      o.accept(next());
    }
  }
}
