package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;
import lengine.runtime.LengineLazyValue;
import lengine.runtime.LengineStreamIterator;

public abstract class LengineStream implements CreateIterator {
  public static LengineStream cons(Object o, LengineStream lengineStream) {
    return new StreamCons(o, lengineStream);
  }

  @Override
  public LengineIterator iterator() {
    return new LengineStreamIterator(this);
  }

  public static LengineStream create(Object obj) {
    if (obj instanceof LengineLazyValue) {
      return new UnresolvedStream((LengineLazyValue) obj);
    }

    return cons(obj, StreamNil.get());
  }
}
