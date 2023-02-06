package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.util.LinkedList;
import java.util.Objects;
import java.util.Set;

/**
 * Accessed with `seq`
 */
public abstract class LengineSequence implements CreateIterator {

  public static LengineSequence create(String str) {
    return new LeafSequence(str);
  }

  public static LengineSequence create(CreateIterator o) {
    LinkedList<Object> thisList = new LinkedList<>();
    o.iterator().forEachRemaining(thisList::add);
    return new LeafSequence(thisList);
  }

  private static LengineSequence create(LinkedList<Object> origin) {
    return new LeafSequence(origin);
  }

  public static LengineSequence create(LengineIterator it) {
    LinkedList<Object> obj = new LinkedList<>();
    it.forEachRemaining(obj::add);
    return create(obj);
  }

  public static LengineSequence create(Set<?> set) {
    LinkedList<Object> obj = new LinkedList<>();
    set.iterator().forEachRemaining(obj::add);
    return create(obj);
  }

  @Override
  public abstract LengineIterator iterator();

  @Override
  public abstract Long len();

  public abstract String printable(boolean isFirst);

  public LengineSequence add(Object o) {
    LinkedList<Object> newList = new LinkedList<>();
    newList.add(o);

    return append(new LeafSequence(newList));
  }

  public abstract LengineSequence append(LengineSequence seq);
  @Override
  public String toString() {
    return String.format("(seq %s)", printable(true));
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }

    if (!(obj instanceof LengineSequence)) {
      return false;
    }

    LengineIterator thisIt = iterator();
    LengineIterator thatIt = ((LengineSequence)obj).iterator();

    while(thisIt.hasNext() && thatIt.hasNext()) {
      Object _this = thisIt.next();
      Object _that = thatIt.next();

      if (!Objects.equals(_this, _that)) {
        return false;
      }
    }

    return !thisIt.hasNext() && !thatIt.hasNext();
  }

  public abstract LengineSequence take(long n);

  public abstract LengineSequence drop(long n);

  public abstract int hashCode();
}
