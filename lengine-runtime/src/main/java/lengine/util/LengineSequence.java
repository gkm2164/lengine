package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.util.LinkedList;
import java.util.stream.Collectors;

/**
 * Accessed with `seq`
 */
public class LengineSequence implements CreateIterator {
  private final LinkedList<Object> list;

  private LengineSequence() {
    this.list = new LinkedList<>();
  }
  private LengineSequence(LinkedList<Object> list) {
    this.list = list;
  }

  public static LengineSequence create(String str) {
    char[] chs = str.toCharArray();
    LengineSequence ret = new LengineSequence();
    for (char ch : chs) {
      ret.list.add(ch);
    }
    return ret;
  }

  public static LengineSequence create(CreateIterator o) {
    LengineIterator it = o.iterator();
    LengineSequence ret = new LengineSequence();
    it.forEachRemaining(ret::add);
    return ret;
  }

  private static LengineSequence create(LinkedList<Object> origin) {
    return new LengineSequence(origin);
  }

  public static LengineSequence create(LengineIterator it) {
    LinkedList<Object> obj = new LinkedList<>();
    it.forEachRemaining(obj::add);
    return create(obj);
  }

  @Override
  public LengineIterator iterator() {
    return new LengineSequenceIterator(list);
  }

  @Override
  public Long len() {
    return (long) list.size();
  }

  @Override
  public String printable(boolean isFirst) {
    return list.stream().map(Object::toString).collect(Collectors.joining(" ", "(seq ", ")"));
  }

  public LengineSequence add(Object o) {
    LinkedList<Object> newList = new LinkedList<>(list);
    newList.add(o);
    return create(newList);
  }

  @Override
  public String toString() {
    return printable(true);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof LengineSequence)) return false;

    LengineSequence that = (LengineSequence) o;

    return list.equals(that.list);
  }

  @Override
  public int hashCode() {
    return list.hashCode();
  }
}
