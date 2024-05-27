package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Accessed with `seq`
 */
public class LeafSequence extends LengineSequence {
  private final List<Object> list;

  protected LeafSequence() {
    this.list = new ArrayList<>();
  }
  protected LeafSequence(List<Object> list) {
    this.list = list;
  }
  protected LeafSequence(String str) {
    this();
    for (char ch : str.toCharArray()) {
      this.list.add(ch);
    }
  }

  public static LeafSequence create() {
    return new LeafSequence();
  }

  public static LeafSequence create(Set<?> set) {
    LinkedList<Object> obj = new LinkedList<>();
    set.iterator().forEachRemaining(obj::add);
    return new LeafSequence(obj);
  }

  @Override
  public LengineIterator iterator() {
    return new LeafSequenceIterator(this.list);
  }

  @Override
  public Long len() {
    return (long) list.size();
  }

  @Override
  public Object head() {
    return this.list.get(0);
  }

  @Override
  public LengineIterable tail() {
    return new LeafSequence(list.subList(1, list.size()));
  }

  @Override
  public String printable(boolean isFirst) {
    return list.stream().map(Object::toString).collect(Collectors.joining(" "));
  }

  @Override
  public LengineSequence append(LengineIterable seq) {
    if (this.len() == 0) {
      List<Object> lst = new LinkedList<>();
      seq.iterator().forEachRemaining(lst::add);
      return new LeafSequence(lst);
    }
    return new NonLeafSequence(this, LeafSequence.create(seq));
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof LeafSequence)) return false;

    LeafSequence that = (LeafSequence) o;

    return list.equals(that.list);
  }

  @Override
  public int hashCode() {
    return list.hashCode();
  }
}
