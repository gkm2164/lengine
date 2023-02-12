package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

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
  public CreateIterator tail() {
    return new LeafSequence(list.subList(1, list.size()));
  }

  @Override
  public String printable(boolean isFirst) {
    return list.stream().map(Object::toString).collect(Collectors.joining(" "));
  }

  @Override
  public LengineSequence append(CreateIterator seq) {
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
  public LengineSequence take(long n) {
    if (this.list.size() <= n) {
      return this;
    }

    return new LeafSequence(this.list.subList(0, (int)Math.max(n, this.list.size())));
  }

  @Override
  public LengineSequence drop(long n) {
    if (this.list.size() >= n) {
      return new LeafSequence(this.list.subList((int)n, this.list.size()));
    }
    return new LeafSequence();
  }

  @Override
  public int hashCode() {
    return list.hashCode();
  }
}
