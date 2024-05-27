package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

class LeafSet extends LengineSet {
  private final Set<Object> set;

  protected LeafSet() {
    super(false);
    this.set = new HashSet<>();
  }

  public LeafSet(Set<Object> elems) {
    super(false);
    this.set = elems;
  }

  @Override
  public Boolean contains(Object elem) {
    return this.set.contains(elem);
  }

  @Override
  public LengineSet add(Object object) {
    if (set.contains(object)) {
      return this;
    }

    if (set.isEmpty()) {
      return new LeafSet(Collections.singleton(object));
    }

    return new NonLeafSet(super.lately, this, new LeafSet(Collections.singleton(object)));
  }

  @Override
  public LengineSet remove(Object elem) {
    return new LeafSet(set.stream().filter(x -> !x.equals(elem)).collect(Collectors.toSet()));
  }

  @Override
  public LengineIterator iterator() {
    return new LeafSetIterator(set);
  }

  @Override
  public Long len() {
    return (long) this.set.size();
  }

  @Override
  public Object head() {
    return this.set.stream().findFirst().get();
  }

  @Override
  public LengineIterable tail() {
    return new LeafSet(this.set.stream().filter(x -> Objects.equals(x, head())).collect(Collectors.toSet()));
  }

  @Override
  protected String printable() {
    return this.set.stream().map(Object::toString).collect(Collectors.joining(" "));
  }
}
