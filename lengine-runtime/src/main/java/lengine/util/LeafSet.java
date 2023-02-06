package lengine.util;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

class LeafSet extends LengineSet {
  private final Set<Object> set;

  protected LeafSet() {
    this.set = new HashSet<>();
  }

  public LeafSet(Set<Object> elems) {
    this.set = elems;
  }

  @Override
  public Boolean contains(Object elem) {
    return this.set.contains(elem);
  }

  @Override
  public LengineSet remove(Object elem) {
    return new LeafSet(set.stream().filter(x -> x.equals(elem)).collect(Collectors.toSet()));
  }

  @Override
  public Long len() {
    return (long) this.set.size();
  }

  @Override
  protected String printable() {
    return this.set.stream().map(Object::toString).collect(Collectors.joining(" "));
  }
}
