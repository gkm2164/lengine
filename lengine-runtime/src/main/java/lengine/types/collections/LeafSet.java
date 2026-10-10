package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The set implementation: immutable and insertion-ordered (so printing is stable). Each add copies
 * the backing set, leaving older sets intact.
 */
class LeafSet extends LengineSet {
  private final Set<Object> set;

  protected LeafSet() {
    this(new LinkedHashSet<>());
  }

  public LeafSet(Set<Object> elems) {
    super(false);
    this.set = elems instanceof LinkedHashSet ? elems : new LinkedHashSet<>(elems);
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
    Set<Object> copy = new LinkedHashSet<>(set);
    copy.add(object);
    return new LeafSet(copy);
  }

  @Override
  public LengineSet remove(Object elem) {
    return new LeafSet(set.stream().filter(x -> !Objects.equals(x, elem)).collect(Collectors.toCollection(LinkedHashSet::new)));
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
    return this.set.iterator().next();
  }

  @Override
  public LengineIterable tail() {
    Set<Object> rest = new LinkedHashSet<>(set);
    Iterator<Object> it = rest.iterator();
    if (it.hasNext()) {
      it.next();
      it.remove();
    }
    return new LeafSet(rest);
  }

  @Override
  protected String printable() {
    return this.set.stream().map(Object::toString).collect(Collectors.joining(" "));
  }
}
