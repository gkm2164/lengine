package lengine.collections;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import lengine.collections.traits.AddFront;
import lengine.collections.traits.AddRear;
import lengine.collections.traits.LengineIterable;
import lengine.collections.traits.LengineIterator;
import lengine.collections.traits.Addable;
import lengine.collections.traits.Buildable;
import lengine.collections.traits.Nillable;
import lengine.collections.traits.Singleton;
import lengine.collections.traits.Wrap;

public abstract class LengineSet implements
    LengineIterable,
    Nillable<LengineSet>,
    Addable<LengineSet>,
    AddFront<LengineSet>,
    AddRear<LengineSet>,
    Singleton<LengineSet>,
    Buildable<LengineSet, LengineSetBuilder>,
    Wrap<LengineSet> {
  protected final boolean lately;

  protected LengineSet(boolean lately) {
    this.lately = !lately;
  }

  public static LengineSet create(String toString) {
    Set<Object> hashSet = new HashSet<>();
    for (char c : toString.toCharArray()) {
      hashSet.add(c);
    }
    return new LeafSet(hashSet);
  }

  public static LengineSet create(LengineIterable o) {
    Set<Object> hashSet = new HashSet<>();
    o.iterator().forEachRemaining(hashSet::add);
    return new LeafSet(hashSet);
  }

  public static LengineSet create(Set<?> set) {
    return new LeafSet(set.stream().map((elem) -> (Object) elem).collect(Collectors.toSet()));
  }

  public abstract Boolean contains(Object elem);

  public static LengineSet empty() {
    return new LeafSet();
  }

  public LengineSet append(LengineSet other) {
    LengineSet current = this;
    LengineIterator setIterator = other.iterator();
    while (setIterator.hasNext()) {
      Object item = setIterator.next();
      current = current.add(item);
    }
    return current;
  }

  public abstract LengineSet add(Object object);

  public abstract LengineSet remove(Object elem);

  public abstract Long len();

  @Override
  public String toString() {
    return "(set " + printable() + ")";
  }

  protected abstract String printable();

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!(obj instanceof LengineSet)) {
      return false;
    }

    Set<Object> thisSet = new HashSet<>();
    Set<Object> thatSet = new HashSet<>();

    LengineIterator it = this.iterator();
    it.forEachRemaining(thisSet::add);

    LengineIterator thatIt = ((LengineSet) obj).iterator();
    thatIt.forEachRemaining(thatSet::add);

    return Objects.equals(thisSet, thatSet);
  }

  @Override
  public LengineSet NIL() {
    return new LeafSet();
  }

  @Override
  public LengineSet ADD(Object item) {
    return this.add(item);
  }

  /**
   * Since the set is unordered, there's no meaning around FRONT/REAR. Just add item.
   * */
  @Override
  public LengineSet ADD_FRONT(Object item) {
    return ADD(item);
  }

  @Override
  public LengineSet ADD_REAR(Object item) {
    return ADD(item);
  }

  @Override
  public LengineSet PURE(Object elem) {
    return new LeafSet(Collections.singleton(elem));
  }

  @Override
  public LengineSetBuilder BUILDER() {
    return new LengineSetBuilder();
  }

  @Override
  public LengineSet WRAP() {
    Set<Object> set = new HashSet<>();
    iterator().forEachRemaining(set::add);
    return new LeafSet(set);
  }

  @Override
  public Boolean IS_NIL() {
    return this.len() == 0;
  }
}
