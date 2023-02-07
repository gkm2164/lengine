package lengine.util;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

public abstract class LengineSet implements CreateIterator {
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

  public static LengineSet create(CreateIterator o) {
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
}
