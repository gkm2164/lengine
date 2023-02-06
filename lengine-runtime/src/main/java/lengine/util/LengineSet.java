package lengine.util;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import lengine.runtime.CreateIterator;

public abstract class LengineSet {
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

  public abstract Boolean contains(Object elem);

  public static LengineSet empty() {
    return new LeafSet();
  }

  public LengineSet append(LengineSet other) {
    return new NonLeafSet(this, other);
  }

  public LengineSet add(Object... object) {
    return new NonLeafSet(this, new LeafSet(new HashSet<>(Arrays.asList(object))));
  }

  public abstract LengineSet remove(Object elem);

  public abstract Long len();

  protected abstract String printable();

  @Override
  public String toString() {
    return "(set " + printable() + ")";
  }
}
