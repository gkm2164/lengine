package lengine.util;

import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

public abstract class LengineMap implements CreateIterator {
  protected final boolean lately;

  protected LengineMap(boolean lately) {
    this.lately = !lately;
  }

  public abstract LengineMap putEntry(LengineMapEntry entry);

  public final LengineMap put(LengineMapKey key, Object obj) {
    return this.putEntry(LengineMapEntry.create(key, obj));
  }

  public abstract Object get(LengineMapKey key);


  public abstract LengineSet keys();

  public abstract LengineSequence entries();

  public static LengineMap create() {
    return new LeafMap();
  }

  public static LengineMap create(CreateIterator seq) {
    return new LeafMap(seq);
  }

  public abstract Stream<String> createStringEntry();
  @Override
  public String toString() {
    return this.createStringEntry().collect(Collectors.joining(", ", "{", "}"));
  }

  @Override
  public abstract int hashCode();

  public abstract Long len();

  public abstract boolean contains(LengineMapKey key);

  @Override
  public final boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof LengineMap)) return false;

    LengineSet thisKey = this.keys();
    LengineSet otherKey = ((LengineMap)o).keys();

    if (!Objects.equals(thisKey, otherKey)) {
      return false;
    }

    LengineIterator keyIterator = thisKey.iterator();

    while (keyIterator.hasNext()) {
      LengineMapKey key = (LengineMapKey) keyIterator.next();
      Object _this = this.get(key);
      Object _that = ((LengineMap) o).get(key);
      if (!Objects.equals(_this, _that)) {
        return false;
      }
    }

    return true;
  }

  protected abstract String printable();
}
