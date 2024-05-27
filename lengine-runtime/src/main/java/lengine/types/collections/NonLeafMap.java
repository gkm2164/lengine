package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

import java.util.stream.Collectors;
import java.util.stream.Stream;

class NonLeafMap extends LengineMap {
  private final LengineMap left;
  private final LengineMap right;

  protected NonLeafMap(final boolean lately, final LengineMap left, final LengineMap right) {
    super(lately);
    this.left = left;
    this.right = right;
  }

  public LengineMap putEntry(LengineMapEntry entry) {
    if (left.contains(entry.getKey())) {
      return new NonLeafMap(lately, this.left.putEntry(entry), this.right);
    }

    if (lately) {
      return new NonLeafMap(true, this.left, this.right.putEntry(entry));
    } else {
      return new NonLeafMap(false, this.left.putEntry(entry), this.right);
    }

  }

  public Object get(LengineMapKey key) {
    if (this.left.contains(key)) {
      return this.left.get(key);
    } else if (this.right.contains(key)) {
      return this.right.get(key);
    }

    return Nil.get();
  }

  public LengineSet keys() {
    return this.left.keys().append(this.right.keys());
  }

  public LengineSequence entries() {
    return this.left.entries().append(this.right.entries());
  }

  public Stream<String> createStringEntry() {
    return Stream.concat(
      this.left.createStringEntry(),
      this.right.createStringEntry()
    );
  }

  @Override
  public String toString() {
    return this.createStringEntry().collect(Collectors.joining(", ", "{", "}"));
  }

  @Override
  public int hashCode() {
    return this.left.hashCode() * 31 + this.right.hashCode();
  }

  @Override
  public LengineIterator iterator() {
    return entries().iterator();
  }

  public Long len() {
    return this.left.len() + this.right.len();
  }

  @Override
  public Object head() {
    return this.left.head();
  }

  @Override
  public LengineIterable tail() {
    if (this.left.len() == 0) {
      return this.right.tail();
    }

    if (this.left.len() == 1) {
      return this.right;
    }

    return new NonLeafMap(super.lately, (LengineMap) this.left.tail(), this.right);
  }

  protected String printable() {
    return this.createStringEntry().collect(Collectors.joining(", "));
  }

  public boolean contains(LengineMapKey key) {
    return this.left.contains(key) || this.right.contains(key);
  }
}
