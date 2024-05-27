package lengine.types.collections;

import lengine.types.collections.traits.LengineIterator;

public class RangeSequenceIterator implements LengineIterator {

  private long current;
  private final long to;

  RangeSequenceIterator(int from, int to) {
    this.current = from;
    this.to = to;
  }

  @Override
  public boolean hasNext() {
    return current < to;
  }

  @Override
  public Object next() {
    return current++;
  }
}
