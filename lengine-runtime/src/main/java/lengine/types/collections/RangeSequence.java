package lengine.types.collections;

import lengine.types.collections.traits.Buildable;
import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;
import lengine.types.collections.traits.Nillable;

public class RangeSequence implements
        LengineIterable,
        Nillable<LengineSequence>,
        Buildable<LengineSequence, LengineSequenceBuilder> {
  private final long from;
  private final long to;

  /** [from, to); a range whose end is not after its start is empty. */
  private RangeSequence(long from, long to) {
    this.from = from;
    this.to = Math.max(from, to);
  }

  public static RangeSequence createRange(Long from, Long to) {
    return new RangeSequence(from, to);
  }

  public static RangeSequence createInclusiveRange(Long from, Long to) {
    return new RangeSequence(from, to + 1);
  }

  @Override
  public String toString() {
    return "[Ranged sequence from " + from + " to " + to + "]";
  }

  @Override
  public LengineIterator iterator() {
    return new RangeSequenceIterator(from, to);
  }

  @Override
  public Long len() {
    return to - from;
  }

  @Override
  public Object head() {
    if (from == to) {
      throw new RuntimeException("empty range has no head");
    }
    return from;
  }

  @Override
  public LengineIterable tail() {
    return from == to ? this : new RangeSequence(from + 1, to);
  }

  @Override
  public Boolean IS_NIL() {
    return from == to;
  }

  @Override
  public LengineSequence NIL() {
    return LeafSequence.create();
  }

  @Override
  public LengineSequenceBuilder BUILDER() {
    return new LengineSequenceBuilder();
  }
}
