package lengine.runtime;

import lengine.util.Buildable;
import lengine.util.LeafSequence;
import lengine.util.LengineSequence;
import lengine.util.LengineSequenceBuilder;
import lengine.util.Nillable;

public class RangeSequence implements
        LengineIterable,
        Nillable<LengineSequence>,
        Buildable<LengineSequence, LengineSequenceBuilder> {
  private final int from;
  private final int to;

  private RangeSequence(int from, int to) {
    this.from = from;
    this.to = to;
  }

  public static RangeSequence createRange(Long from, Long to) {
    return new RangeSequence(from.intValue(), to.intValue());
  }


  public static RangeSequence createInclusiveRange(Long from, Long to) {
    return new RangeSequence(from.intValue(), to.intValue() + 1);
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
    return (long)Math.abs(to - from);
  }

  @Override
  public Object head() {
    return from;
  }

  @Override
  public LengineIterable tail() {
    return new RangeSequence(from + 1, to);
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
