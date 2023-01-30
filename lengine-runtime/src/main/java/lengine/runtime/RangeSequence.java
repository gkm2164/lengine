package lengine.runtime;

public class RangeSequence implements CreateIterator {
  private final int from;
  private final int to;

  private RangeSequence(int from, int to) {
    this.from = from;
    this.to = to;
  }

  public static RangeSequence create(Long from, Long to) {
    return new RangeSequence(from.intValue(), to.intValue());
  }

  @Override
  public String toString() {
    return "Ranged sequence from " + from + " to " + to;
  }

  @Override
  public LengineIterator iterator() {
    return new RangeSequenceIterator(from, to);
  }

  @Override
  public Object len() {
    return (long)Math.abs(to - from);
  }
}
