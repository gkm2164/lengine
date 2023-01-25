package lengine.runtime;

public class RatioNumber extends Number {
  private final int above;
  private final int under;

  public RatioNumber(final int above, final int under) {
    if (under == 0) {
      throw new IllegalArgumentException("under cannot be 0");
    }
    this.above = above;
    this.under = under;
  }

  public String toString() {
    return String.format("%d/%d", above, under);
  }

  @Override
  public int intValue() {
    return above / under;
  }

  @Override
  public long longValue() {
    return (long)above / under;
  }

  @Override
  public float floatValue() {
    return (float)above / under;
  }

  @Override
  public double doubleValue() {
    return (double)above / under;
  }
}
