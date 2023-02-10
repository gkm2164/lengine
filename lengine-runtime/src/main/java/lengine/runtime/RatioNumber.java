package lengine.runtime;

public class RatioNumber extends Number {
  private final long over;
  private final long under;

  public RatioNumber(final long over, final long under) {
    if (under == 0) {
      throw new IllegalArgumentException("under cannot be 0");
    }
    this.over = over;
    this.under = under;
  }

  public String toString() {
    return String.format("%d/%d", over, under);
  }

  @Override
  public int intValue() {
    return (int) (over / under);
  }

  @Override
  public long longValue() {
    return over / under;
  }

  @Override
  public float floatValue() {
    return (float) over / under;
  }

  @Override
  public double doubleValue() {
    return (double) over / under;
  }

  public RatioNumber add(RatioNumber other) {

    return new RatioNumber(over * other.under + under * other.over, other.under * under);
  }

  public RatioNumber mult(RatioNumber other) {
    return new RatioNumber(over * other.over, under * other.under);
  }

  public RatioNumber inverse() {
    return new RatioNumber(under, over);
  }

  public static RatioNumber create(long over, long under) {
    return new RatioNumber(over, under);
  }
}
