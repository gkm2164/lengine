package lengine.types;

import lengine.types.collections.LengineMapKey;
import lengine.types.collections.LengineSequence;
import lengine.types.collections.traits.LengineObjectWithHelp;

import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * An exact rational number, always kept in lowest terms with a positive denominator.
 * Arithmetic that lands on a whole number yields a Long (see {@link #simplify()}).
 */
public class RatioNumber extends Number implements LengineObjectWithHelp, Comparable<RatioNumber> {
  private final long over;
  private final long under;

  public RatioNumber(final long over, final long under) {
    if (under == 0) {
      throw new ArithmeticException("under cannot be 0");
    }
    long g = gcd(Math.abs(over), Math.abs(under));
    long sign = under < 0 ? -1 : 1;
    this.over = Math.multiplyExact(sign, over / g);
    this.under = Math.multiplyExact(sign, under / g);
  }

  private static long gcd(long a, long b) {
    while (b != 0) {
      long t = a % b;
      a = b;
      b = t;
    }
    return a == 0 ? 1 : a;
  }

  /** Kept for compatibility: ratios are always normalized now. */
  public RatioNumber normalize() {
    return this;
  }

  /** This ratio as a Long when it is a whole number, otherwise itself. */
  public Number simplify() {
    return under == 1 ? (Number) over : this;
  }

  public long getOver() {
    return over;
  }

  public long getUnder() {
    return under;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof RatioNumber)) return false;
    RatioNumber that = (RatioNumber) o;
    return over == that.over && under == that.under;
  }

  @Override
  public int hashCode() {
    return Long.hashCode(over) * 31 + Long.hashCode(under);
  }

  @Override
  public int compareTo(RatioNumber o) {
    // a/b vs c/d with b, d > 0: compare a*d and c*b
    return Long.compare(Math.multiplyExact(over, o.under), Math.multiplyExact(o.over, under));
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

    return new RatioNumber(
        Math.addExact(Math.multiplyExact(over, other.under), Math.multiplyExact(under, other.over)),
        Math.multiplyExact(other.under, under));
  }

  public RatioNumber neg() {
    return new RatioNumber(-over, under);
  }

  public RatioNumber sub(RatioNumber other) {
    return this.add(other.neg());
  }

  public RatioNumber mult(RatioNumber other) {
    return new RatioNumber(Math.multiplyExact(over, other.over), Math.multiplyExact(under, other.under));
  }

  public RatioNumber div(RatioNumber other) {
    return this.mult(other.inverse());
  }


  public RatioNumber inverse() {
    return new RatioNumber(under, over);
  }

  public static RatioNumber create(long over, long under) {
    return new RatioNumber(over, under);
  }

  @Override
  public LengineSequence help() {
    return LengineSequence.create(
            Stream.of("over", "under", "norm")
                    .map(LengineString::create)
                    .map(LengineMapKey::create)
                    .collect(Collectors.toList())
    );
  }

  @Override
  public LengineString help(LengineMapKey key) {
    switch (key.getKey().toString()) {
      case "over":
        return LengineString.create("return a in a/b");
      case "under":
        return LengineString.create("return b in a/b");
      case "norm":
        return LengineString.create("return normalized number");
      default:
        return LengineString.create("unknown operation: " + key.getKey());
    }
  }

  @Override
  public Object get(LengineMapKey key) {
    switch(key.getKey().toString()) {
      case "over":
        return this.over;
      case "under":
        return this.under;
      case "norm":
        return this.normalize();
      default:
        throw new RuntimeException("unknown accessor to rational number: " + key);
    }
  }
}
