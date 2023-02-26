package lengine.runtime;

import lengine.collections.LengineMapKey;
import lengine.collections.LengineSequence;
import lengine.collections.traits.LengineObjectWithHelp;

import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RatioNumber extends Number implements LengineObjectWithHelp {
  private final long over;
  private final long under;

  public RatioNumber(final long over, final long under) {
    if (under == 0) {
      throw new ArithmeticException("under cannot be 0");
    }
    this.over = over;
    this.under = under;
  }

  public RatioNumber normalize() {
    long div = 2;
    long newOver = over;
    long newUnder = under;

    // Find GCD
    while (div <= Math.min(newOver, newUnder)) {
      if (newOver % div == 0 && newUnder % div == 0) {
        newOver /= div;
        newUnder /= div;
      } else {
        div++;
      }
    }

    return new RatioNumber(newOver, newUnder);
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

  public RatioNumber neg() {
    return new RatioNumber(-over, under);
  }

  public RatioNumber sub(RatioNumber other) {
    return this.add(other.neg());
  }

  public RatioNumber mult(RatioNumber other) {
    return new RatioNumber(over * other.over, under * other.under);
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
