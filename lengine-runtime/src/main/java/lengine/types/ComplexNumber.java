package lengine.types;

import lengine.types.collections.LengineMapKey;
import lengine.types.collections.LengineSequence;
import lengine.types.collections.traits.LengineObjectWithHelp;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import static lengine.PreludeImpl._ADD;
import static lengine.PreludeImpl._DIV;
import static lengine.PreludeImpl._MULT;
import static lengine.PreludeImpl._SUB;

public class ComplexNumber implements LengineObjectWithHelp {
  private final Number real;
  private final Number imagine;

  public ComplexNumber(final Number real,
                       final Number imagine) {
    this.real = real;
    this.imagine = imagine;
  }

  @Override
  public LengineSequence help() {
    return LengineSequence.create(Stream.of("real", "imagine")
            .map(LengineString::create)
            .map(LengineMapKey::create)
            .collect(Collectors.toList()));
  }

  @Override
  public LengineString help(LengineMapKey key) {
    switch (key.getKey().toString()) {
      case "real":
        return LengineString.create("Get real part number");
      case "imagine":
        return LengineString.create("Get imagine part number");
    }

    return LengineString.create("unknown operation: " + key.getKey());
  }

  @Override
  public Object get(LengineMapKey key) {
    switch (key.getKey().toString()) {
      case "real":
        return this.real;
      case "imagine":
        return this.imagine;
      case "inverse":
        return this.inverse();
      default:
        throw new RuntimeException("Unknown accessor for complex number: " + key);
    }
  }

  @Override
  public String toString() {
    return String.format("{%s + %si}", real.toString(), imagine.toString());
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof ComplexNumber)) return false;
    ComplexNumber that = (ComplexNumber) o;
    return real.equals(that.real) && imagine.equals(that.imagine);
  }

  @Override
  public int hashCode() {
    return this.real.hashCode() * 31 + this.imagine.hashCode();
  }

  public Number getReal() {
    return real;
  }

  public Number getImagine() {
    return imagine;
  }

  public ComplexNumber add(ComplexNumber other) {
    return new ComplexNumber(
          (Number) _ADD.invoke(this.real, other.real),
          (Number) _ADD.invoke(this.imagine, other.imagine)
    );
  }

  public ComplexNumber sub(ComplexNumber other) {
    return new ComplexNumber(
          (Number) _SUB.invoke(this.real, other.real),
          (Number) _SUB.invoke(this.imagine, other.imagine)
    );
  }

  public ComplexNumber mult(ComplexNumber other) {
    // (a + bi) * (c + di)
    // (ac - bd) + (ad + bc)i


    return new ComplexNumber(
          (Number) _SUB.invoke(
              _MULT.invoke(this.real, other.real),
              _MULT.invoke(this.imagine, other.imagine)
          ),
          (Number) _ADD.invoke(
              _MULT.invoke(this.real, other.imagine),
              _MULT.invoke(this.imagine, other.real)
          )
    );
  }

  public ComplexNumber div(ComplexNumber other) {
    return this.mult(other.inverse());
  }

  public ComplexNumber inverse() {
    // 1 / (a + bi) == (a - bi) / (a^2 + b^2)
    Number commonDiv = (Number) _ADD.invoke(
            _MULT.invoke(this.real, this.real),
            _MULT.invoke(this.imagine, this.imagine)
    );

    if (commonDiv.doubleValue() == 0.0) {
      throw new ArithmeticException("inverting fails as common divider became 0");
    }

    return new ComplexNumber(
        exactDiv(this.real, commonDiv),
        exactDiv((Number) _SUB.invoke(0L, this.imagine), commonDiv)
    );
  }

  /** Division that stays exact (a ratio or a Long) for exact operands, and falls back to double. */
  private static Number exactDiv(Number a, Number b) {
    if (isExact(a) && isExact(b)) {
      return toRatio(a).div(toRatio(b)).simplify();
    }
    return a.doubleValue() / b.doubleValue();
  }

  private static boolean isExact(Number n) {
    return n instanceof Long || n instanceof RatioNumber;
  }

  private static RatioNumber toRatio(Number n) {
    return n instanceof RatioNumber ? (RatioNumber) n : RatioNumber.create(n.longValue(), 1L);
  }

  public static ComplexNumber create(Number over, Number under) {
    return new ComplexNumber(over, under);
  }
}
