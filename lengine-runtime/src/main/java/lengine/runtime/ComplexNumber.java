package lengine.runtime;

import lengine.util.LengineMapKey;

import static lengine.Prelude._ADD;
import static lengine.Prelude._DIV;
import static lengine.Prelude._MULT;
import static lengine.Prelude._SUB;

public class ComplexNumber implements LengineObjectType {
  private final Number real;
  private final Number imagine;

  public ComplexNumber(final Number real,
                       final Number imagine) {
    this.real = real;
    this.imagine = imagine;
  }

  @Override
  public Object get(LengineMapKey key) {
    switch (key.getKey()) {
      case "real":
        return this.real;
      case "imagine":
        return this.imagine;
      default:
        throw new RuntimeException("Unknown accessor for complex number: " + key);
    }
  }

  @Override
  public String toString() {
    return String.format("{%s + %si}", real.toString(), imagine.toString());
  }

  @Override
  public int hashCode() {
    return this.real.hashCode() * 31 + this.imagine.hashCode();
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
    // 1 / (a + bi) == (a - bi) / (a^2 - b^2)

    ComplexNumber commonMult = new ComplexNumber(this.real, (Number)_SUB.invoke(0L, this.imagine));
    Number commonDiv = (Number)_SUB.invoke(
            _MULT.invoke(this.real, this.real),
            _MULT.invoke(this.imagine, this.imagine)
    );

    return new ComplexNumber(
        (Number)_DIV.invoke(commonMult.real.doubleValue(), commonDiv),
        (Number)_DIV.invoke(commonMult.imagine.doubleValue(), commonDiv)
    );
  }

  public static ComplexNumber create(Number over, Number under) {
    return new ComplexNumber(over, under);
  }
}
