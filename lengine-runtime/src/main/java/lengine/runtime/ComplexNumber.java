package lengine.runtime;

public class ComplexNumber {
  private final Number real;
  private final Number imagine;

  public ComplexNumber(final Number real,
                       final Number imagine) {
    this.real = real;
    this.imagine = imagine;
  }

  @Override
  public String toString() {
    return String.format("%s+%si", real.toString(), imagine.toString());
  }
}
