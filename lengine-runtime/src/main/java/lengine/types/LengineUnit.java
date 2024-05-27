package lengine.types;

public class LengineUnit {
  private static LengineUnit singleton = null;

  private LengineUnit() {

  }

  public static LengineUnit create() {
    if (singleton == null) {
      singleton = new LengineUnit();
    }

    return singleton;
  }

  public String toString() {
    return "()";
  }
}
