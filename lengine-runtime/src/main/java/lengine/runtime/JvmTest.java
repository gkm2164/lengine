package lengine.runtime;

public class JvmTest {
  static {
    Boolean x = true;
    Boolean y;
    if (x) {
      System.out.println("1234");
      y = !x;
    } else {
      System.out.println("23456");
      y = x;
    }
  }
}
