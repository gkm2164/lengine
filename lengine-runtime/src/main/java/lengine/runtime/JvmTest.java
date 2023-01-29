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

    LengineIterator li = new LengineIterator() {
      @Override
      public boolean hasNext() {
        return false;
      }

      @Override
      public Object next() {
        return null;
      }
    };

    li.hasNext();
  }
}
