package lengine.runtime;

import java.util.HashMap;
import java.util.Map;

public class JvmTest {
  public static Map hashMap = new HashMap<>();
  static {
    hashMap.put("Something", "Something");
    Boolean x = true;
    Boolean y;
    if (x) {
      System.out.println("1234");
      y = !x;
    } else {
      System.out.println("23456");
      y = x;
    }

    {
      int a = 30;
      {
        int b = 20;
        {
          System.out.println(a + b);
        }
      }
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
