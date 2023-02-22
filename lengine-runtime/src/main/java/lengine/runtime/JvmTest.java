package lengine.runtime;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
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

  int xsss;
  public JvmTest(int xsss) {
    this.xsss = xsss;
  }

  public void something(int xx) {
    something2(xx, xsss);
  }

  public void something2(int x, int y) {
    Object z;
    PrintStream stream = System.out;
    try {
      System.out.println("Hello");
      z = "hello";
    } catch (Exception e) {
      System.out.println("asdf");
      z = e;
    }

    String k = "1234";
    String l = "23456";

    String m = k + l;
    System.out.println(m);

    if (z == null) {
      System.out.println("!??");
      throw new RuntimeException();
    }

    stream.println(z);

    final int xyz = 10;
  }
}
