package lengine.runtime;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

public class LengineFn {
  private final String name;
  private final String[] args;
  private final Method method;
  private final Object[] captured;

  private LengineFn(String name, String[] args, Method method, Object[] captured) {
    this.name = name;
    this.args = args;
    this.method = method;
    this.captured = captured;
  }

  public static LengineFn create(String clsName, String name, String[] args, Object[] captured) {
    try {
      Class[] clsArr = new Class[args.length + captured.length];
      Arrays.fill(clsArr, Object.class);
      Method method = Class.forName(clsName).getMethod(name, clsArr);
      return new LengineFn(name, args, method, captured);
    } catch (ClassNotFoundException | NoSuchMethodException e) {
      throw new RuntimeException(e);
    }
  }

  public Object invoke(Object... args) {
    try {
      Object[] includingCaptures = new Object[args.length + captured.length];
      for (int i = 0; i < args.length; i++) {
        includingCaptures[i] = args[i];
      }

      for (int i = 0; i < captured.length; i++) {
        includingCaptures[i + args.length] = captured[i];
      }
      return method.invoke(null, includingCaptures);
    } catch (IllegalAccessException e) {
      throw new RuntimeException("Illegal approach to access", e);
    } catch (InvocationTargetException e) {
      throw new RuntimeException("Invocation target exception!", e);
    }
  }

  public String toString() {
    return name + "(" + String.join(" ", args) + ")";
  }
}
