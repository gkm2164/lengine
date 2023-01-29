package lengine.runtime;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class LengineFn {
  private static final Set<String> initialized = new HashSet<>();
  private static final Map<String, ArrayList<Method>> methodMap = new HashMap<>();

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
    if (!initialized.contains(clsName)) {
      initMethodCache(clsName);
    }

    Class[] clsArr = new Class[args.length + captured.length];
    Arrays.fill(clsArr, Object.class);

    if (!methodMap.containsKey(name)) {
        throw new RuntimeException("no such method defined!");
      }

    List<Method> matchedMethods = methodMap.get(name)
          .stream()
          .filter(x -> x.getParameters().length == clsArr.length + 1)
          .collect(Collectors.toList());

    if (matchedMethods.isEmpty()) {
        throw new RuntimeException("unable to find method with signature");
      }
    if (matchedMethods.size() > 1) {
        throw new RuntimeException("more than one method identified for the given");
      }

    Method method = matchedMethods.stream().findFirst().get();
    return new LengineFn(name, args, method, captured);
  }

  private static void initMethodCache(String className) {
    try {
      for (Method method : Class.forName(className).getMethods()) {
        List<Method> arrayList = methodMap.computeIfAbsent(method.getName(), (key) -> new ArrayList<>());
        arrayList.add(method);
      }
    } catch (ClassNotFoundException e) {
      throw new RuntimeException("Failed to initialize!");
    }
    initialized.add(className);
  }

  public Object invoke(Object... args) {
    try {
      Object[] includingCaptures = new Object[1 + args.length + captured.length];
      includingCaptures[0] = this;
      System.arraycopy(args, 0, includingCaptures, 1, args.length);
      System.arraycopy(captured, 0, includingCaptures, 1 + args.length, captured.length);

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
