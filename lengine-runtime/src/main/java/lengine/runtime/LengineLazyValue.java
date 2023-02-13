package lengine.runtime;

import lengine.functions.LengineLambda0;

public class LengineLazyValue implements LengineLambda0<Object> {
    private Object value = null;
    private final LengineLambda0<Object> resolver;

    private LengineLazyValue(LengineLambda0<Object> resolver) {
        this.resolver = resolver;
    }

    public Object force() {
      return invoke();
    }

    @Override
    public Object invoke() {
        if (value != null) {
            return value;
        }

        value = resolver.invoke();
        return value;
    }

    public boolean isResolved() {
      return value != null;
    }

    public static LengineLazyValue create(LengineLambda0<Object> resolver) {
        return new LengineLazyValue(resolver);
    }
}
