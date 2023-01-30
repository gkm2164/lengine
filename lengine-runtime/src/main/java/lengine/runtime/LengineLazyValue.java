package lengine.runtime;

import lengine.functions.LengineLambda0;

public class LengineLazyValue<T> {
    private final LengineLambda0<T> value;

    public LengineLazyValue(LengineLambda0<T> value) {
        this.value = value;
    }

    public T get() {
        return value.invoke();
    }
}
