package lengine.runtime;

public class LengineValue {
    Object value;

    public LengineValue(Object value) {
        this.value = value;
    }

    public Object get() {
        return value;
    }
}
