package lengine.runtime;

import java.util.Objects;

public class LengineString implements CreateIterator {
    private final String value;

    public LengineString(String value) {
        this.value = value;
    }

    @Override
    public LengineIterator iterator() {
        return new LengineStringIterator(value);
    }

    @Override
    public Long len() {
        return (long)value.length();
    }

    @Override
    public Object head() {
        return value.charAt(0);
    }

    @Override
    public CreateIterator tail() {
        return create(value.substring(1));
    }

    public static LengineString create(String value) {
        return new LengineString(value);
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LengineString)) return false;

        LengineString that = (LengineString) o;

        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return value != null ? value.hashCode() : 0;
    }

    public LengineString add(Object ys) {
        return create(value + ys.toString());
    }
}
