package lengine.types;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

public class LengineStringIterator implements LengineIterator {
    private final String value;
    private int idx = 0;

    public LengineStringIterator(String value) {
        this.value = value;
    }

    @Override
    public boolean hasNext() {
        return idx < value.length();
    }

    @Override
    public Object next() {
        return value.charAt(idx++);
    }

    public LengineIterable _remains() {
        return LengineString.create(value.substring(idx));
    }
}
