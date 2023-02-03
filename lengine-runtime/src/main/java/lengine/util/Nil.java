package lengine.util;

import lengine.runtime.LengineIterator;

public class Nil extends LengineList {
    private static Nil singleton = null;

    public static LengineList get() {
        if (singleton == null) {
            singleton = new Nil();
        }

        return singleton;
    }

    @Override
    public LengineIterator iterator() {
        return new LengineListIterator(this);
    }

    @Override
    public Long len() {
        return 0L;
    }

    @Override
    public String toString() {
        return "nil";
    }

    @Override
    public String printable(boolean isFirst) {
        return "]";
    }
}
