package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.util.concurrent.atomic.AtomicReference;

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
    public LengineList append(CreateIterator ys) {
        if (ys instanceof LengineList) {
            return (LengineList) ys;
        }

        AtomicReference<LengineList> ref = new AtomicReference<>(Nil.get());
        ys.iterator().forEachRemaining(elem -> ref.set(ref.get().add(ys)));

        return ref.get();
    }
}
