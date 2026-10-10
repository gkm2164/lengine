package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

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
    public LengineList append(LengineIterable ys) {
        if (ys instanceof LengineList) {
            return (LengineList) ys;
        }

        LengineListBuilder builder = new LengineListBuilder();
        ys.iterator().forEachRemaining(builder::ADD);
        return builder.BUILD();
    }
}
