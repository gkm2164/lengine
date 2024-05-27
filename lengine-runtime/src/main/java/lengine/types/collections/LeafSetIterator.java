package lengine.types.collections;

import lengine.types.collections.traits.LengineIterator;

import java.util.Iterator;
import java.util.Set;

public class LeafSetIterator implements LengineIterator {
    private final Iterator<Object> thisIterator;

    public LeafSetIterator(Set<Object> set) {
        this.thisIterator = set.iterator();
    }

    @Override
    public boolean hasNext() {
        return thisIterator.hasNext();
    }

    @Override
    public Object next() {
        return thisIterator.next();
    }
}
