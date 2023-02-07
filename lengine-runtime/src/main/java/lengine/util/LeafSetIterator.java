package lengine.util;

import lengine.runtime.LengineIterator;

import java.util.Iterator;
import java.util.Set;

public class LeafSetIterator implements LengineIterator {
    private final PeekingIterator<Object> thisIterator;

    public LeafSetIterator(Set<Object> set) {
        this.thisIterator = PeekingIterator.peekingIterator(set.iterator());
    }

    @Override
    public boolean hasNext() {
        return thisIterator.hasNext();
    }

    @Override
    public Object peek() {
        return thisIterator.peek();
    }

    @Override
    public Object next() {
        return thisIterator.next();
    }
}
