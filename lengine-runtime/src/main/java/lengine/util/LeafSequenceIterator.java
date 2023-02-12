package lengine.util;

import lengine.runtime.LengineIterator;
import lengine.runtime.LengineSequenceIterator;

import java.util.LinkedList;
import java.util.List;

public class LeafSequenceIterator implements LengineSequenceIterator {
    private final PeekingIterator<Object> listIterator;

    public LeafSequenceIterator(List<Object> list) {
        this.listIterator = PeekingIterator.peekingIterator(list.iterator());
    }

    @Override
    public boolean hasNext() {
        return listIterator.hasNext();
    }

    @Override
    public Object peek() {
        return listIterator.peek();
    }

    @Override
    public Object next() {
        return listIterator.next();
    }
}
