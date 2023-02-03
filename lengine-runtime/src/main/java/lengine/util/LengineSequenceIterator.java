package lengine.util;

import lengine.runtime.LengineIterator;

import java.util.LinkedList;

public class LengineSequenceIterator implements LengineIterator {
    private final PeekingIterator<Object> listIterator;

    public LengineSequenceIterator(LinkedList<Object> list) {
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
