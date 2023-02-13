package lengine.util;

import lengine.runtime.LengineSequenceIterator;

import java.util.Iterator;
import java.util.List;

public class LeafSequenceIterator implements LengineSequenceIterator {
    private final Iterator<Object> listIterator;

    public LeafSequenceIterator(List<Object> list) {
        this.listIterator = list.iterator();
    }

    @Override
    public boolean hasNext() {
        return listIterator.hasNext();
    }

    @Override
    public Object next() {
        return listIterator.next();
    }
}
