package lengine.util;

import lengine.runtime.LengineIterator;

public class NonLeafSetIterator implements LengineIterator {
    private final LengineIterator leftIterator;
    private final LengineIterator rightIterator;

    public NonLeafSetIterator(LengineSet left, LengineSet right) {
        this.leftIterator = left.iterator();
        this.rightIterator = right.iterator();
    }

    @Override
    public boolean hasNext() {
        return leftIterator.hasNext() || rightIterator.hasNext();
    }

    @Override
    public Object next() {
        if (leftIterator.hasNext()) {
            return leftIterator.next();
        } else if (rightIterator.hasNext()) {
            return rightIterator.next();
        }

        throw new RuntimeException("No more items left");
    }
}
