package lengine.util;

import lengine.runtime.LengineIterator;
import lengine.runtime.LengineSequenceIterator;
import lengine.runtime.exceptions.LengineRuntimeException;

public class NonLeafSequenceIterator implements LengineSequenceIterator {
    private final LengineIterator leftIt;
    private final LengineIterator rightIt;

    public NonLeafSequenceIterator(LengineSequence left, LengineSequence right) {
        this.leftIt = left.iterator();
        this.rightIt = right.iterator();
    }

    @Override
    public boolean hasNext() {
        return leftIt.hasNext() || rightIt.hasNext();
    }

    @Override
    public Object next() {
        if (leftIt.hasNext()) {
            return leftIt.next();
        } else if (rightIt.hasNext()) {
            return rightIt.next();
        }

        throw new LengineRuntimeException("No more items to consume.");
    }
}
