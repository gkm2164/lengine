package lengine.runtime;

import org.apache.commons.collections4.iterators.PeekingIterator;

import java.util.Iterator;
import java.util.LinkedList;

public class SequenceIterator implements LengineIterator {

  private final PeekingIterator<Object> iterator;

  public SequenceIterator(LinkedList<Object> list) {
    this.iterator = PeekingIterator.peekingIterator(list.iterator());
  }

  public boolean hasNext() {
    return iterator.hasNext();
  }

  @Override
  public Object peek() {
    return iterator.peek();
  }

  public Object next() {
    return iterator.next();
  }
}