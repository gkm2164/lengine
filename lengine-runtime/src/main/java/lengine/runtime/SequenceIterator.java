package lengine.runtime;

import java.util.Iterator;
import java.util.LinkedList;

public class SequenceIterator implements LengineIterator {

  private final Iterator<Object> iterator;

  public SequenceIterator(LinkedList<Object> list) {
    this.iterator = list.iterator();
  }

  public boolean hasNext() {
    return iterator.hasNext();
  }

  public Object next() {
    return iterator.next();
  }
}
