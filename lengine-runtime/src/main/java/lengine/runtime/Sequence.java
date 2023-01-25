package lengine.runtime;

import static java.util.stream.Collectors.joining;

import java.util.Iterator;
import java.util.LinkedList;

public class Sequence {
  final private LinkedList<Object> list;

  private Sequence(LinkedList<Object> objects) {
    this.list = objects;
  }

  public Sequence() {
    this.list = new LinkedList<>();
  }

  public void add(int item) {
    list.add(item);
  }

  public void add(long item) {
    list.add(item);
  }

  public void add(double item) {
    list.add(item);
  }

  public void add(String item) {
    list.add(item);
  }

  public void add(Sequence item) {
    list.add(item);
  }

  public Object head() {
    return list.getFirst();
  }

  public Sequence tail() {
    return drop(1);
  }

  public Sequence take(int n) {
    LinkedList<Object> subList = new LinkedList<>(list.subList(0, n));
    return new Sequence(subList);
  }

  public Sequence drop(int n) {
    LinkedList<Object> subList = new LinkedList<>();
    Iterator<Object> it = list.iterator();
    for (int i = 0; i < n && it.hasNext(); i++) {
      it.next();
    }
    it.forEachRemaining(subList::add);
    return new Sequence(subList);
  }

  public String toString() {
    return "[" + list.stream()
        .map(Object::toString)
        .collect(joining(" ")) + "]";
  }
}
