package lengine.runtime;

import static java.util.stream.Collectors.joining;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.Objects;

public class Sequence implements CreateIterator {
  final private LinkedList<Object> list;

  private Sequence(LinkedList<Object> objects) {
    this.list = objects;
  }

  public Sequence() {
    this.list = new LinkedList<>();
  }

  public static Sequence create() {
    return new Sequence();
  }

  public static Sequence create(Object... vars) {
    return new Sequence(new LinkedList<>(Arrays.asList(vars)));
  }

  public void add(Character item) {
    list.add(item);
  }

  public void add(Long item) {
    list.add(item);
  }

  public void add(Double item) {
    list.add(item);
  }

  public void add(String item) {
    list.add(item);
  }

  public void add(Object item) { list.add(item); }

  public void addObject(Object item) { list.add(item); }


  public void add(Sequence item) {
    list.add(item);
  }

  public void append(Sequence toBeAppended) {
    list.addAll(toBeAppended.list);
  }

  public String toString() {
    return "[" + list.stream()
        .map(Object::toString)
        .collect(joining(" ")) + "]";
  }

  public Sequence flatten() {
    Sequence retSeq = new Sequence();
    for (Object item : list) {
      if (item instanceof Sequence) {
        retSeq.append(((Sequence) item).flatten());
      } else {
        retSeq.add(item);
      }
    }

    return retSeq;
  }

  @Override
  public Object len() {
    return (long)list.size();
  }

  @Override
  public SequenceIterator iterator() {
    return new SequenceIterator(list);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Sequence sequence = (Sequence) o;
    return Objects.equals(list, sequence.list);
  }
}
