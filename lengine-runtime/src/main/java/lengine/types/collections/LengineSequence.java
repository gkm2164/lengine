package lengine.types.collections;

import lengine.types.collections.traits.AddFront;
import lengine.types.collections.traits.AddRear;
import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;
import lengine.types.collections.traits.Addable;
import lengine.types.collections.traits.Buildable;
import lengine.types.collections.traits.Nillable;
import lengine.types.collections.traits.Singleton;
import lengine.types.collections.traits.Wrap;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Accessed with `seq`
 */
public abstract class LengineSequence implements LengineIterable,
        Singleton<LengineSequence>,
        Nillable<LengineSequence>,
        Addable<LengineSequence>,
        AddFront<LengineSequence>,
        AddRear<LengineSequence>,
        Buildable<LengineSequence, LengineSequenceBuilder>,
        Wrap<LengineSequence> {

  @Override
  public LengineSequence NIL() {
    return new LeafSequence();
  }

  @Override
  public LengineSequence ADD(Object elem) {
    return this.add(elem);
  }

  @Override
  public LengineSequence PURE(Object elem) {
    return LeafSequence.create(Collections.singletonList(elem));
  }

  public static LengineSequence create(String str) {
    return new LeafSequence(str);
  }

  public static LengineSequence create(LengineIterable o) {
    LinkedList<Object> thisList = new LinkedList<>();
    o.iterator().forEachRemaining(thisList::add);
    return new LeafSequence(thisList);
  }

  public static LengineSequence create(List<Object> origin) {
    return new LeafSequence(origin);
  }

  public static LengineSequence create(LengineIterator it) {
    LinkedList<Object> obj = new LinkedList<>();
    it.forEachRemaining(obj::add);
    return create(obj);
  }

  public static LengineSequence create(Set<?> set) {
    LinkedList<Object> obj = new LinkedList<>();
    set.iterator().forEachRemaining(obj::add);
    return create(obj);
  }

  @Override
  public abstract LengineIterator iterator();

  @Override
  public abstract Long len();

  public abstract String printable(boolean isFirst);

  public LengineSequence add(Object o) {
    LinkedList<Object> newList = new LinkedList<>();
    newList.add(o);

    return append(new LeafSequence(newList));
  }

  public abstract LengineSequence append(LengineIterable seq);
  @Override
  public String toString() {
    return String.format("(seq %s)", printable(true));
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }

    if (!(obj instanceof LengineSequence)) {
      return false;
    }

    LengineIterator thisIt = iterator();
    LengineIterator thatIt = ((LengineSequence)obj).iterator();

    while(thisIt.hasNext() && thatIt.hasNext()) {
      Object _this = thisIt.next();
      Object _that = thatIt.next();

      if (!Objects.equals(_this, _that)) {
        return false;
      }
    }

    return !thisIt.hasNext() && !thatIt.hasNext();
  }

  public abstract int hashCode();

  @Override
  public LengineSequenceBuilder BUILDER() {
    return new LengineSequenceBuilder();
  }

  @Override
  public LengineSequence WRAP() {
    final List<Object> list = new LinkedList<>();
    iterator().forEachRemaining(list::add);
    return new LeafSequence(list);
  }

  @Override
  public Boolean IS_NIL() {
    return len() == 0;
  }

  @Override
  public LengineSequence ADD_FRONT(Object item) {
    return new NonLeafSequence(
        create(Collections.singletonList(item)),
        this
    );
  }

  @Override
  public LengineSequence ADD_REAR(Object item) {
    return this.add(item);
  }
}
