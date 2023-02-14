package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * General single linked list.
 *
 * Consider to use when it is stack based accumulation.
 * Fast at "head/tail", "cons(appending)" operation
 */

public abstract class LengineList implements CreateIterator, Addable<LengineList>, Nillable<LengineList> {
    @Override
    public LengineList ADD(Object item) {
        return this.add(item);
    }

    @Override
    public LengineList NIL() {
        return Nil.get();
    }

    public static Cons cons(Object item, LengineList next) {
        return new Cons(item, next);
    }

    public static LengineList nil() {
        return Nil.get();
    }

    public static LengineList create(Object[] items) {
        return create(Arrays.stream(items).collect(Collectors.toList()));
    }

    public static LengineList create(Collection<?> keySet) {
        if (keySet.isEmpty()) {
            return Nil.get();
        }
        Iterator<?> it = keySet.iterator();

        Cons head = cons(it.next(), null);
        Cons _this = head;
        while (it.hasNext()) {
            Cons newCons = cons(it.next(), null);
            _this.next = newCons;
            _this = newCons;
        }
        _this.next = Nil.get();
        return head;
    }

    public static LengineList create(String str) {
        Character[] items = new Character[str.length()];
        int i = 0;
        for (Character ch : str.toCharArray()) {
            items[i++] = ch;
        }
        return create(Arrays.stream(items).collect(Collectors.toList()));
    }

    public Object head() {
        if (this instanceof Nil) {
            throw new RuntimeException("nil can't have a value");
        }

        return ((Cons)this).item;
    }

    public LengineList tail() {
        if (this instanceof Nil) {
            return this;
        }

        return ((Cons)this).next;
    }

    @Override
    public final Long len() {
      AtomicInteger length = new AtomicInteger();
      iterator().forEachRemaining(it -> length.incrementAndGet());
      return (long)length.get();
    }

    public LengineList copy() {
        if (this instanceof Nil) {
            return Nil.get();
        }

        Cons _this = (Cons)this;
        return new Cons(_this.item, _this.next);
    }

    public Cons add(Object elem) {
        if (this instanceof Cons) {
            Cons _this = (Cons)this;
            _this.setNext(_this.next.add(elem));
            return _this;
        }
        return cons(elem, nil());
    }

    public abstract LengineList append(CreateIterator ys);


  @Override
  public final String toString() {
    LengineIterator it = this.iterator();
    StringBuilder sb = new StringBuilder();
    StringBuilder pars = new StringBuilder();

    it.forEachRemaining(elem -> {
      sb.append(" (cons ");
      sb.append(elem);
      pars.append(")");
    });

    return (sb.append(" nil").substring(1) + pars).substring(1);
  }
}
