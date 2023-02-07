package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.util.Objects;

public class Cons extends LengineList {
    Object item;
    LengineList next;

    Cons(Object item, LengineList next) {
        this.item = item;
        this.next = next;
    }

    public Object get() {
        return item;
    }

    public void setNext(LengineList c) {
        this.next = c;
    }

    @Override
    public LengineIterator iterator() {
        return new LengineListIterator(this);
    }

    @Override
    public Long len() {
        return 1 + next.len();
    }

    @Override
    public String toString() {
        return String.format("(cons %s %s)", item.toString(), next.toString());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Cons)) return false;

        Cons cons = (Cons) o;

        return Objects.equals(item, cons.item)
                && Objects.equals(next, cons.next);
    }

    @Override
    public int hashCode() {
        int result = item != null ? item.hashCode() : 0;
        result = 31 * result + (next != null ? next.hashCode() : 0);
        return result;
    }

    @Override
    public LengineList append(CreateIterator ys) {
        return cons(item, this.next.append(ys));
    }
}
