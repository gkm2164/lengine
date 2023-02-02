package lengine.runtime;

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
}
