package lengine.runtime;

public abstract class LengineList implements CreateIterator {
    public static Cons cons(Object item, LengineList next) {
        return new Cons(item, next);
    }

    public static LengineList nil() {
        return Nil.get();
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

    public LengineList copy() {
        if (this instanceof Nil) {
            return Nil.get();
        }

        Cons _this = (Cons)this;
        return new Cons(_this.item, _this.next);
    }
}
