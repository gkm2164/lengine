package lengine.util;

import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;

import java.util.LinkedList;
import java.util.stream.Collectors;

/**
 * Accessed with `seq`
 */
public class LengineSequence implements CreateIterator {
    private final LinkedList<Object> list = new LinkedList<>();

    public static LengineSequence create(String str) {
        char[] chs = str.toCharArray();
        LengineSequence ret = new LengineSequence();
        for (char ch : chs) {
            ret.list.add(ch);
        }
        return ret;
    }

    public static LengineSequence create(CreateIterator o) {
        LengineIterator it = o.iterator();
        LengineSequence ret = new LengineSequence();
        it.forEachRemaining(ret::add);
        return ret;
    }

    @Override
    public LengineIterator iterator() {
        return new LengineSequenceIterator(list);
    }

    @Override
    public Long len() {
        return (long) list.size();
    }

    @Override
    public String printable(boolean isFirst) {
        return list.stream().map(Object::toString).collect(Collectors.joining(" ", "(list ", ")"));
    }

    public void add(Object o) {
        list.add(o);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LengineSequence)) return false;

        LengineSequence that = (LengineSequence) o;

        return list.equals(that.list);
    }

    @Override
    public int hashCode() {
        return list.hashCode();
    }
}
