package lengine.util;

import lengine.runtime.LengineIterator;

public class NonLeafSequence extends LengineSequence {
    private final LengineSequence left;
    private LengineSequence right;

    protected NonLeafSequence(
            final LengineSequence left,
            final LengineSequence right
    ) {
        this.left = left;
        this.right = right;
    }

    @Override
    public LengineIterator iterator() {
        return new NonLeafSequenceIterator(left, right);
    }

    @Override
    public Long len() {
        return this.left.len() + this.right.len();
    }

    @Override
    public String printable(boolean isFirst) {
        String[] res = {left.printable(true), right.printable(false)};
        return String.join(" ", res);
    }

    @Override
    public LengineSequence append(LengineSequence seq) {
        this.right = this.right.append(seq);
        return this;
    }

    @Override
    public LengineSequence take(long n) {
        LengineSequence leftSeq = this.left.take(n);
        long size = leftSeq.len();
        if (size == n) {
            return leftSeq;
        }

        LengineSequence rightSeq = this.right.take(n - size);
        return new NonLeafSequence(leftSeq, rightSeq);
    }

    @Override
    public LengineSequence drop(long n) {
        if (this.left.len() <= n) {
            return this.right.drop(n - this.left.len());
        }
        return new NonLeafSequence(this.left.drop(n), this.right);
    }

    @Override
    public int hashCode() {
        return this.left.hashCode() * (int) Math.pow(31, this.left.len()) + this.right.hashCode();
    }
}
