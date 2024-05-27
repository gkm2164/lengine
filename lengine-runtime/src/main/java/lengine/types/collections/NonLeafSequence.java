package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

public class NonLeafSequence extends LengineSequence {
  private final LengineSequence left;
  private final LengineSequence right;

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
  public Object head() {
    return this.left.head();
  }

  @Override
  public LengineIterable tail() {
    if (this.left.len() == 1) {
      return this.right;
    }

    if (this.left.len() == 0) {
      return this.right.tail();
    }

    return new NonLeafSequence((LeafSequence) this.left.tail(), this.right);
  }

  @Override
  public String printable(boolean isFirst) {
    String[] res = {left.printable(true), right.printable(false)};
    return String.join(" ", res);
  }

  @Override
  public LengineSequence append(LengineIterable seq) {
    return new NonLeafSequence(this.left, this.right.append(seq));
  }

  @Override
  public int hashCode() {
    return this.left.hashCode() * (int) Math.pow(31, this.left.len()) + this.right.hashCode();
  }
}
