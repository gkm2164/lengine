package lengine.util;

public class NonLeafSet extends LengineSet {
  private final LengineSet left;
  private final LengineSet right;

  protected NonLeafSet(final LengineSet left, final LengineSet right) {
    this.left = left;
    this.right = right;
  }

  @Override
  public Boolean contains(Object elem) {
    return left.contains(elem) || right.contains(elem);
  }

  @Override
  public LengineSet remove(Object elem) {
    return new NonLeafSet(this.left.remove(elem), this.right.remove(elem));
  }

  @Override
  public Long len() {
    return this.left.len() + this.right.len();
  }

  @Override
  protected String printable() {
    String[] result = {this.left.printable(), this.right.printable()};

    return String.join(" ", result);
  }
}
