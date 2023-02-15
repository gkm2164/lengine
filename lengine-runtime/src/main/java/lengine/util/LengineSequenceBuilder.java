package lengine.util;

import java.util.LinkedList;
import java.util.List;

public class LengineSequenceBuilder implements CollectionBuilder<LengineSequence> {
  private final List<Object> list = new LinkedList<>();

  @Override
  public CollectionBuilder<LengineSequence> ADD(Object elem) {
    list.add(elem);
    return this;
  }

  @Override
  public LengineSequence BUILD() {
    return new LeafSequence(list);
  }
}
