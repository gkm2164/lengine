package lengine.util;

import java.util.LinkedList;
import java.util.List;

public class LengineSequenceBuilder implements CollectionBuilder<LengineSequence> {
  private final List<Object> list = new LinkedList<>();

  @Override
  public void ADD(Object elem) {
    list.add(elem);
  }

  @Override
  public LengineSequence BUILD() {
    return new LeafSequence(list);
  }
}
