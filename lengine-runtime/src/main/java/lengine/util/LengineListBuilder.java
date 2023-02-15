package lengine.util;

import java.util.LinkedList;
import java.util.List;

public class LengineListBuilder implements CollectionBuilder<LengineList> {
  private final List<Object> list = new LinkedList<>();

  @Override
  public LengineListBuilder ADD(Object elem) {
    list.add(elem);
    return this;
  }

  @Override
  public LengineList BUILD() {
    return LengineList.create(list);
  }
}
