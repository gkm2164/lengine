package lengine.util;

import java.util.LinkedList;
import java.util.List;

public class LengineListBuilder implements CollectionBuilder<LengineList> {
  private final List<Object> list = new LinkedList<>();

  @Override
  public void ADD(Object elem) {
    list.add(elem);
  }

  @Override
  public LengineList BUILD() {
    return LengineList.create(list);
  }
}
