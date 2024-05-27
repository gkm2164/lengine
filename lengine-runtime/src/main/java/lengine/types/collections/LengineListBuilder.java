package lengine.types.collections;

import java.util.LinkedList;
import java.util.List;

import lengine.types.collections.traits.CollectionBuilder;

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
