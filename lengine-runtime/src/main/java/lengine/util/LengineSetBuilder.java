package lengine.util;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class LengineSetBuilder implements CollectionBuilder<LengineSet> {
  private final Set<Object> set = new HashSet<>();

  @Override
  public LengineSetBuilder ADD(Object elem) {
    set.add(elem);
    return this;
  }

  @Override
  public LengineSet BUILD() {
    return new LeafSet(set);
  }
}
