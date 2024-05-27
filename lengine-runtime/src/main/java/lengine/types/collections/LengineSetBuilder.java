package lengine.types.collections;

import java.util.HashSet;
import java.util.Set;

import lengine.types.collections.traits.CollectionBuilder;

public class LengineSetBuilder implements CollectionBuilder<LengineSet> {
  private final Set<Object> set = new HashSet<>();

  @Override
  public void ADD(Object elem) {
    set.add(elem);
  }

  @Override
  public LengineSet BUILD() {
    return new LeafSet(set);
  }
}
