package lengine.types.collections.traits;

public interface CollectionBuilder<T extends LengineIterable> {
    void ADD(Object elem);
    T BUILD();
}
