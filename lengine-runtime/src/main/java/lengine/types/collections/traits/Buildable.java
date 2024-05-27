package lengine.types.collections.traits;

public interface Buildable<T extends LengineIterable, U extends CollectionBuilder<T>> {
    U BUILDER();
}
