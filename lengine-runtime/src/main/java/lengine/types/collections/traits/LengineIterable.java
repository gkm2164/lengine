package lengine.types.collections.traits;

public interface LengineIterable {
  LengineIterator iterator();
  Long len();
  Object head();
  LengineIterable tail();
  Boolean IS_NIL();
}
