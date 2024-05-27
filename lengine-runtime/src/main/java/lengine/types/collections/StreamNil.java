package lengine.types.collections;


import lengine.types.collections.traits.LengineIterable;

public class StreamNil extends LengineStream {
  private static final StreamNil singleton = new StreamNil();

  public static StreamNil get() {
    return singleton;
  }

  @Override
  public Object head() {
    throw new RuntimeException("no more items left on the stream");
  }

  @Override
  public LengineIterable tail() {
    return singleton;
  }

  @Override
  public String toString() {
    return "'nil";
  }
}
