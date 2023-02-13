package lengine.util;


import lengine.runtime.CreateIterator;

public class StreamNil extends LengineStream {
  private static final StreamNil singleton = new StreamNil();

  public static StreamNil get() {
    return singleton;
  }

  @Override
  public Long len() {
    return 0L;
  }

  @Override
  public Object head() {
    throw new RuntimeException("no more items left on the stream");
  }

  @Override
  public CreateIterator tail() {
    return singleton;
  }

  @Override
  public String toString() {
    return "'nil";
  }
}
