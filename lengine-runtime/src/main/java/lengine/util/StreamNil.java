package lengine.util;


public class StreamNil extends LengineStream {
  private static final StreamNil singleton = new StreamNil();

  public static StreamNil get() {
    return singleton;
  }

  @Override
  public Long len() {
    return null;
  }

  @Override
  public Object head() {
    return null;
  }
}
