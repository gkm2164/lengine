package lengine.util;

import lengine.runtime.LengineIterable;
import lengine.runtime.LengineLazyValue;

public class UnresolvedStream extends LengineStream {
  private final LengineLazyValue provider;

  UnresolvedStream(LengineLazyValue provider) {
    this.provider = provider;
  }

  public LengineStream force() {
    return (LengineStream) provider.invoke();
  }

  public static UnresolvedStream create(LengineLazyValue lazyValue) {
    return new UnresolvedStream(lazyValue);
  }

  @Override
  public Long len() {
    return 1L;
  }

  @Override
  public Object head() {
    return force().head();
  }

  @Override
  public LengineIterable tail() {
    return force().tail();
  }

  @Override
  public String toString() {
    if (provider.isResolved()) {
      return provider.invoke().toString();
    } else {
      return "...";
    }
  }
}
