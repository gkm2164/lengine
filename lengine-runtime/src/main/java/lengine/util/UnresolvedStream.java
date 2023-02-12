package lengine.util;

import lengine.runtime.LengineLazyValue;

public class UnresolvedStream extends LengineStream {
  private final LengineLazyValue provider;

  UnresolvedStream(LengineLazyValue provider) {
    this.provider = provider;
  }

  public Object force() {
    return provider.invoke();
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
    return provider;
  }
}