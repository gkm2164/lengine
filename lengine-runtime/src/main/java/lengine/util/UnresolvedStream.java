package lengine.util;

import lengine.functions.LengineLambda0;
import lengine.runtime.LengineIterable;
import lengine.runtime.LengineLazyValue;

public class UnresolvedStream extends LengineStream implements LengineLambda0<LengineStream> {
  private final LengineLazyValue provider;

  UnresolvedStream(LengineLazyValue provider) {
    this.provider = provider;
  }

  public LengineStream force() {
    return (LengineStream) provider.force();
  }

  public boolean isResolved() {
    return this.provider.isResolved();
  }

  public static UnresolvedStream create(LengineLazyValue lazyValue) {
    return new UnresolvedStream(lazyValue);
  }

  @Override
  public Long len() {
    return force().len();
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

  @Override
  public LengineStream invoke() {
    return force();
  }
}
