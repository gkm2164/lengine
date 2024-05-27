package lengine.types.collections;

import lengine.functions.LengineLambda0;
import lengine.types.collections.traits.LengineIterable;
import lengine.types.LengineLazyValue;

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
