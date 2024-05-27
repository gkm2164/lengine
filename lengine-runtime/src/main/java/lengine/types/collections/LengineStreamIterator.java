package lengine.types.collections;

import lengine.types.collections.traits.LengineIterable;
import lengine.types.collections.traits.LengineIterator;

public class LengineStreamIterator implements LengineIterator {
  private LengineStream _this;

  public LengineStreamIterator(LengineStream _this) {
    this._this = _this;
  }

  @Override
  public boolean hasNext() {
    if (_this instanceof StreamCons) {
      return true;
    } else if (_this instanceof UnresolvedStream) {
      _this = forceFully(_this);
      return _this instanceof StreamCons;
    }

    return false;
  }

  private LengineStream forceFully(LengineStream unresolvedStream) {
    while (unresolvedStream instanceof UnresolvedStream) {
      unresolvedStream = ((UnresolvedStream) unresolvedStream).force();
    }
    return unresolvedStream;
  }

  @Override
  public Object next() {
    Object result = ((StreamCons)_this).getValue();

    _this = ((StreamCons)_this).getNext();

    return result;
  }

  public LengineIterable _this() {
    return _this;
  }
}
