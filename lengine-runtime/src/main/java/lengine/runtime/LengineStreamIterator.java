package lengine.runtime;

import lengine.util.LengineStream;
import lengine.util.StreamCons;
import lengine.util.UnresolvedStream;

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
      _this = ((UnresolvedStream)_this).force();
      return _this instanceof StreamCons;
    }

    return false;
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
