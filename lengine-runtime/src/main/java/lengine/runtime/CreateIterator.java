package lengine.runtime;

public interface CreateIterator {
  LengineIterator iterator();
  Long len();

  String printable(boolean isFirst);
}
