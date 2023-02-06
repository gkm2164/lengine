package lengine.runtime.exceptions;

public class LengineTypeMismatchException extends RuntimeException {
  public LengineTypeMismatchException(Object elem, Class<?> type) {
    super(String.format("%s is not a %s type", elem, type));
  }
}
