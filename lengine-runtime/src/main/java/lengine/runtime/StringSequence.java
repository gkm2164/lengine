package lengine.runtime;

public class StringSequence {
  private String str;

  public StringSequence(String str) {
    this.str = str;
  }

  public StringSequence create(String str) {
    return new StringSequence(str);
  }

  public void add(Object elem) {
    str += elem;
  }

  public String take(int n) {
    return str.substring(0, n);
  }

  public String drop(int n) {
    return str.substring(n);
  }

  public String takeWhile(LengineFn fn) {
    for (int i = 0; i < str.length(); i++) {
      if (!(Boolean) fn.invoke(str.charAt(i))) {
        return str.substring(0, i);
      }
    }
    return str;
  }

  public String dropWhile(LengineFn fn) {
    for (int i = 0; i < str.length(); i++) {
      if (!(Boolean) fn.invoke(str.charAt(i))) {
        return str.substring(i);
      }
    }
    return str;
  }
}
