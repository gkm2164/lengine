package lengine.runtime;

import java.text.StringCharacterIterator;

import lengine.functions.LengineLambda1;

public class StringSequence implements CreateIterator {
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

  public String takeWhile(LengineLambda1 fn) {
    for (int i = 0; i < str.length(); i++) {
      if (!(Boolean) fn.invoke(str.charAt(i))) {
        return str.substring(0, i);
      }
    }
    return str;
  }

  public String dropWhile(LengineLambda1 fn) {
    for (int i = 0; i < str.length(); i++) {
      if (!(Boolean) fn.invoke(str.charAt(i))) {
        return str.substring(i);
      }
    }
    return str;
  }

  @Override
  public LengineIterator iterator() {
    final String thisString = str;
    final StringCharacterIterator sci = new StringCharacterIterator(thisString);
    return new LengineIterator() {
      @Override
      public boolean hasNext() {
        return sci.getIndex() < str.length();
      }

      @Override
      public Object peek() {
        return sci.current();
      }

      @Override
      public Object next() {
        char ret = sci.current();
        sci.next();
        return ret;
      }
    };
  }

  @Override
  public Long len() {
    return (long) str.length();
  }

  @Override
  public String printable(boolean isFirst) {
    return str;
  }
}
