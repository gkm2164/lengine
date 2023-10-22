package lengine.runtime;

public interface LengineObject {
    void scriptMain();
    Object importSymbol(LengineString symbolName);
}
