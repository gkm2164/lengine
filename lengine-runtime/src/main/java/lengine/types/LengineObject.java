package lengine.types;

public interface LengineObject {
    void scriptMain();
    Object importSymbol(LengineString symbolName);
}
