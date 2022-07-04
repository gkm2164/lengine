package lengine.runtime;

public class LengineRuntime {
    public LengineValue add(LengineValue a, LengineValue b) {
        Object a_ = a.get();
        Object b_ = b.get();

        if (a_ instanceof Number && b_ instanceof Number) {
            return new LengineValue(((Number) a_).doubleValue() + ((Number) b_).doubleValue());
        } else if (a_ instanceof String) {
            return new LengineValue((String)a_ + b_);
        } else {
            throw new RuntimeException("given parameters are not allowed to +: " + a + ", " + b);
        }
    }
}