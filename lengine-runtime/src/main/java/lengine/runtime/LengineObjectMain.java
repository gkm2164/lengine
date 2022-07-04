package lengine.runtime;

public class LengineObjectMain {
    private static final LengineRuntime runtime = new LengineRuntime();

    public static void main(String[] args) {
        LengineValue a = new LengineValue(10);
        LengineValue b = new LengineValue(10);
        LengineValue lengineValue = runtime.add(a, b);

        System.out.println(lengineValue);
    }
}
