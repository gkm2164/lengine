package lengine.runtime;

import java.util.function.IntConsumer;

/**
 * Where Lengine's {@code exit} goes. Defaults to {@link System#exit(int)}; a REPL or a test can
 * install its own handler (for example one that throws) instead of a SecurityManager, which
 * Java 18+ no longer supports.
 */
public final class ExitHandler {
    private static volatile IntConsumer handler = System::exit;

    private ExitHandler() {
    }

    public static void exit(int code) {
        handler.accept(code);
    }

    /** Installs a handler and returns the previous one, so callers can restore it. */
    public static IntConsumer set(IntConsumer next) {
        IntConsumer prev = handler;
        handler = next == null ? System::exit : next;
        return prev;
    }
}
