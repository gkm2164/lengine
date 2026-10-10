package lengine.runtime;

import lengine.runtime.exceptions.LengineRuntimeException;
import lengine.types.functions.*;

/** Call-site check that turns an arity mismatch into a readable error. */
public final class Arity {
    private static final Class<?>[] LAMBDAS = {
            LengineLambda0.class, LengineLambda1.class, LengineLambda2.class, LengineLambda3.class,
            LengineLambda4.class, LengineLambda5.class, LengineLambda6.class, LengineLambda7.class,
            LengineLambda8.class, LengineLambda9.class, LengineLambda10.class, LengineLambda11.class
    };

    private Arity() {
    }

    /** Returns fn if it can be called with n arguments; otherwise throws, naming both counts. */
    public static Object check(Object fn, int n) {
        if (n < LAMBDAS.length && LAMBDAS[n].isInstance(fn)) {
            return fn;
        }
        StringBuilder accepted = new StringBuilder();
        for (int i = 0; i < LAMBDAS.length; i++) {
            if (LAMBDAS[i].isInstance(fn)) {
                accepted.append(accepted.length() == 0 ? "" : " or ").append(i);
            }
        }
        if (accepted.length() == 0) {
            throw new LengineRuntimeException(describe(fn) + " is not a function");
        }
        String plural = accepted.toString().equals("1") ? "" : "s";
        throw new LengineRuntimeException(
                describe(fn) + " expects " + accepted + " argument" + plural + " but got " + n);
    }

    private static String describe(Object fn) {
        if (fn == null) return "nil";
        String name = fn.getClass().getName();
        int dollar = name.lastIndexOf('$');
        return dollar >= 0 && !name.contains("$lambda$") ? name.substring(dollar + 1) : String.valueOf(fn);
    }
}
