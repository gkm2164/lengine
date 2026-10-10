package lengine.runtime;

import lengine.types.LengineObject;
import lengine.types.LengineString;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

public class LengineClassLoader {
    private static final Map<String, LengineObject> alreadyLoadedClass = new HashMap<>();
    /** Modules whose top level is running right now, to report import cycles instead of recursing forever. */
    private static final java.util.Set<String> loading = new java.util.LinkedHashSet<>();

    private static String[] split(String qualifiedName) {
        String[] splitted = qualifiedName.split("\\.");
        String[] moduleName = new String[splitted.length - 1];
        String symbolName = splitted[splitted.length - 1];

        System.arraycopy(splitted, 0, moduleName, 0, splitted.length - 1);

        return new String[]{String.join(".", moduleName), symbolName};
    }

    public static Object importSymbol(LengineString qualifiedName) {
        String[] classAndSymbolName = split(qualifiedName.toString());
        String className = classAndSymbolName[0];
        String symbolName = classAndSymbolName[1];

        try {
            if (!alreadyLoadedClass.containsKey(className)) {
                if (loading.contains(className)) {
                    throw new RuntimeException("import cycle: " + String.join(" -> ", loading) + " -> " + className);
                }
                ClassLoader cl = Thread.currentThread().getContextClassLoader();
                Class<?> cls = cl.loadClass(className);

                if (!LengineObject.class.isAssignableFrom(cls)) {
                    throw new RuntimeException("The name " + className + " is not a Lengine module");
                }

                Constructor<?> constructor = cls.getConstructor();
                LengineObject lengineObject = (LengineObject) constructor.newInstance();
                loading.add(className);
                try {
                    lengineObject.scriptMain();
                } finally {
                    loading.remove(className);
                }
                alreadyLoadedClass.put(className, lengineObject);
            }
            LengineObject lengineObject = alreadyLoadedClass.get(className);
            return lengineObject.importSymbol(LengineString.create(symbolName));
        } catch (InvocationTargetException e) {
            if (e.getCause() instanceof RuntimeException) {
                throw (RuntimeException) e.getCause();
            } else {
                throw new RuntimeException(e);
            }
        } catch (ClassNotFoundException | IllegalAccessException | NoSuchMethodException | InstantiationException e) {
            throw new RuntimeException(e);
        }
    }
}
