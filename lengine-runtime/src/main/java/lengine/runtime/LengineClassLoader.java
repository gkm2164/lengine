package lengine.runtime;

import lengine.types.LengineObject;
import lengine.types.LengineString;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

public class LengineClassLoader {
    private static final Map<String, LengineObject> alreadyLoadedClass = new HashMap<>();

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
                ClassLoader cl = Thread.currentThread().getContextClassLoader();
                Class<?> cls = cl.loadClass(className);

                if (cls.isAssignableFrom(LengineObject.class)) {
                    throw new RuntimeException("The name " + className + " is not LengineObject");
                }

                Constructor<?> constructor = cls.getConstructor();
                LengineObject lengineObject = (LengineObject) constructor.newInstance();
                lengineObject.scriptMain();
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
