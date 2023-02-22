package lengine.runtime;

import lengine.Prelude;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class LengineClassLoader {
    private static final Map<String, Class<?>> alreadyLoadedClass = new HashMap<>();

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
                Class<?> cls = Prelude.class.getClassLoader().loadClass(className);
                Optional<Method> foundMethod = Arrays.stream(cls.getMethods())
                        .filter(x -> x.getName().equals("main")).findFirst();
                if (!foundMethod.isPresent()) {
                    throw new RuntimeException("Unable to find method main!");
                } else {
                    Method mainMethod = foundMethod.get();
                    mainMethod.invoke(null, new Object[]{new String[]{}});
                }
                alreadyLoadedClass.put(className, cls);
            }

            Class<?> cls = alreadyLoadedClass.get(className);

            Optional<Method> foundMethodOptional = Arrays.stream(cls.getMethods())
                    .filter(x -> x.getName().equals("importSymbol")).findFirst();
            if (!foundMethodOptional.isPresent()) {
                throw new RuntimeException("Not a lengine class.");
            } else {
                Method importSymbol = foundMethodOptional.get();
                return importSymbol.invoke(null, LengineString.create(symbolName));
            }
        } catch (InvocationTargetException e) {
            if (e.getCause() instanceof RuntimeException) {
                throw (RuntimeException) e.getCause();
            } else {
                throw new RuntimeException(e);
            }
        } catch (ClassNotFoundException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }
}
