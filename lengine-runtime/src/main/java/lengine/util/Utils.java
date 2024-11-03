package lengine.util;

public class Utils {
    @SuppressWarnings("unchecked")
    public static <T> T UNSAFE_cast(Object object) {
        return (T) object;
    }
}
