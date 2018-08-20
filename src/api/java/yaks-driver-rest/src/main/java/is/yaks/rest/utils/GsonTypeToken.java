package is.yaks.rest.utils;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.google.gson.reflect.TypeToken;

public class GsonTypeToken {

    private Map<String, Type> objectTT = new HashMap<String, Type>();
    private Map<String, Type> collectionTT = new HashMap<String, Type>();

    private GsonTypeToken() {
    }

    private static class GsonTypeTokenHolder {
        private final static GsonTypeToken instance = new GsonTypeToken();
    }

    public static GsonTypeToken getInstance() {
        return GsonTypeTokenHolder.instance;
    }

    public <T> Type getTypeToken(Class<T> c) {
        Type ret = objectTT.get(c.getCanonicalName());
        if (ret != null) {
            return ret;
        }
        Type value = TypeToken.get(c).getType();
        objectTT.put(c.getCanonicalName(), value);
        return value;
    }

    public <T> Type getCollectionTypeToken(Class<T> c) {
        Type ret = collectionTT.get(c.getCanonicalName());
        if (ret != null) {
            return ret;
        }
        Type value = new TypeToken<Collection<T>>() {
        }.getType();
        collectionTT.put(c.getCanonicalName(), value);
        return value;
    }

    public <P, T> Type getMapTypeToken() {
        return new TypeToken<Map<P, T>>() {
        }.getType();
    }
}
