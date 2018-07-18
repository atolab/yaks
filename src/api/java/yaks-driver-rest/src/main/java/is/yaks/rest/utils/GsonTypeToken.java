package is.yaks.rest.utils;

import java.lang.reflect.Type;
import java.util.Collection;
import com.google.gson.reflect.TypeToken;

public class GsonTypeToken {

	public final Type ACCESS = new TypeToken<is.yaks.rest.AccessImpl>(){}.getType();
	public final Type STRING_COLLECTION = new TypeToken<Collection<String>>(){}.getType();	

	private GsonTypeToken(){}

    private static class GsonTypeTokenHolder
    {
        private final static GsonTypeToken instance = new GsonTypeToken();
    }

    public static GsonTypeToken getInstance()
    {
        return GsonTypeTokenHolder.instance;
    }
}
