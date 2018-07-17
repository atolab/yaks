package is.yaks.rest.utils;

import java.lang.reflect.Type;
import java.util.Collection;
import com.google.gson.reflect.TypeToken;

import is.yaks.rest.async.AccessImpl;

public class GsonTypeToken {

	public final Type ACCESS = new TypeToken<AccessImpl>(){}.getType();
	public final Type COLLECTION_ID = new TypeToken<Collection<String>>(){}.getType();	

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
