package is.yaks.rest;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.HashMap;
import com.google.gson.reflect.TypeToken;

public class GsonTypeToken {

	public final Type ACCESS_DATA = new TypeToken<AccessImpl>(){}.getType();
	
	public final Type COLLECTION_ACCESS = new TypeToken<Collection<AccessImpl>>(){}.getType();
	public final Type COLLECTION_ACCESS_ID = new TypeToken<Collection<String>>(){}.getType();
	
	public final Type MAP_KV = new TypeToken<HashMap<String, ?>>() {}.getType();
	public final Type MAP_SELECTOR_BY_SUBS = new TypeToken<HashMap<String, String>>() {}.getType();

	private GsonTypeToken() 
	{		
	}

    private static class GsonTypeTokenHolder
    {
        private final static GsonTypeToken instance = new GsonTypeToken();
    }

    public static GsonTypeToken getInstance()
    {
        return GsonTypeTokenHolder.instance;
    }
}
