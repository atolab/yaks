package is.yaks;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;

import is.yaks.utils.Utils;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

@SuppressWarnings("restriction")
public final class Selector extends Utils implements Comparable<Selector>
{
	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="path", alternate={"selector", "location"})
	public String path;
	
	public Selector() {} //used when reading data [from REST]
		
	public Selector(String path) {
		this.path = path;
	}
	
    public boolean isPrefix(String prefix, String path){
        assert prefix != null;
        assert path != null;        
        return path.startsWith(prefix);
    }
	
	public static Selector path(String path) {		
		return new Selector(path);
	}
	
	public static Selector pathSelector(String path) {
		// TODO
		throw new NotImplementedException();
	}
	
	public static Selector selector(String string) {
		// TODO
		throw new NotImplementedException();
	}

	@Override
	public int compareTo(Selector o) {
		// TODO
		throw new NotImplementedException();
	}
	
	@Override
	public String toString() {	
		return "selector: " + path;
	}



}