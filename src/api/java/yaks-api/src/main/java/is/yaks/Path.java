package is.yaks;

import java.util.Map;

public class Path implements Comparable<Path>
{
	private String path;

	private Path(String p) {
		assert p != null;
		validateSelectorPath(p);
		this.path = p;
	}

	private void validateSelectorPath(String p) throws IllegalArgumentException
	{
		// TODO: validate the path string
	}

	public static Path ofString(String string) {
		return new Path(string);
	}

	@Override
	public String toString() { 
		return path;
	}

	public Map<String, String> getQuery()
	{
		/// TODO
		return null;
	}


	public boolean isPrefix(String prefix, String path) {
		assert prefix != null;
		assert path != null;        
		return path.startsWith(prefix);
	}


	@Override
	public int compareTo(Path o) {
		return this.path.compareTo(o.path);
	}


}
