package is.yaks;

import java.util.Map;

public final class Selector implements Comparable<Selector> {

    private String path;

    private Selector(String s) throws IllegalArgumentException {
        if (s == null) {
            throw new IllegalArgumentException("Provided selector string is null");
        }
        validateSelectorPath(s);
        this.path = s;
    }

    private void validateSelectorPath(String s) throws IllegalArgumentException {
        // TODO: validate the selector string
    }

    public static Selector ofString(String string) {
        return new Selector(string);
    }

    @Override
    public String toString() {
        return path;
    }

    public Map<String, String> getQuery() {
        /// TODO
        return null;
    }

    public boolean isPrefix(String prefix, String path) {
        if (prefix == null || path == null) {
            throw new NullPointerException("Can't check with isPrefix, one of parameters is null");
        }
        return path.startsWith(prefix);
    }

    @Override
    public int compareTo(Selector o) {
        return this.path.compareTo(o.path);
    }

}