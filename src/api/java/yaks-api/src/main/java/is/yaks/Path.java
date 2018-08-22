package is.yaks;

import java.util.Map;

public class Path implements Comparable<Path> {

    private String pathValue;

    private Path(String p) {
        if (p == null) {
            throw new NullPointerException("The ginven path is null");
        }
        validateSelectorPath(p);
        this.pathValue = p;
    }

    private void validateSelectorPath(String p) throws IllegalArgumentException {
        // TODO: validate the path string
    }

    public static Path ofString(String string) {
        return new Path(string);
    }

    @Override
    public String toString() {
        return pathValue;
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
    public int compareTo(Path o) {
        return o.pathValue.compareTo(this.pathValue);
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof Path) {
            return ((Path) object).pathValue.equals(this.pathValue);
        }
        return this == object || this.pathValue.equals(object);
    }

    @Override
    public int hashCode() {
        return this.pathValue.hashCode();
    }

}
