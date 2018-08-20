package is.yaks;

public interface Listener<T> {

    public void onData(Path key, T value);

}
