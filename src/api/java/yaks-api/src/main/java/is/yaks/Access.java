package is.yaks;

import java.util.Map;
import java.util.function.Function;

/**
 * An Access to the global Selector/Value store, using V as data type for the values. Notice that a Serializer<V> must
 * have been provided to Yaks at Access creation.
 */
public interface Access {

    /**
     * Set a new value to one or more key. If the selector does not contain wildcard (i.e. it’s a key), the value is set
     * to the key.
     * 
     * If the selector contains wildcards, the value is set to each existing key matching the selector.
     */
    public Access put(Selector selector, Object value);

    /**
     * Updates an existing association between a Selector and a value with a delta (i.e. a part of the value that have
     * to be updated). Notice that if the Selector is not yet associated with a value in the store the delta will be
     * inserted as a full value.
     */
    public Access deltaPut(Selector selector, Object delta);

    /**
     * Removes the Selector from the store, if present.
     */
    public Access remove(Selector selector);

    /**
     * Subscribes to the keys matching with the selector
     * 
     * @return the subscription identifier
     */
    public Long subscribe(Selector selector);

    /**
     * Subscribes to the keys matching with the selector
     * 
     * @return the subscription identifier
     */
    public <T> Long subscribe(Selector selector, Listener<T> listener);

    /***
     * Get list of subscriptions
     * 
     * @return the list of subscriptions id with their associated Selector
     */
    public Map<String, Selector> getSubscriptions();

    /**
     * Removes a previously registered subscription with the specified selector.
     */
    public void unsubscribe(long subscriptionId);

    /**
     * Returns a map of the Selector/values matching the specified Selector.
     */
    public <T> Map<Path, T> get(Selector Selector, Class<T> c);

    /**
     * Returns the Value matching the specified Path (or null if no Value exists for this Path)
     */
    public <T> T get(Path path, Class<T> c);

    /**
     * Inserts a new association between a Selector and a computation function. This computation function will be called
     * for each get(Selector) operation with a Selector that matches the Selector specified in this eval(Selector,f)
     * operation, and the result of this function will be returned in the result of the get operation.
     */
    public void eval(Selector selector, Function<Path, Object> computation);

    /**
     * Close the current Access connection. Notice that it is possible to re-open this connection calling the
     * Yaks.resolve() operation.
     */
    public void close();

    /**
     * Close the current Access connection and dispose the Access. Notice that it is NOT possible to re-open this
     * connection calling the Yaks.resolve() operation.
     */
    public void dispose();

}
