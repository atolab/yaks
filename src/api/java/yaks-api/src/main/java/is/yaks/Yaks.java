package is.yaks;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Yaks API entrypoint.
 */
public interface Yaks {

    /**
     * Static operation to get an instance of a Yaks implementation.
     * 
     * @param yaksImplName
     *            Name of the class implementing the Yaks API
     * @param classLoader
     *            The Classloader to be used
     * @param args
     *            Options to pass to the Yaks implementation (e.g. credentials)
     * @return
     */

    public Logger logger = LoggerFactory.getLogger(Yaks.class);

    public static Yaks getInstance(String yaksImplName, ClassLoader classLoader, String... args) {
        if (classLoader == null) {
            throw new NullPointerException("No class loader provided");
        }

        if (yaksImplName == null) {
            throw new NullPointerException("No Yaks implementation provided");
        }

        try {
            Class<?> yaks = classLoader.loadClass(yaksImplName);
            Constructor<?> ctor = yaks.getDeclaredConstructor(String[].class);
            ctor.setAccessible(true);
            return (Yaks) ctor.newInstance((Object) args);
        } catch (ClassNotFoundException e) {
            logger.error("{}", e);
        } catch (NoSuchMethodException e) {
            logger.error("{}", e);
        } catch (SecurityException e) {
            logger.error("{}", e);
        } catch (InstantiationException e) {
            logger.error("{}", e);
        } catch (IllegalAccessException e) {
            logger.error("{}", e);
        } catch (IllegalArgumentException e) {
            logger.error("{}", e);
        } catch (InvocationTargetException e) {
            logger.error("{}", e);
        }
        return null;
    }

    /**
     * Creates an Access with a scope path and a cache with the specified size (in bytes). An Access id will be created
     * by Yaks and returned as a cookie.
     * 
     * @param scopePath
     *            The prefix of the accessible key space (keys outside this root won't be accessible)
     * @param cacheSize
     *            The size of the Access cache
     */
    public Access createAccess(Path scopePath, long cacheSize, Encoding encoding);

    /**
     * Creates an Access with the specified id, with a scope path and a cache with the specified size (in bytes). If not
     * already existing, the access will be created by Yaks and the id returned as a cookie. If already existing, the id
     * of the existing Access will be returned
     * 
     * @param id
     *            The Access identifier
     * @param scopePath
     *            The prefix of the accessible key space (keys outside this root won't be accessible)
     * @param cacheSize
     *            The size of the Access cache
     */
    public Access createAccess(String id, Path scopePath, long cacheSize, Encoding encoding);

    /**
     * Returns list of existing Access identifiers.
     * 
     */
    public List<String> getAccess();

    /***
     * Returns the information details about the Access with identifier id (if exists)
     * 
     * @param id
     * @return
     */
    public Access getAccess(String id);

    /**
     * Creates an Storage with a scope path and some options for its creation. A Storage id will be created by Yaks and
     * returned as a cookie.
     */
    public Storage createStorage(Path path, Properties option);

    /**
     * Creates an Storage with the specified id, a scope path and some options for its creation. If not already
     * existing, the Storage will be created by Yaks and the id returned as a cookie. If already existing, the id of the
     * existing Storage will be returned
     * 
     * @param id
     *            The Storage identifier
     * @param path
     *            The prefix of the key space managed by the Storage.
     * @param option
     *            Options to pass to the Storage implementation (e.g. back-end configuration)
     */
    public Storage createStorage(String id, Path path, Properties option);

    /**
     * Returns list of existing Storage identifiers.
     * 
     * @return future list of id in string format
     */
    public List<String> getStorages();

    /**
     * Returns the information details about the Storage with identifier id (if exists)
     * 
     * @return future storage corresponding to the specified id
     */
    public Storage getStorage(String id);
}
