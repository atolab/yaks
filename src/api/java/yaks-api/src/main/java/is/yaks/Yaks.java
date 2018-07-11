package is.yaks;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.Future;

/**
 * Yaks API entrypoint.
 */
public interface Yaks {

   /**
    * Static operation to get an instance of a Yaks implementation.
    * 
    * @param yaksImplName Name of the class implementing the Yaks API
    * @param classLoader The Classloader to be used
    * @param args Options to pass to the Yaks implementation (e.g. credentials)
    * @return
    */
	public static Yaks getInstance(String yaksImplName, ClassLoader classLoader, String... args)
	{
		assert classLoader != null;
		assert yaksImplName != null;
		try {			
			Class<?> yaks = classLoader.loadClass(yaksImplName);			
			Constructor<?> ctor = yaks.getConstructor(String[].class);
			return (Yaks) ctor.newInstance((Object) args);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		}
		return null;
	}


   /**
	* Creates an Access with a scope path and a cache with the specified size (in bytes). 
	* An Access id will be created by Yaks and returned as a cookie.
    * 
    * @param scopePath The prefix of the accessible key space (keys outside this root won't be accessible)
    * @param cacheSize The size of the Access cache
    */   
   public Future<Access> createAccess (String scopePath, long cacheSize, Encoding encoding);

   
   /**
    * Creates an Access with the specified id, with a scope path and a cache with the specified size (in bytes). 
    * If not already existing, the access will be created by Yaks and the id returned as a cookie. 
    * If already existing, the id of the existing Access will be returned
    * 
    * @param id The Access identifier
    * @param scopePath The prefix of the accessible key space (keys outside this root won't be accessible)
    * @param cacheSize The size of the Access cache
    */
   public Future<Access> createAccess (String id, String scopePath, long cacheSize, Encoding encoding);

   /**
    * Returns list of existing Access identifiers.
    * 
    */
   public Future<List<String>> getAccess();
   

   /***
    * Returns the information details about the Access with identifier id (if exists)
    * 
    * @param id
    * @return
    */
   public Future<Access> getAccess(String id);

   
   /**
    * Creates a Storage in the global key/value store.
    * 
    * @param id The Storage identifier
    * @param path The prefix of the key space managed by the Storage.
    * @param option Options to pass to the Storage implementation (e.g. back-end configuration)
    */
   public Storage createStorage(String id, Selector path, Properties option);
   
   /**
    * Finds an existing Storage with a specified identifier.
    * Null is returned if not found.
    */
   public Storage resolveStorage(String id);

   
}


