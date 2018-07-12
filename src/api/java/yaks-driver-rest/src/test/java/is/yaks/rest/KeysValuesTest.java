package is.yaks.rest;

import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import is.yaks.Access;
import is.yaks.Selector;
import is.yaks.Yaks;
import is.yaks.rest.foo.Foo;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class KeysValuesTest {

	Yaks yaks;
	public static final Logger LOG = LoggerFactory.getLogger(KeysValuesTest.class);
	
	@Before
	public void init() {
		String[] args = {"http://localhost:8000"};		
		yaks = Yaks.getInstance("is.yaks.rest.YaksImpl", KeysValuesTest.class.getClassLoader(), args);
		Assert.assertTrue(yaks instanceof YaksImpl);		
	}
	
	//@Test
	public void accessPutStringTest() throws InterruptedException, ExecutionException {
		Access access = new AccessImpl("access-id-2", "//residence-1/house-id-10", 10000L);
		String value = "{\"name\":\"door-kitchen-1\"}";		
		Future<Access> futurePut = access.put(Selector.path("//residence-1/house-id-10"), value);
		Access put = futurePut.get();
		Assert.assertNotNull(put);
	}

	//@Test
	public void accessPutObjectTest() throws InterruptedException, ExecutionException {		
		Access access = new AccessImpl("access-id-2", "//residence-1/house-id-10", 10000L);		
		Future<Access> futurePut = access.put(Selector.path("//residence-1/house-id-10"), new Foo());
		Access put = futurePut.get();
		Assert.assertNotNull(put);
	}

	//@Test
	public void accessDeltaPutTest() throws InterruptedException, ExecutionException {
		Access access = new AccessImpl("access-id-2", "//residence-1/house-id-10", 10000L);		
		String value = "{\"name\":\"door-room\"}";
		Future<Access> futureDeltaPut = access.deltaPut(Selector.path("//residence-1/house-id-10"), value);
		Access deltaPut = futureDeltaPut.get();
		Assert.assertNotNull(deltaPut);
	}
	
	//@Test
	public void accessRemoveWithSelectorTest() throws InterruptedException, ExecutionException {
		Access access = new AccessImpl("accessid2", "//residence-1/house-id-10", 10000L);
		Future<Access> futureAccess = access.remove(Selector.path("//residence-1/house-id-10"));
		access = futureAccess.get();
		Assert.assertNotNull(access);
	}
	
	//@Test
	public void accessGetTest() throws InterruptedException, ExecutionException {
		Future<Access> futureaccess = yaks.getAccess("access-id-2");
		Access access = futureaccess.get();
		Future<Map<String, byte[]>> futureGet = access.get(Selector.path("//residence-1/house-id-10"));
		Map<String, byte[]> map = futureGet.get();
		Assert.assertNotNull(map);
	}
	
	//@Test
	public void accessGetWithClassTest() throws InterruptedException, ExecutionException {
		Future<Access> futureaccess = yaks.getAccess("access-id-2");
		Access access = futureaccess.get();
		Future<Map<String, Foo>> futureGet = access.get(Selector.path("//residence-1/house-id-10"), Foo.class);
		Map<String, Foo> map = futureGet.get();		
		map.forEach( (key,obj)->{
			System.out.println(key + "+++" + obj.getClass().getName());
		});
	}
	
	@After
	public void stop() {
		yaks = null;
	}
}