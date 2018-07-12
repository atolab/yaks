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
		Future<Access> futureHouseId10 = yaks.getAccess("access-id-2");
		Access houseId10 = futureHouseId10.get();
		String value = "{\"name\":\"door-kitchen-1\"}";
		houseId10.put(Selector.path("//residence-1/house-id-10"), value);		
		Future<Access> futurePut = houseId10.put(Selector.path("//residence-1/house-id-10"), value);
		Access put = futurePut.get();
		Assert.assertNotNull(put);
	}


	@Test
	public void accessPutObjectTest() throws InterruptedException, ExecutionException {
		Future<Access> futureHouseId10 = yaks.getAccess("access-id-2");
		Access houseId10 = futureHouseId10.get();
		
		Foo foo = new Foo();
		houseId10.put(Selector.path("//residence-1/house-id-10"), foo);
		
		Future<Access> futurePut = houseId10.put(Selector.path("//residence-1/house-id-10"), foo);
		Access put = futurePut.get();
		Assert.assertNotNull(put);
	}

	//@Test
	public void accessDeltaPutTest() throws InterruptedException, ExecutionException {
		Future<Access> futureHouseId10 = yaks.getAccess("access-id-2");
		Access houseId10 = futureHouseId10.get();		
		String value = "{\"name\":\"door-room\"}";
		Future<Access> futureDeltaPut = houseId10.deltaPut(Selector.path("//residence-1/house-id-10"), value);
		Access deltaPut = futureDeltaPut.get();
		Assert.assertNotNull(deltaPut);
	}
	
	//@Test
	public void accessGetTest() throws InterruptedException, ExecutionException {
		Future<Access> futureHouseId10 = yaks.getAccess("access-id-2");
		Access houseId10 = futureHouseId10.get();
		Future<Map<String, byte[]>> futureGet = houseId10.get(Selector.path("//residence-1/house-id-10"));
		Map<String, byte[]> map = futureGet.get();
		Assert.assertNotNull(map);
	}
	
	//@Test
	public void accessGetWithClassTest() throws InterruptedException, ExecutionException {
		Future<Access> futureHouseId10 = yaks.getAccess("access-id-2");
		Access houseId10 = futureHouseId10.get();
		Future<Map<String, Foo>> futureGet = houseId10.get(Selector.path("//residence-1/house-id-10"), Foo.class);
		Map<String, Foo> map = futureGet.get();		
		map.forEach( (key,obj)->{
			System.out.println(key + "+++" + obj.getClass().getName());
		});
	}
	
	@After
	public void stop() {}
}