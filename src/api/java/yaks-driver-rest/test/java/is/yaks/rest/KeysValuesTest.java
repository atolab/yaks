package is.yaks.rest;

import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

import is.yaks.Access;
import is.yaks.Selector;
import is.yaks.Yaks;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class KeysValuesTest {

	Yaks yaks;
	public static final Logger LOG = LoggerFactory.getLogger(KeysValuesTest.class);
	
	@SuppressWarnings("unchecked")
	@Before
	public void init() {
		String[] args = {"http://127.0.0.1/json/index.php"};		
		yaks = Yaks.getInstance("is.yaks.rest.YaksImpl", KeysValuesTest.class.getClassLoader(), args);
		Assert.assertTrue(yaks instanceof YaksImpl);		
	}
	
	//@Test
	public void accessPutTest() throws InterruptedException, ExecutionException {
		Future<Access> futureHouseId10 = yaks.getAccess("house-id-10");
		Assert.assertNotNull(futureHouseId10);

		Access houseId10 = futureHouseId10.get();
		Assert.assertNotNull(houseId10);
		
		String value = "{\"name\":\"door-kitchen-1\"}";
		houseId10.put(Selector.path("//residence-1/house-id-10"), value);
		
		Future<Access> futurePut = houseId10.put(Selector.path("//residence-1/house-id-10"), value);
		Access put = futurePut.get();
		Assert.assertNotNull(put);
	}
	
	//@Test
	public void accessDeltaPutTest() throws InterruptedException, ExecutionException {
		Future<Access> futureHouseId10 = yaks.getAccess("house-id-10");
		Assert.assertNotNull(futureHouseId10);

		Access houseId10 = futureHouseId10.get();
		Assert.assertNotNull(houseId10);
		
		String value = "{\"name\":\"door-room\"}";
		Future<Access> futureDeltaPut = houseId10.deltaPut(Selector.path("//residence-1/house-id-10"), value);
		Access deltaPut = futureDeltaPut.get();
		Assert.assertNotNull(deltaPut);
	}
	
	//@Test
	public void accessGetTest() throws InterruptedException, ExecutionException {
		Future<Access> futureHouseId10 = yaks.getAccess("house-id-10");
		Assert.assertNotNull(futureHouseId10);

		Access houseId10 = futureHouseId10.get();
		Assert.assertNotNull(houseId10);

		Future<Map<Selector, byte[]>> futureGet = houseId10.get(Selector.path("//residence-1/house-id-10"));
		Map<Selector, byte[]> map = futureGet.get();
		System.out.println(map);
	}
	
	@After
	public void stop() {				
	}
}