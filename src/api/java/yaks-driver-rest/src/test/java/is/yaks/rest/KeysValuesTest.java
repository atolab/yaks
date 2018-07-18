package is.yaks.rest;

import java.util.Map;
import java.util.concurrent.ExecutionException;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.Selector;
import is.yaks.Yaks;
import is.yaks.rest.foo.Foo;

public class KeysValuesTest {

	private Yaks yaks;
	private Access access;
	public static final Logger LOG = LoggerFactory.getLogger(KeysValuesTest.class);
	
	@Before
	public void init() {
		String[] args = {"http://localhost:8000"};		
		yaks = Yaks.getInstance("is.yaks.rest.YaksImpl", KeysValuesTest.class.getClassLoader(), args);
		Assert.assertTrue(yaks instanceof YaksImpl);
		access = yaks.createAccess(Path.ofString("//residence-1/house-id-10"), 10000L, Encoding.JSON);
		Assert.assertTrue(access instanceof AccessImpl);
	}
	
	//@Test
	public void accessPutStringTest() throws InterruptedException, ExecutionException {
		String value = "{\"name\":\"door-kitchen-1\"}";		
		Access put = access.put(Selector.ofString("//residence-1/house-id-10"), value);		
		Assert.assertNotNull(put);
	}

	//@Test
	public void accessPutObjectTest() throws InterruptedException, ExecutionException {
		Access put = access.put(Selector.ofString("//residence-1/house-id-10"), new Foo());		
		Assert.assertNotNull(put);
	}

	//@Test
	public void accessDeltaPutTest() throws InterruptedException, ExecutionException {		
		String value = "{\"name\":\"door-room\"}";
		Access deltaPut = access.deltaPut(Selector.ofString("//residence-1/house-id-10"), value);		
		Assert.assertNotNull(deltaPut);
	}
	
	//@Test
	public void accessRemoveWithSelectorTest() throws InterruptedException, ExecutionException {		
		Access rem = access.remove(Selector.ofString("//residence-1/house-id-10"));		
		Assert.assertNotNull(rem);
	}
	
	//@Test
	public void accessGetTest() throws InterruptedException, ExecutionException {				
		Foo get = access.get(Path.ofString("//residence-1/house-id-10"), Foo.class);		
		Assert.assertNotNull(get);
	}
	
	//@Test
	public void accessGetWithClassTest() throws InterruptedException, ExecutionException {
		Access getAccess = yaks.getAccess("access-id-2");		
		Map<Path, Foo> map = getAccess.get(Selector.ofString("//residence-1/house-id-10"), Foo.class);				
		map.forEach( (key,obj)->{
			System.out.println(key + "+++" + obj.getClass().getName());
		});
	}
	
	@After
	public void stop() {
		access.dispose();
		yaks = null;
	}
}