package is.yaks.rest;

import java.util.List;
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
import is.yaks.rest.AccessImpl;
import is.yaks.rest.YaksImpl;

public class AccessTest {

	Yaks yaks;
	public static final Logger LOG = LoggerFactory.getLogger(AccessTest.class);

	@Before
	public void init() {
		String[] args = {"http://localhost:8000"};		
		yaks = Yaks.getInstance("is.yaks.rest.YaksImpl", AccessTest.class.getClassLoader(), args);
		Assert.assertTrue(yaks instanceof YaksImpl);		
	}

	//@Test
	public void yaksCreateAccessTest() throws InterruptedException, ExecutionException {		
		Access access = yaks.createAccess(Path.ofString("//residence-1/house-access-1"), 100000L, Encoding.JSON);
		Assert.assertNotNull(access);
	}

	//@Test
	public void yaksCreateAccessTestWithId() throws InterruptedException, ExecutionException {
		Access access = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		Assert.assertNotNull(access);
	}

	//@Test
	public void yaksGetAccessTest() throws InterruptedException, ExecutionException {
		List<String> listAccess = yaks.getAccess();				
		Assert.assertTrue(listAccess.size()>0);
	}

	//@Test
	public void yaksGetAccessByIdTest() throws InterruptedException, ExecutionException {
		Access access = yaks.getAccess("access-id-1");
		Assert.assertNotNull(access);
	}

	//@Test
	public void yaksDisposeTest() throws InterruptedException, ExecutionException {				
		Access access = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		access.dispose();
	}

	//@Test
	public void yaksSubscribeTest() throws InterruptedException, ExecutionException {
		Access access = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		Long subId = access.subscribe(Selector.ofString("//residence-1/house10"));		
		Assert.assertTrue(subId>0);
	}

	//@Test
	public void yaksUnsubscribeTest() throws InterruptedException, ExecutionException {
		Access access = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		access.unsubscribe(511512);
	}

	//@Test
	public void yaksGetSubscriptionsTest() throws InterruptedException, ExecutionException {
		AccessImpl access = new AccessImpl("access-id-1", Path.ofString("//residence-1/house10"), 10000L);		
		Map<String, Selector> subs = access.getSubscriptions();
		Assert.assertNotNull(subs);
		Assert.assertTrue(subs.size()>0);		
	}


	@After
	public void stop() {
		yaks = null;
	}

}