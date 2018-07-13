package is.yaks.rest;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Selector;
import is.yaks.Yaks;

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
		Future<Access> futureAccess = yaks.createAccess("//residence-1/house-access-1", 100000L, Encoding.JSON);
		Assert.assertNotNull(futureAccess);		
		Access access = futureAccess.get();
		if(access instanceof AccessImpl) {
			AccessImpl accessImpl = (AccessImpl)access;			
			Assert.assertNotNull(accessImpl.getAccessId());			
		}
	}

	//@Test
	public void yaksCreateAccessTestWithId() throws InterruptedException, ExecutionException {
		Future<Access> futureAccess = yaks.createAccess("access-id-1", "//residence-1/house10", 100000L, Encoding.JSON);
		Access access = futureAccess.get();
		if(access instanceof AccessImpl) {
			AccessImpl accessImpl = (AccessImpl)access;
			Assert.assertEquals("Bad id", "access-id-1", accessImpl.getAccessId());
			Assert.assertEquals("Bad cache", 100000L, accessImpl.getCacheSize());
		}
	}

	//@Test
	public void yaksGetAccessTest() throws InterruptedException, ExecutionException {
		Future<List<String>> futureListAccess = yaks.getAccess();
		List<String> listAccess = futureListAccess.get();		
		Assert.assertTrue(listAccess.size()>0);		
		System.out.println(listAccess);
	}


	//@Test
	public void yaksGetAccessByIdTest() throws InterruptedException, ExecutionException {
		Future<Access> futureAccess = yaks.getAccess("access-id-1");
		Access access = futureAccess.get();
		Assert.assertNotNull(access);		
		if(access instanceof AccessImpl) {
			System.out.println(access);
		}
	}


	//@Test
	public void yaksDisposeTest() throws InterruptedException, ExecutionException {				
		AccessImpl access = new AccessImpl("access-id-1", "//residence-1/house10", 10000L);
		access.dispose();
	}


	//@Test
	public void yaksSubscribeTest() throws InterruptedException, ExecutionException {
		AccessImpl access = new AccessImpl("access-id-1", "//residence-1/house10", 10000L);
		Future<Long> futureSubId = access.subscribe(Selector.path("//residence-1/house10"));
		Long subId = futureSubId.get();
		Assert.assertTrue(subId>0);
	}
	
	
	//@Test
	public void yaksUnsubscribeTest() throws InterruptedException, ExecutionException {
		AccessImpl access = new AccessImpl("access-id-1", "//residence-1/house10", 10000L);
		access.unsubscribe(511512);
	}


	//@Test
	public void yaksGetSubscriptionsTest() throws InterruptedException, ExecutionException {
		AccessImpl access = new AccessImpl("access-id-1", "//residence-1/house10", 10000L);		
		Future<Map<String, Selector>> futureSubs = access.getSubscriptions();		
		Map<String, Selector> subs = futureSubs.get();
		System.out.println(subs);		
	}


	@After
	public void stop() {
		yaks = null;
	}

}