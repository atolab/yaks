package is.yaks.rest.async;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.Selector;


import is.yaks.async.Access;
import is.yaks.async.Yaks;
import is.yaks.rest.async.YaksImpl;

public class AccessTest {

	Yaks yaks;
	public static final Logger LOG = LoggerFactory.getLogger(AccessTest.class);

	@Before
	public void init() {
		String[] args = {"http://localhost:8000"};		
		yaks = Yaks.getInstance("is.yaks.rest.async.YaksImpl", AccessTest.class.getClassLoader(), args);
		Assert.assertTrue(yaks instanceof YaksImpl);		
	}

	//@Test
	public void yaksCreateAccessTest() throws InterruptedException, ExecutionException {		
		CompletableFuture<Access> futureAccess = yaks.createAccess(Path.ofString("//residence-1/house-access-1"), 100000L, Encoding.JSON);
		Access access = futureAccess.get();
		Assert.assertNotNull(access);
	}

	//@Test
	public void yaksCreateAccessTestWithId() throws InterruptedException, ExecutionException {
		CompletableFuture<Access>  futureAccess = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		Access access = futureAccess.get();
		Assert.assertNotNull(access);
	}

	//@Test
	public void yaksGetAccessTest() throws InterruptedException, ExecutionException {
		CompletableFuture<List<String>> futureListAccess = yaks.getAccess();
		List<String> listAccess = futureListAccess.get();		
		Assert.assertTrue(listAccess.size()>0);
	}

	//@Test
	public void yaksGetAccessByIdTest() throws InterruptedException, ExecutionException {
		CompletableFuture<Access> futureAccess = yaks.getAccess("access-id-1");
		Access access = futureAccess.get();
		Assert.assertNotNull(access);
	}

	//@Test
	public void yaksDisposeTest() throws InterruptedException, ExecutionException {				
		CompletableFuture<Access> futureAccess = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		Access access = futureAccess.get();
		access.dispose();
	}

	//@Test
	public void yaksSubscribeTest() throws InterruptedException, ExecutionException {
		CompletableFuture<Access> futureAccess = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		Access access = futureAccess.get();
		CompletableFuture<Long> subId = access.subscribe(Selector.ofString("//residence-1/house10"));		
		Assert.assertTrue(subId.get()>0);
	}

	//@Test
	public void yaksUnsubscribeTest() throws InterruptedException, ExecutionException {
		CompletableFuture<Access> futureAccess = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		Access access = futureAccess.get();
		access.unsubscribe(511512);
	}

	//@Test
	public void yaksGetSubscriptionsTest() throws InterruptedException, ExecutionException {
		CompletableFuture<Access> futureAccess = yaks.createAccess("access-id-1", Path.ofString("//residence-1/house10"), 100000L, Encoding.JSON);
		Access access = futureAccess.get();
		CompletableFuture<Map<String, Selector>> futureSubs = access.getSubscriptions();
		Map<String, Selector> subs = futureSubs.get();
		Assert.assertNotNull(subs);
		Assert.assertTrue(subs.size()>0);		
	}


	@After
	public void stop() {
		yaks = null;
	}

}