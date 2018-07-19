package is.yaks.rest.async;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;

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
import is.yaks.rest.foo.Foo;

public class AsyncTest {

	private Yaks yaks;
	private CompletableFuture<Access> futureAccess;
	public static final Logger LOG = LoggerFactory.getLogger(AsyncTest.class);
	
	@Before
	public void init() {
		String[] args = {"http://localhost:8000"};		
		yaks = Yaks.getInstance("is.yaks.rest.async.YaksImpl", AsyncTest.class.getClassLoader(), args);		
		Assert.assertTrue(yaks instanceof YaksImpl);
	}

	//@Test
	public void basicTest() throws InterruptedException, ExecutionException {
		futureAccess = yaks.createAccess(Path.ofString("//residence-1/house-id-10"), 10000L, Encoding.JSON);
		Assert.assertNotNull(futureAccess);
		
		Access access = futureAccess.get();
		Assert.assertNotNull(access);
		
		Access finalAccess = access.put(Selector.ofString("//residence-1/house-id-10"), "{\"name\":\"door-room-1\"}")
		.thenApply((Access a)->{ return a.deltaPut(Selector.ofString("//residence-1/house-id-10"), new Foo());}).get()
		.thenApply((Access a)->{ a.dispose(); return a;}).get();
		
		Assert.assertNotNull(finalAccess);
		
		CompletableFuture<Void> x = finalAccess.dispose();
		x.get();		
	}
	
	
	@After
	public void stop() {		
		yaks = null;
	}
}