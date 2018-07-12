package is.yaks.rest;

import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.Storage;
import is.yaks.Yaks;

public class StorageTest {
	Yaks yaks;
	public static final Logger LOG = LoggerFactory.getLogger(AccessTest.class);

	@Before
	public void init() {
		String[] args = {"http://localhost:8000"};		
		yaks = Yaks.getInstance("is.yaks.rest.YaksImpl", AccessTest.class.getClassLoader(), args);
		Assert.assertTrue(yaks instanceof YaksImpl);		
	}


	//@Test
	public void yaksCreateStorage() throws InterruptedException, ExecutionException {
		Properties options = new Properties();
		options.put("options1", "OPT1");
		options.put("options2", "OPT2");		
		Future<Storage> futureStorage = yaks.createStorage("//residence-1/storage-1", options);
		Storage storage = futureStorage.get();
		Assert.assertNotNull(storage);
	}
	
	
	//@Test
	public void yaksCreateStorageWithId() throws InterruptedException, ExecutionException {
		Properties options = new Properties();
		options.put("options1", "OPT1");
		options.put("options2", "OPT2");
		options.put("options3", "OPT3");
		Future<Storage> futureStorage = yaks.createStorage("storage-id-1", "//residence-1/storage-1", options);
		Storage storage = futureStorage.get();
		Assert.assertNotNull(storage);
	}


	//@Test
	public void storagesDisposeTest() throws InterruptedException, ExecutionException {				
		Storage storage = new StorageImpl("storage-id-1", "//residence-1/storage-1");
		storage.dispose();
	}


	@After
	public void stop() {
		yaks = null;
	}
}
