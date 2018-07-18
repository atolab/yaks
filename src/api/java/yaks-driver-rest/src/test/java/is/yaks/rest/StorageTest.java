package is.yaks.rest;

import java.util.Properties;
import java.util.concurrent.ExecutionException;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.Path;
import is.yaks.Storage;
import is.yaks.Yaks;
import is.yaks.rest.YaksImpl;

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
		Storage storage = yaks.createStorage(Path.ofString("//residence-1/storage-1"), options);
		Assert.assertNotNull(storage);
	}


	//@Test
	public void yaksCreateStorageWithId() throws InterruptedException, ExecutionException {
		Properties options = new Properties();
		options.put("options1", "OPT1");
		options.put("options2", "OPT2");
		options.put("options3", "OPT3");
		Storage storage = yaks.createStorage("storage-id-1", Path.ofString("//residence-1/storage-1"), options);		
		Assert.assertNotNull(storage);
	}


	//@Test
	public void storagesDisposeTest() throws InterruptedException, ExecutionException {
		Properties options = new Properties();
		options.put("options1", "OPT1");
		options.put("options2", "OPT2");
		options.put("options3", "OPT3");
		Storage storage = yaks.createStorage("storage-id-1", Path.ofString("//residence-1/storage-1"), options);
		storage.dispose();
	}


	@After
	public void stop() {
		yaks = null;
	}
}
