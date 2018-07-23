package is.yaks.rest;

import java.util.List;
import java.util.Map;
import java.util.Properties;
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
import is.yaks.Storage;
import is.yaks.Yaks;
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


	@Test
	public void BasicTest()
	{
		//create access
		Access  access1 = yaks.createAccess("access-1", Path.ofString("//is.yaks.tests"), 1024, Encoding.JSON);			
		Storage storage1 = yaks.createStorage("MM-store1", Path.ofString("//is.yaks.tests"), new Properties());	
		Assert.assertNotNull(storage1);
		
		//put
		access1.put(Selector.ofString("//is.yaks.tests/a"), "ABC");
		
		//get value from key
		String v = access1.get(Path.ofString("//is.yaks.tests/a"), String.class);		
		Assert.assertEquals("ABC", v);
		
		
		//get access from access-id
		access1 = yaks.getAccess("access-1");
		Assert.assertNotNull(access1);

		//TODO activate
		//Long subId = access1.subscribe(Selector.ofString("//is.yaks.tests"));
		//Assert.assertTrue(subId>0);
		
		//create access
		Access  access2 = yaks.createAccess("access-2", Path.ofString("//is.yaks.tests-2"), 1024, Encoding.JSON);
		Assert.assertNotNull(access2);
		Storage storage2 = yaks.createStorage("MM-store2", Path.ofString("//is.yaks.tests-2"), new Properties());	
		Assert.assertNotNull(storage2);
		
		
		
		Access  access3 = yaks.createAccess("access-3", Path.ofString("//is.yaks.tests-3"), 1024, Encoding.JSON);
		Assert.assertNotNull(access3);
		Storage storage3 = yaks.createStorage("MM-store3", Path.ofString("//is.yaks.tests-3"), new Properties());	
		Assert.assertNotNull(storage3);
		
		//TODO activate
		//List<String> listAccess = yaks.getAccess();
		//Assert.assertNotNull(listAccess);
		//Assert.assertTrue(listAccess.size()>0);
		
		//dispose
		//access1.unsubscribe(subId);
		storage1.dispose();
		storage2.dispose();
		storage3.dispose();
		
		access1.dispose();
		access2.dispose();
		access3.dispose();
	}

	@After
	public void stop() {
		yaks = null;
	}

}