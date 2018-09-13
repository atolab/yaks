package is.yaks.rest;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.junit.After;
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
import is.yaks.rest.foo.Foo;
import is.yaks.rest.utils.GsonTypeToken;
import is.yaks.rest.utils.YaksConfiguration;
import junit.framework.Assert;

public class AccessTest {

    Yaks yaks;
    public static final Logger LOG = LoggerFactory.getLogger(AccessTest.class);
    private GsonTypeToken gsonTypes = GsonTypeToken.getInstance();

    @Before
    public void init() {
        String[] args = { "http://localhost:8000" };
        yaks = Yaks.getInstance("is.yaks.rest.YaksImpl", AccessTest.class.getClassLoader(), args);
        Assert.assertTrue(yaks instanceof YaksImpl);
    }

    @Test
    public void debugTest() {
        YaksConfiguration config = YaksConfiguration.getInstance();
        Map<Path, String> m = new HashMap<>();
        m.put(Path.ofString("//is.yaks.tests/a"), "ABC");
        m.put(Path.ofString("//is.yaks.tests/x"), "XYZ");
        String json = config.getGson().toJson(m);

        Map<Path, String> result = new HashMap<>();
        result = config.getGson().fromJson(json, gsonTypes.<Path, String> getMapTypeToken());
        Assert.assertNotNull(result);
        Assert.assertTrue(result.size() > 0);
        Assert.assertTrue(result.get(Path.ofString("//is.yaks.tests/a")).equals("ABC"));
    }

    @Test
    public void BasicTest() {
        // create access
        Access access1 = yaks.createAccess("access-1", Path.ofString("//is.yaks.tests"), 1024, Encoding.JSON);
        Assert.assertNotNull(access1);

        // put
        access1.put(Selector.ofString("//is.yaks.tests/a"), "ABC");

        // get value from key
        String v = access1.get(Path.ofString("//is.yaks.tests/a"), String.class);
        Assert.assertEquals("ABC", v);

        // get access from access-id
        access1 = yaks.getAccess("access-1");
        Assert.assertNotNull(access1);

        // TODO activate
        // Long subId = access1.subscribe(Selector.ofString("//is.yaks.tests/"));
        // Assert.assertTrue(subId>0);

        // create access
        Access access2 = yaks.createAccess("access-2", Path.ofString("//is.yaks.tests-2"), 1024, Encoding.JSON);
        Assert.assertNotNull(access2);

        /*
         * TODO to solve/to activate String data = response.getEntity(String.class); //in get returns
         * {"//is.yaks.tests-2/a":"DEF","//is.yaks.tests-2/a":"DEF"}
         * 
         * String value = "DEF"; Access deltaPut = access2.deltaPut(Selector.ofString("//is.yaks.tests-2/a"), value);
         * Assert.assertNotNull(deltaPut); String deltaPutStr = access2.get(Path.ofString("//is.yaks.tests-2/a"),
         * String.class); System.out.println(deltaPutStr);
         */

        // put/get on object
        Access fooAccess = access1.put(Selector.ofString("//is.yaks.tests/foo"), new Foo());
        Assert.assertNotNull(fooAccess);
        Foo foo = access1.get(Path.ofString("//is.yaks.tests/foo"), Foo.class);
        Assert.assertTrue(foo instanceof Foo);
        System.out.println(foo.toString());

        Access access3 = yaks.createAccess("access-3", Path.ofString("//is.yaks.tests-3"), 1024, Encoding.JSON);
        Assert.assertNotNull(access3);

        // TODO activate
        // List<String> listAccess = yaks.getAccess();
        // Assert.assertNotNull(listAccess);
        // Assert.assertTrue(listAccess.size()>0);

        Access rem = access3.remove(Selector.ofString("//is.yaks.tests-3"));
        Assert.assertNotNull(rem);

        // dispose
        // access1.unsubscribe(subId);

        access1.dispose();
        access2.dispose();
        access3.dispose();
    }

    @After
    public void stop() {
        yaks = null;
    }

}