package is.yaks.rest.qualif;

import java.util.Properties;
import java.util.concurrent.ExecutionException;

import org.junit.Before;
import org.junit.Test;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.Selector;
import is.yaks.Storage;
import is.yaks.Yaks;
import junit.framework.Assert;

public class BasicTests {
    private Yaks yaks;
    private Access access;
    private Storage storage;

    @Before
    public void init() throws InterruptedException, ExecutionException {
        // Instantiate a Yaks Java API using a socket to connect the Yaks instance on localhost:8000
        this.yaks = Yaks.getInstance("is.yaks.rest.YaksImpl", BasicTests.class.getClassLoader(),
                "http://localhost:8000");
        this.access = yaks.createAccess("MyAccess", Path.ofString("//is.yaks.tests"), 1024, Encoding.JSON);
        this.storage = yaks.createStorage("MM-store", Path.ofString("//is.yaks.tests"), new Properties());
    }

    @Test
    public void putGet() {
        try {
            access.put(Selector.ofString("//is.yaks.tests/a"), "ABC");
            String v = access.get(Path.ofString("//is.yaks.tests/a"), String.class);
            System.out.println(v);
            Assert.assertEquals("ABC", v);
        } catch (Throwable e) {
            e.printStackTrace();
            Assert.fail();
        }
    }

}
