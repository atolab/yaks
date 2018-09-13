package is.yaks.rest.async;

import java.util.Map;
import java.util.Properties;
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
import is.yaks.async.Storage;
import is.yaks.async.Yaks;
import is.yaks.rest.foo.Foo;

public class AccessTest {

    Yaks yaks;
    public static final Logger LOG = LoggerFactory.getLogger(AccessTest.class);

    @Before
    public void init() {
        String[] args = { "http://localhost:8000" };
        yaks = Yaks.getInstance("is.yaks.rest.async.YaksImpl", AccessTest.class.getClassLoader(), args);
        Assert.assertTrue(yaks instanceof YaksImpl);
    }

    @Test
    public void BasicAsyncTest() throws InterruptedException, ExecutionException {
        // create access
        CompletableFuture<Access> fAccess1 = yaks.createAccess(Path.ofString("//is.yaks.tests.async"), 100000L,
                Encoding.JSON);
        Access access1 = fAccess1.get();
        Assert.assertNotNull(access1);

        CompletableFuture<Access> fAccess2 = yaks.createAccess("access-async-1", Path.ofString("//is.yaks.tests.async"),
                100000L, Encoding.JSON);
        Access access = fAccess2.get();
        Assert.assertNotNull(access);

        /*
         * TODO to activate CompletableFuture<List<String>> futureListAccess = yaks.getAccess(); List<String> listAccess
         * = futureListAccess.get(); Assert.assertTrue(listAccess.size()>0);
         */

        // test put/get
        String a = access1.put(Selector.ofString("//is.yaks.tests.async/a"), "ABC").thenApply((Access asyncAccess) -> {
            return asyncAccess.get(Path.ofString("//is.yaks.tests.async/a"), String.class);
        }).get().get();
        Assert.assertEquals("ABC", a);

        // test put/get
        Foo b = access1.put(Selector.ofString("//is.yaks.tests.async/b"), new Foo()).thenApply((Access asyncAccess) -> {
            return asyncAccess.get(Path.ofString("//is.yaks.tests.async/b"), Foo.class);
        }).get().get();
        Assert.assertNotNull(b);

        // get access with id
        CompletableFuture<Access> futureAccess = yaks.getAccess("access-async-1");
        Access assyncAccess = futureAccess.get();
        Assert.assertNotNull(assyncAccess);

        access1.dispose().get();
        assyncAccess.dispose().get();

    }

    // @Test
    public void yaksSubscribeTest() throws InterruptedException, ExecutionException {
        CompletableFuture<Access> futureAccess = yaks.createAccess("access-async-2",
                Path.ofString("//is.yaks.tests.async-2"), 100000L, Encoding.JSON);
        Access access = futureAccess.get();
        Assert.assertNotNull(access);

        CompletableFuture<Long> fSubId = access.subscribe(Selector.ofString("//is.yaks.tests.async-2"));
        Long subId = null;
        Assert.assertTrue((subId = fSubId.get()) > 0);

        CompletableFuture<Map<String, Selector>> futureSubs = access.getSubscriptions();
        Map<String, Selector> subs = futureSubs.get();
        Assert.assertNotNull(subs);
        Assert.assertTrue(subs.size() > 0);

        access = futureAccess.get();
        access.unsubscribe(subId);
        access.dispose();
    }

    @After
    public void stop() {
        yaks = null;
    }
}
