package is.yaks.rest.utils;

import java.util.List;
import javax.ws.rs.core.MultivaluedMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Utils {

    public static final Logger LOG = LoggerFactory.getLogger(Utils.class);
    public static final String IS_YAKS_ACCESS = "is.yaks.access";
    public static final String IS_YAKS_STORAGE = "is.yaks.storage";

    private Utils() {
        // nothing to do, just hides the implicit public one
    }

    public static String getHeader(MultivaluedMap<String, String> headers, String cookie) {
        List<String> setCookie = headers.get(cookie);
        if (setCookie != null && !setCookie.isEmpty()) {
            String value = setCookie.get(0);
            if (value != null && !value.isEmpty()) {
                return value;
            } else {
                fail("No value in cookie");
            }
        } else {
            fail("No cookie");
        }

        return null;
    }

    public static String getValueFromHeaderKey(MultivaluedMap<String, String> headers, String cookie, String field) {
        List<String> cookieList = headers.get(cookie);
        if (cookieList != null && !cookieList.isEmpty()) {
            String firstCookie = cookieList.get(0);
            if (firstCookie != null && !firstCookie.isEmpty()) {
                String[] splittedString = firstCookie.split("=");
                if (splittedString[0].equals(field)) {
                    return splittedString[1];
                } else {
                    fail("Invalid field in cookie: " + splittedString[0] + " != " + field);
                }
            } else {
                fail("No value in cookie: " + firstCookie);
            }
        } else {
            fail("No cookie: " + headers);
        }

        return null;
    }

    public static void fail(String string) {
        try {
            throw new RuntimeException(string);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String stringify(String text) {
        return "_" + text.replaceAll("\\-", "_");
    }
}
