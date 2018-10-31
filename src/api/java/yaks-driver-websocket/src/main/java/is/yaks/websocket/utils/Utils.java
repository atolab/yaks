package is.yaks.websocket.utils;

import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;

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
    
	public static int generate_correlation_id()
	{		
		int min = 0;
		int max = 1024;
		Random random = new Random();
		return random.nextInt((max - min) + 1) + min;
	}
	
	public static ByteBuffer mapToByteBuffer(Map<String, String> map) 
	{  
		ByteBuffer buffer = ByteBuffer.allocate(512);
		buffer.put((byte)map.size()); // put the size of the properties map
		
		Iterator<Map.Entry<String, String>> entries = map.entrySet().iterator();
		while(entries.hasNext()) 
		{
		    Map.Entry<String, String> entry = entries.next();
		    buffer.put((byte) entry.getKey().length()); 
		    buffer.put(entry.getKey().getBytes()); 
		    buffer.put((byte) entry.getValue().length());
		    buffer.put(entry.getValue().getBytes());
		}	
		buffer.flip();
		return buffer;  
	}  

	public static Map<String, String> byteBufferToMap(ByteBuffer buffer) 
	{  
	/*	Map<String, String> map = new HashMap<String, String>();  

		String[] nameValuePairs = input.split("&");  
		for (String nameValuePair : nameValuePairs) {  
			String[] nameValue = nameValuePair.split("=");  
			try {  
				map.put(URLDecoder.decode(nameValue[0], "UTF-8"), nameValue.length > 1 ? URLDecoder.decode(  
						nameValue[1], "UTF-8") : "");  
			} catch (UnsupportedEncodingException e) {  
				throw new RuntimeException("This method requires UTF-8 encoding support", e);  
			}  
		}  */

		return null;  
	} 
	
}
