package is.yaks.rest.utils;

import java.util.List;
import java.util.UUID;

import javax.ws.rs.core.MultivaluedMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.rest.GsonTypeToken;
import is.yaks.rest.YaksConfiguration;


public class Utils {

	public String uniqueID = Utils.stringify(UUID.randomUUID().toString());
	public static final Logger LOG = LoggerFactory.getLogger(Utils.class);
	protected GsonTypeToken<?> gsonTypes = GsonTypeToken.getInstance();
	protected YaksConfiguration config = YaksConfiguration.getInstance();
	
	public String getUuid() {
		return uniqueID;
	}
	
	public String getCookie(MultivaluedMap<String, String> headers, String cookie) {
		List<String> setCookie = headers.get(cookie);
		if(setCookie != null && !setCookie.isEmpty()) {
			String value = setCookie.get(0);
			if(value != null && !value.isEmpty()) {
				return value;
			} else {
				fail("No value in cookie");
			}
		} else {
			fail("No cookie");
		}
		
		return null;
	}
	

	public String getCookieData(MultivaluedMap<String, String> headers, String cookie, String field) {
		List<String> cookieList = headers.get(cookie);		
		if(cookieList != null && !cookieList.isEmpty()) {
			String firstParam = cookieList.get(0);			
			if(firstParam != null && !firstParam.isEmpty()) {				
				String[] splittedString = firstParam.split("=");
				if(splittedString[0].equals(field)) {
					return splittedString[1];
				} else {
					fail("Invalid field in cookie: " + splittedString[0] + " != "+ field);
				}
			} else {
				fail("No value in cookie");
			}
		} else {
			fail("No cookie");
		}
		
		return null;
	}
	
	public void fail(String string) {
		try {
			throw new Exception(string);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static String stringify(String text) {
		return "_" + text.replaceAll("\\-", "_");
	}
}
