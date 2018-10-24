package is.yaks.websocket.utils;

public class Data 
{	
	String key;
	String value;
	
	/** Get Data*/
	public Data(String key, String value) {
		this.key = key;
		this.value = value;
	} 
	
	/** Get Key*/
	public String getKey() {
		return this.key;
	}
	
	/**
	 * Set key
	 * @param key
	 */
	public void setKey(String key) {
		this.key = key;
	}
	
	/** Get value*/
	public String getValue() {
		return this.value;
	}
	
	/**
	 * Set value
	 * @param value
	 */
	public void setValue(String value) {
		this.value = value;
	}

}
