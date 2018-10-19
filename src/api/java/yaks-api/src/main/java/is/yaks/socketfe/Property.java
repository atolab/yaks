package is.yaks.socketfe;

public class Property 
{	
	String key;
	String value;
	
	/** Get Property*/
	public Property(String key, String value) {
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
