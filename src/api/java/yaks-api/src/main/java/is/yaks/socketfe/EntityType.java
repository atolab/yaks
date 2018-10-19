package is.yaks.socketfe;

public enum EntityType 
{
	ACCESS(0), 
	STORAGE(1);
	
	private int value;    

	private EntityType(int value) {
		this.value = value;
	}

	public int getValue() {
		return value;
	}
}
