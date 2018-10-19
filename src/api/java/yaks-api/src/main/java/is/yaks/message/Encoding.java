package is.yaks.message;

public enum Encoding 
{
	RAW(0x01),
	STRING(0x02),
	JSON(0x03),
	PROTOBUF(0x04),
	SQL(0x05),
	INVALID(0x00);
	
	private int value;    
	
	private Encoding(int value) {
		this.value = value;
	}
	
	public int getValue() {
		return value;
	}
}
