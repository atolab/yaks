package is.yaks.socketfe;

public enum MessageCode 
{
	NONE(0x0),
	
	OPEN(0x01),
	CREATE(0x02),
	DELETE(0x03),
	PUT(0xA0),
	PATCH(0xA1),
	GET(0xA2),
	SUB(0xB0),
	UNSUB(0xB1),
	NOTIFY(0xB2),
	EVAL(0xB3),
	OK(0xD0),
	VALUE(0xD1),
	VALUES(0xD2),
	ERROR(0xE0);
	
	private int value;    

	private MessageCode(int value) {
		this.value = value;
	}

	public int getValue() {
		return value;
	}
}
