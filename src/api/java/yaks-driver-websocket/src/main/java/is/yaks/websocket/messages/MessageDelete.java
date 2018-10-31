package is.yaks.websocket.messages;

import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.Map;

import is.yaks.Path;
import is.yaks.socketfe.EntityType;
import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;
import is.yaks.websocket.utils.Utils;
import is.yaks.websocket.utils.VLEEncoder;

public class MessageDelete implements Message {
	
	private byte[] body = { 0x00 };
	private ByteBuffer data;
	private byte[] raw_msg = { 0x00, 0x01, 0x02, 0x03, 0x04};
	//1VLE max 64bit
	private VLEEncoder encoder;
	//1VLE max 64bit
	private int correction_id;
	private int length;
	//1byte
	private int flags;
	//1bit
	private int flag_a;
	//1bit
	private int flag_s;
	//1bit
	private int flag_p;
	//vle length max 64 bits
	private int vle_length;
	private int encoding;
	private int message_code;
	private Map<String, String> dataList;	
	private Map<String, String> propertiesList;
	
	private Path path;
	

	public MessageDelete() {
		super();
		// TODO Auto-generated constructor stub
	}
	
	public MessageDelete(String id, EntityType type, Path path) 
	{
		this.message_code = MessageCode.DELETE.getValue();
		this.correction_id = Utils.generate_correlation_id();
		if(type.equals(EntityType.ACCESS)) 
		{
			setFlag_a();
			this.propertiesList.put("'is.yaks.access.id'", id);
		} 
		else if (type.equals(EntityType.STORAGE)) 
		{
			setFlag_s();
			this.propertiesList.put("'is.yaks.storage.id'", id);
		} 
		else if (!path.equals("None")) 
		{
			this.path = path;
			this.propertiesList.put("'is.yaks.access.id'", id);
		}
	}

	@Override
	public ByteBuffer write() {
		// TODO Auto-generated method stub
		
		System.out.println("Inside MessageDelete::write() method.");
		
		return null;
	}

	@Override
	public Message read(ByteBuffer bytes) {
		// TODO Auto-generated method stub
		return null;
	}
	
	public void setFlag_s() 
	{
		this.flag_s = 1;
		this.flags = (int) (this.flags | 0x02);
	}

	
	public void setFlag_a() 
	{
		this.flag_a = 1;
		this.flags = (int) (this.flags | 0x03);
	}
}
