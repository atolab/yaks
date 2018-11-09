package is.yaks.websocket.messages;

import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Map;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

import is.yaks.Path;
import is.yaks.socketfe.EntityType;
import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;
import is.yaks.websocket.utils.Utils;
import is.yaks.websocket.utils.VLEEncoder;

public class MessageCreate implements Message {

	private byte[] body = { 0x00 };
	private ByteBuffer data;
	private byte[] raw_msg = { 0x00, 0x01, 0x02, 0x03, 0x04};
	//1VLE max 64bit
	private VLEEncoder encoder;
	//1VLE max 64bit
	private int correlation_id;
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
	private int vle_length = 1024;
	private int encoding;
	private int message_code;
	private Map<String, String> dataList;	
	private Map<String, String> propertiesList;
	
	private Path path;
	
	public MessageCreate() {
		super();
		// TODO Auto-generated constructor stub
	}

	public MessageCreate(EntityType type, Path path, String id, int cache_size, String config, boolean complete) 
	{
		this.propertiesList =  new HashMap<String, String>();
		JSONObject json = new JSONObject();
		this.message_code = MessageCode.CREATE.getValue();
		this.correlation_id = Utils.generate_correlation_id();
		this.path = path;
		
		if(type.equals(EntityType.ACCESS)) 
		{
			setFlag_a();			
			if(!id.equals("None"))
			{
				add_property("is.yaks.access.alias", id);
			}
			if(cache_size != 0)
			{
				add_property("is.yaks.access.cachesize", String.valueOf(cache_size));
			} 
		} 
		else if (type.equals(EntityType.STORAGE)) 
		{
			setFlag_s();
			if(!id.equals("None")) 
			{
				add_property("is.yaks.storage.alias", id);
			}
			if(!config.equals("None")) 
			{
				json = (JSONObject) JSONValue.parse(config);
				add_property("is.yaks.storage.config", json.toString());
			}
			if(complete) {			
				add_property("is.yaks.storage.complete", "true");
			}
		}
	}

	@Override
	public ByteBuffer write() {
		// TODO Auto-generated method stub
		
		System.out.println("Inside MessageCreate::write() method.");
		
		ByteBuffer buffer =  ByteBuffer.allocate(vle_length);

		//set header		
		buffer.put((byte) 0x00); // i keep space for the vle
		buffer.put((byte) this.message_code); //0x02 CREATE		
		buffer.put((byte) this.flags);        //0x02
		buffer.put((byte) this.correlation_id); // correlation_id in vle random 
		
		if (this.propertiesList.size() > 0) 
		{
			buffer.put(Utils.mapToByteBuffer(this.propertiesList));
		}
		//
		buffer.put((byte)this.path.toString().length());
		buffer.put((this.path.toString()).getBytes());
		
		
		// adding the vle length of the msg
		buffer.flip();
		int limit = buffer.limit();
		buffer.put(0, (byte) (limit-1));
        
		return buffer;
		
		
	}

	@Override
	public Message read(ByteBuffer bytes) {
		// TODO Auto-generated method stub
		return null;
	}
	
	public void setFlag_a() 
	{
		this.flag_a = 1;
		this.flags = (int) (this.flags | 0x04);
	}
	
	public void setFlag_s() 
	{
		this.flag_s = 1;
		this.flags = (int) (this.flags | 0x02);
	}
	
	public void setFlag_p() 
	{
		this.flag_p = 1;
		this.flags = (int) (this.flags | 0x01);
	}
	
	public void add_property(String key, String value) {
		setFlag_p();
		this.propertiesList.put(key, value);
	}

}
