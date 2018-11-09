package is.yaks.websocket.messages;

import java.nio.ByteBuffer;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;

import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Map;

import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;
import is.yaks.websocket.utils.Utils;
import is.yaks.websocket.utils.VLEEncoder;

public class MessageOpen implements Message {
	
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
	
	private String username;
	private String password;

	
	
	public MessageOpen() {
		// TODO Auto-generated constructor stub
		super();

	}


	public MessageOpen(String username, String password) {
		
		this.propertiesList =  new HashMap<String, String>();		
		this.correction_id = Utils.generate_correlation_id();
		this.message_code = MessageCode.OPEN.getValue();
		if(!username.equals("None") && !password.equals("None")) 
		{
			add_property("yaks.login", "'"+username+":"+password+"'");
		}
		else 
		{
			add_property("yaks.login", "None:None");	
		}
		return;
	}

	@Override
	public ByteBuffer write() 
	{
		System.out.println("Inside MessageOpen::write() method.");

		ByteBuffer buffer =  ByteBuffer.allocate(4);

		//set header		
		buffer.put((byte) 0x00); // i keep space for the vle
		buffer.put((byte) MessageCode.OPEN.getValue());
		buffer.put((byte) this.flags);
		buffer.put((byte) this.correction_id);
		
		// adding the vle length
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
