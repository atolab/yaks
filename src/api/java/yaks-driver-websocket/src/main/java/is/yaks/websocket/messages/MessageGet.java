package is.yaks.websocket.messages;

import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Map;

import is.yaks.socketfe.Encoding;
import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;
import is.yaks.websocket.utils.Utils;
import is.yaks.websocket.utils.VLEEncoder;

public class MessageGet implements Message {
	
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
	

	public MessageGet() {
		super();
		// TODO Auto-generated constructor stub
	}

	public MessageGet(String id, String key) 
	{
		this.propertiesList = new HashMap<String, String>();
		this.message_code = MessageCode.GET.getValue();
		this.encoding = Encoding.RAW.getValue();
		this.correction_id = Utils.generate_correlation_id();
		this.propertiesList.put("'is.yaks.access.id'", id);
		this.data.put(key.getBytes());
	}
	
	@Override
	public ByteBuffer write() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Message read(ByteBuffer bytes) {
		// TODO Auto-generated method stub
		return null;
	}

}
