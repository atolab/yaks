package is.yaks.websocket.messages;

import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.Map;

import is.yaks.socketfe.Message;
import is.yaks.websocket.utils.VLEEncoder;

public class MessageNotify implements Message {
	
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
	

	public MessageNotify() {
		super();
		// TODO Auto-generated constructor stub
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
