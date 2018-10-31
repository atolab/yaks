package is.yaks.websocket.messages;

import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.Map;

import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;
import is.yaks.websocket.utils.VLEEncoder;

public class MessageOk implements Message {
	
	
	//1byte
	private int flags;

	private int message_code;
	
	//1VLE max 64bit
	private int correlation_id;

	
	public MessageOk() {
		super();
		this.message_code = MessageCode.OK.getValue();
		// TODO Auto-generated constructor stub
	}
	
	public MessageOk(int corr_id) 
	{
		this.message_code = MessageCode.OK.getValue();
		this.correlation_id = corr_id;
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
	
	public int getCorrelation_id() {
		return correlation_id;
	}

	public void setCorrelation_id(int correlation_id) {
		this.correlation_id = correlation_id;
	}

	public int getFlags() {
		return flags;
	}

	public void setFlags(int flags) {
		this.flags = flags;
	}
	
	public int getMessage_code() {
		return message_code;
	}

	public void setMessage_code(int message_code) {
		this.message_code = message_code;
	}

}
