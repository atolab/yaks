package is.yaks.websocket.messages;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

import is.yaks.socketfe.Encoding;
import is.yaks.socketfe.EntityType;
import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;
import is.yaks.socketfe.Property;
import is.yaks.websocket.utils.Data;
import is.yaks.websocket.utils.VLEEncoder;

/**

MESSAGE FORMAT:
LITTLE ENDIAN

7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+ ----------------------+
|  MESSAGE CODE |    8bit               |
+-+-+-+-+-+-+-+-+                       |
|X|X|E|N|C|A|S|P|    8bit               +--> Header
+-+-+-+-+-+-+-+-+                       |
~   Corr. ID    ~  VLE max 64bit        |
+---------------+                       |
~   Properties  ~ --> Present if P = 1  |
+---------------+ ----------------------+
~     Body      ~ --> its structure depends on the message code
+---------------+

Where the flags A, S and P have the following meaning:
ENC = → The message encoding {RAW, STRING, JSON, PROTOBUF, SQL, INVALID}
A   = 1 → The message concerns an Access
S   = 1 →The message concerns a Storage
P   = 1 → The message includes properties


WIRE MESSAGE:

 7 6 5 4 3 2 1 0
+-+-+-+-+-+-+-+-+
~    Length     ~ VLE max 64bit
+-+-+-+-+-+-+-+-+ ----------------------+
|  MESSAGE CODE |    8bit               |
+-+-+-+-+-+-+-+-+                       |
|X|X|E|N|C|A|S|P|    8bit               +--> Header
+-+-+-+-+-+-+-+-+                       |
~    Corr. id   ~  VLE max 64bit        |
+---------------+                       |
~   Properties  ~ --> Present if P = 1  |
+---------------+ ----------------------+
~     Body      ~ VL
+---------------+

Message Codes: 
---------------
OPEN 	= 0x01;
CREATE 	= 0x02;
DELETE 	= 0x03;
PUT 	= 0xA0;
PATCH 	= 0xA1;
GET 	= 0xA2;
SUB 	= 0xB0;
UNSUB 	= 0xB1;
NOTIFY 	= 0xB2;
EVAL 	= 0xB3;
OK 		= 0xD0;
VALUE 	= 0xD1;
VALUES 	= 0xD2;
ERROR 	= 0xE0;


Encoding: 
---------------
RAW 	 = 0x01;
STRING 	 = 0x02;
JSON 	 = 0x03;
PROTOBUF = 0x04;
SQL 	 = 0x05;
INVALID  = 0x00;

 **/

public class MessageImpl implements Message
{
	
	private static MessageImpl instance = null;
	
	byte[] body = { 0x00 };
	
	ByteBuffer data;

	byte[] raw_msg = { 0x00, 0x01, 0x02, 0x03, 0x04};

	//1VLE max 64bit
	VLEEncoder encoder;
	//1VLE max 64bit
	int correction_id;
	
	int length;

	//1byte
	int flags;

	//1bit
	int flag_a;

	//1bit
	int flag_s;

	//1bit
	int flag_p;

	//vle length max 64 bits
	int vle_length;

	int encoding;

	int message_code;

	Map<String, String> dataList;	
	
	Map<String, String> propertiesList;
	
	MessageImpl() {

		// VLE encoder
		this.encoder = VLEEncoder.getInstance();

		// Correction is VLEEncoder max 64bit
		this.correction_id = 0;
		
		//VLE max 64bit
        int length = 0;

		// 8bit (1byte) comprises A,S,P (0xFF = 0b11111111) 
		this.flags = 0x0;

		// 1bit. Initialized to false. 
		this.flag_a = 0;

		// 1bit. Initialized to false.
		this.flag_s = 0;

		// 1bit. Initialized to false.
		this.flag_p = 0;

		
		dataList = new HashMap<String, String>();

		propertiesList = new HashMap<String, String>();
		
		// 8bit (1byte)
		this.encoding = 0x0;

		this.message_code = MessageCode.NONE.getValue();

		if(this.message_code != MessageCode.NONE.getValue()) 
		{
			this.length = ByteBuffer.wrap(this.raw_msg).capacity();
		}
	}
	
	/**
	 * Get instance of the Message implementation
	 * @return MessageImpl
	 */
	public static MessageImpl getInstance() {
		if( instance == null ) 
		{
			instance = new MessageImpl();
		}
		return instance;
	}

	@Override
	public ByteBuffer write() 
	{
	//	ByteBuffer buff = ByteBuffer.allocate(read_encoding(raw_msg, 0)); 
		
		ByteBuffer buff = ByteBuffer.allocate(48);
		
		buff.put(String.valueOf(read_vle_field(raw_msg, 0)).getBytes());
		
		buff.put(String.valueOf(read_flags()).getBytes());
		
		buff.put(String.valueOf(read_correction_id()).getBytes());
		
		int flag_p = read_flag_p(); 
		
		if(flag_p != 0) 
		{
			Map<String, String> properties = read_all_properties();
			
			for(Map.Entry<String, String> entry : properties.entrySet()) 
			{				
				buff.put(("'key':"+entry.getKey()+",'value':"+entry.getValue()).getBytes());	
			}
		}
		
		return buff;		
	}

	@Override
	public Message read(ByteBuffer bytes) 
	{

		int base_p = 0;		
		MessageImpl msg = new MessageImpl();
		
		msg.setVle_length(read_vle_field(raw_msg, base_p));
		
		msg.setEncoding(read_encoding(raw_msg, base_p));
		
		msg.setFlags(read_flags());
		
		if(msg.getFlag_p() != 0) 
		{
			this.propertiesList = read_all_properties();	
		}	
		return null;
	}
	
	//@Override
	public int read_vle_field(byte[] raw_msg, int base_position) 
	{
		byte[] vle_field = new byte[] {};
		ByteBuffer buf = ByteBuffer.wrap(raw_msg) ; 
		buf.order(ByteOrder.BIG_ENDIAN);
		byte data = (byte)buf.get(base_position);
		vle_field[0] = data;
		
		// while (data & 0x80):
		int highestOneBit = data & 0x80;
		if(highestOneBit != 0) 
		{
			base_position =  base_position + 1;
			buf.order(ByteOrder.BIG_ENDIAN);
			data = (byte)buf.get(base_position);
			vle_field[0] = data;	
		}		
		return this.encoder.decode(vle_field);
	}



	//@Override
	public int read_encoding(byte[] raw_msg, int base_position) 
	{
		  
		int vle_field = read_vle_field(raw_msg, base_position);
		
		if(vle_field > 0) 
		{
			int k_len = read_vle_field(raw_msg, base_position);
			
			base_position = base_position + k_len; 
			
			return  Integer.parseInt(Arrays.copyOfRange(raw_msg, base_position, base_position+1).toString());
		}
		return Encoding.INVALID.getValue();
	}
	
	public void set_encoding(int encoding) throws Exception
	{
		if( encoding > 0xFF) 
		{
			throw new Exception("Encoding not supported");
		}
		this.encoding = encoding;
	}

//	@Override
	public int read_flags() 
	{
		flags = ByteBuffer.wrap(this.raw_msg).get(2);

		this.flag_a = (flags & 0x04) >> 2;
        this.flag_s = (flags & 0x02) >> 1;
        this.flag_p = flags & 0x01;
        int base_position = 2;
        return flags;
	}

	public void setFlag_p() 
	{
		this.flag_p = 1;
		this.flags = (int) (this.flags | 0x01);
	}

	public void unsetFlag_p() 
	{
		this.flag_p = 0;
		this.flags = (int) (this.flags ^ 0x01);
	}

	public void setFlag_s() 
	{
		this.flag_s = 1;
		this.flags = (int) (this.flags | 0x02);
	}

	public void unsetFlag_s() 
	{
		this.flag_s = 0;
		this.flags = (int) (this.flags ^ 0x02);
	}

	public void setFlag_a() 
	{
		this.flag_a = 1;
		this.flags = (int) (this.flags | 0x03);
	}

	public void unsetFlag_a() 
	{
		this.flag_a = 0;
		this.flags = (int) (this.flags ^ 0x03);
	}
	
	// getrandbits(k) -> x. Generates an int with k random bits
	public int generate_correction_id()
	{
		this.length = (int)Math.random();
		return this.length;
	}
	
	public void add_selector(String selector) 
	{
		data.put(selector.getBytes());
	}

	public void add_subscription(String subcriptor_id) 
	{
		data.put(subcriptor_id.getBytes());
	}

	public void add_error(int error_code) 
	{
		data.put(String.valueOf(error_code).getBytes());
	}
	
	public int get_encoding() 
	{
		return this.encoding;
	}

	
	public void add_property(String key, String value) 
	{
        setFlag_p();
        this.propertiesList.put(key, value);
	}
	
	public void add_path(String path) 
	{
       this.data.put(path.getBytes());
	}
	
	public void add_values(String key, String value) 
	{
		this.dataList.put(key, value);
	}
	
	public void add_vle(int number) 
	{
		body = encoder.encode(number).toString().getBytes();
	}

//	@Override
	public Map<String, String> read_all_properties() 
	{
		return null;
	}

//	@Override
	public Property read_property_by_key(Map<String, String> properties, String key) 
	{
		return null;
	}

//	@Override
	public int read_message_code() 
	{

		return 0;
	}


//	@Override
	public int read_flag_a() 
	{
		return this.flag_a;
	}

//	@Override
	public int read_flag_s() 
	{
		return 0;
	}

//	@Override
	public int read_flag_p() 
	{
		return 0;
	}

//	@Override
	public int read_correction_id() {
		return this.correction_id;
	}

	
	public String pprint() 
	{
       String pretty = "\n############ YAKS FE SOCKET MESSAGE ###################" 
                 + "\n# CODE: "+ this.message_code + "\n"
                 + "\n# CORR.ID:"+ this.correction_id + "\n"
                 + "\n# LENGTH: "+ this.length + "\n"
                 + "\n# FLAGS: RAW: " + (byte)this.flags + " | "
                 		+ "A:" + this.flag_a + " S:" + this.flag_s + " P:" + this.flag_p + "'.\n";
            
        if (this.flag_p==1) {
            pretty = pretty + "\n# HAS PROPERTIES\n# NUMBER OF PROPERTIES: "+ this.propertiesList.size();

            for(Map.Entry<String, String> entry : propertiesList.entrySet()) 
            {
                pretty = pretty + "\n#========\n# "
                		+ " KEY: "+entry.getKey()  
                		+ " VALUE: "+entry.getValue();
                    
            }
        }
        pretty = pretty + "\n#========\nDATA:"+this.body.toString()
                 + "\n#######################################################";
                 
        
        return pretty;
	}
	
	
	private void setFlags(int flags) {
		this.flags = flags;
	}

	public int getCorrection_id() {
		return correction_id;
	}

	public void setCorrection_id(int corr_id) {
		this.correction_id = corr_id;
	}

	public int getLength() {
		return length;
	}

	public void setLength(int length) {
		this.length = length;
		
	}

	public int getFlags() {
		return flags;
	}

	public int getFlag_a() {
		return flag_a;
	}


	public int getFlag_s() {
		return flag_s;
	}

	public int getFlag_p() {
		return flag_p;
	}

	public int getVle_length() {
		return vle_length;
	}

	public void setVle_length(int vle_length) {
		this.vle_length = vle_length;
	}

	public int getEncoding() {
		return encoding;
	}

	public void setEncoding(int encoding) {
		this.encoding = encoding;
	}

	public int getMessage_code() {
		return message_code;
	}

	public void set_message_code() 
	{
		this.message_code = ByteBuffer.wrap(this.raw_msg).get(1);
	}

	public VLEEncoder getEncoder() 
	{
		return VLEEncoder.getInstance();
	}

	public void setEncoder(VLEEncoder encoder) 
	{
		this.encoder = encoder;
	}

	public VLEEncoder getVLEEncoder() 
	{
		return VLEEncoder.getInstance();
	}
	
}





