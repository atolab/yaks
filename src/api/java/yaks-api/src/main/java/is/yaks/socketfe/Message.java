package is.yaks.socketfe;

import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

public interface Message 
{
	/**
	 * Write Message into a Buffer 
	 * 
	 * @return ByteBuffer
	 */
	public ByteBuffer write();	

	/**
	 * Read data from the buffer into a Message
	 * 
	 * @param raw_msg
	 * @return Message
	 */
	public Message read(ByteBuffer bytes);
	
//    /**
//     * Returns value of VLEEncoder set in the header
//     * 
//     * @return vleEncoder
//     */
//    public int read_vle_field(byte[] raw_msg, int base_p);
//
//    /**
//     * Returns the encoding set in the header
//     * @param buf
//     * @param base_p
//     * @return Encoding
//     */
//    public int read_encoding(byte[] raw_msg, int base_p);
//    
//    
//    /**
//     * Return the list of all the message's properties. 
//     * If P = 1 the message include properties
//     * 
//     * @return all message properties
//     */
//    public Map<String, String> read_all_properties();
//    
//    /**
//     * Return a single property indexed by key. 
//     * 
//     * @return a property
//     */
//    public is.yaks.socketfe.Property read_property_by_key(Map<String, String> properties, String key);
//	
//    /**
//     * Reads the value of Message Code set in the header 
//     * 
//     * OPEN	    0x01
//     * CREATE	0x02
//     * DELETE	0x03
//     * PUT	    0xA0
//     * PATCH	0xA1
//     * GET 	    0XA2
//     * SUB	    0xB0
//     * UNSUB	0xB1
//     * NOTIFY	0xB2
//     * EVAL	    0xB3
//     * OK	    0XD0
//     * VALUE	0xD1
//     * VALUES	0xD2
//     * ERROR	0xE0
//
//     * @return ByteBuffer
//     */
//    public int read_message_code();
//
//    /**
//     * read the value of the Message flags set in the header 
//     * 
//     * PROPERTY         0x01 
//     * STORAGE          0x02
//     * ACCESS           0x04 
//     * ENCODING         0x38
//     * ENCODING_RAW     0x08
//     * ENCODING_JSON    0x10
//     * ENCODING_PROTO   0x18
//     * 
//     * @return message flags
//     */    
//    public int read_flags();
//
//    public int read_flag_a();
//    
//    public int read_flag_s();
//    
//    public int read_flag_p();
//    
//    
//    /**
//     * Read the value of Correction Id (Vle.t)
//     * 
//     * @return Correction Id
//     */
//    public int  read_correction_id();

}

