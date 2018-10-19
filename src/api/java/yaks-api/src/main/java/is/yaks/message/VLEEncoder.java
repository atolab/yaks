package is.yaks.message;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.List;

public class VLEEncoder 

{
	
	private static VLEEncoder instance;
	
	/** 0x80 => 128 dec => 0b10000000 */
	public static final int  MORE_BYTES_FLAG = 0x80;
	
	/** 0x7F	=> 0b1111111 */
	public static final byte BYTE_MASK 		 = 0x7F;
	
	/** 7 => 0b111 */
	public static final byte SHIFT_LEN 		 = 7;
	
	/** 64 => 0b1000000 */
	public static final byte MAX_BITS 		 = 64;
	
	/** 10 =>  0b1010 */
	public static final byte MAX_BYTES		 = 10;

	public int length = 0x0;
	
	public VLEEncoder()	{}
	
	public static synchronized VLEEncoder getInstance() 
	{
		if(instance ==null) {
			instance = new VLEEncoder();
		}
		return instance;
	} 
	
	public int getLength() 
	{
		return length;
	}
	
	public byte[] encode(int value) 
	{		
		byte[] buff = new byte[]{};
		buff = actual_encoding(value, buff);
		this.length = buff.length;
		return buff;		
	}
	
	public byte[] actual_encoding(int value, byte[] buff) 
	{	
		ByteBuffer dbuf = ByteBuffer.allocate(1);		
		
		if( value <= VLEEncoder.BYTE_MASK)
		{			
			dbuf.order(ByteOrder.BIG_ENDIAN);
			buff = dbuf.putInt(value).array();
		} 
		else 
		{
             int mv = VLEEncoder.MORE_BYTES_FLAG | (value & VLEEncoder.BYTE_MASK);
             dbuf.order(ByteOrder.BIG_ENDIAN);              
             buff = dbuf.putInt(mv).array();
             actual_encoding((value >> VLEEncoder.SHIFT_LEN), buff);
		}
		return buff;
	}
	
	public int decode(byte[] buff) 
	{				
		int value = 0;
		
		ByteBuffer wrapped = ByteBuffer.wrap(buff).order(ByteOrder.BIG_ENDIAN);
		
		for(int i =0; i < buff.length;) 
		{		     
			int v = wrapped.getInt();
		     
		    value = value | (v << (8 * i));	
		}
		
		return actual_decoding(0, 0, buff);
	}
	
	public int actual_decoding(int v, int n, byte[] buff) 
	{	
		if (n < VLEEncoder.MAX_BYTES) 
		{
			byte[] newBuff = { 0x00 }; 
			
			int c = (new BigInteger(buff)).intValue();
			
			for (int i =1 ;  i < buff.length; i++) 
			{
				newBuff[i] = buff[i]; 
			}
						
			if( c <= VLEEncoder.BYTE_MASK) 
			{
				return merge(v, c, n);
			} 
			else 
			{
				return actual_decoding(merge(v, masked(c), n), n + 1, newBuff);
			}
		}
		return actual_decoding(0, 0, buff);
	}
	
	private int merge(int v, int charValue, int n) 
	{
		 return v | (charValue << (n * VLEEncoder.SHIFT_LEN));
	}
	
	private int masked(int c) 
	{
		 return VLEEncoder.BYTE_MASK & c;
	}

}
