package is.yaks.websocket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.Storage;
import is.yaks.Yaks;
import is.yaks.socketfe.EntityType;
import is.yaks.socketfe.Message;
import is.yaks.socketfe.MessageCode;
import is.yaks.websocket.messages.MessageCreate;
import is.yaks.websocket.messages.MessageFactory;
import is.yaks.websocket.messages.MessageGet;
import is.yaks.websocket.messages.MessageOk;
import is.yaks.websocket.messages.MessagePut;
import is.yaks.websocket.messages.MessageSub;
import is.yaks.websocket.qualif.BasicTests;
import is.yaks.websocket.utils.Utils;

public class YaksSocketClient 
{
    private static Yaks yaks;
    private static Access access;
    private static Storage storage;
    
	private static Socket socket = new Socket();
	private static String host = "localhost";
	private static int port = 7887;
	private static PrintWriter s_out = null;
	private static BufferedReader s_in = null;
    
	public static final Logger LOG = LoggerFactory.getLogger(AccessTest.class);
    
   
	public static void main(String[] args)  throws IOException, InterruptedException
	{
		MessageOk msgOK;
		MessageFactory messageFactory = new MessageFactory();
		InetSocketAddress inetAddr = new InetSocketAddress(host, port);
		
		ByteBuffer buffer = ByteBuffer.allocate(1024);		
		SocketChannel client = SocketChannel.open(inetAddr);	
		
		try		
		{
			socket.connect(new InetSocketAddress(host, 7887));		
			System.out.println("0. Connected to Yaks Server!");
			
			//1. Open the Connection
			System.out.println("1. Send OPEN message !");
			Message msgOpen = messageFactory.getMessage(MessageCode.OPEN);
			client.write(msgOpen.write());					
			
			// wait for 2 seconds before sending next message
			Thread.sleep(2000);
		
			// 2. Create STORAGE 			
			System.out.println("2. Send CREATE message !");
			Message msgCreateStorage = new MessageCreate(EntityType.STORAGE, 
					Path.ofString("//test"), "None", 1024, "None", false);
			client.write(msgCreateStorage.write());
			
			// wait for 2 seconds before sending next message
			Thread.sleep(2000);

//			client.read(buffer);
//			int msg_code = (int)buffer.get(1); // position 0 is the vle
//			if(msg_code == MessageCode.OK.getValue()) {
//				msgOK = new MessageOk(); 
//				msgOK.setCorrelation_id((int)buffer.get());
//				msgOK.setFlags((int)buffer.get());
//				System.out.println("response STORAGE=" + msgOK.getMessage_code());				
//			}	
//			buffer.clear();
			

			//3. Create ACCESS
			System.out.println("3. Send ACCESS message !");
			Message msgCreateAccess = new MessageCreate(EntityType.ACCESS, 
					Path.ofString("//test"), "None", 1024, "None", false);			
			client.write(msgCreateAccess.write());

//			client.read(buffer);
//			int msg_code = (int)buffer.get(1); // position 0 is the vle
//			if(msg_code == MessageCode.OK.getValue()) {
//				msgOK = new MessageOk(); 
//				msgOK.setCorrelation_id((int)buffer.get());
//				msgOK.setFlags((int)buffer.get());
//				
//				System.out.println("response ACCESS=" + msgOK.getMessage_code());				
//			}	
//			buffer.clear();
			
			
			//4. Create SUB message. Recover the 'is.yaks.access.id' from the Access created in step 3.
			Message msgSub = new MessageSub("is.yaks.access.id","key");
	
			
			//5. PUT TUPLE
			System.out.println("3. Send PUT TUPLE !");
			Message msgPut = new MessagePut("is.yaks.access.id", "key", "value");
			
			
			
			
			//6. GET TUPLE
			System.out.println("3. Send GET TUPLE !");
			Message msgGet = new MessageGet("is.yaks.access.id", "key");
			
			
			
		} 
		// Host not found exception
		catch(UnknownHostException e) 
		{
			System.err.println("Don't know about host: "+host);
			System.exit(1);
		}
		
		//close the socketChannel
		client.close();
		
		//close the socket
		socket.close();
	}
}
