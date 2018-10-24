package is.yaks.websocket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import org.apache.logging.log4j.core.net.DatagramOutputStream;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import is.yaks.Yaks;
import is.yaks.websocket.utils.MessageImpl;
import junit.framework.Assert;

public class YaksSocketClient 
{
	
	Yaks yaks;	
	public static final Logger LOG = LoggerFactory.getLogger(AccessTest.class);
    private MessageImpl msgType = MessageImpl.getInstance();
	
//    @Before
//    public void init() {
//        String[] args = { "http://localhost:7887" };
//        yaks = Yaks.getInstance("is.yaks.websocket.YaksImpl", AccessTest.class.getClassLoader(), args);
//        Assert.assertTrue(yaks instanceof YaksImpl);
//    }
    
	public static void main(String[] args) throws IOException 
	{
		
		
        
		Socket s = new Socket();
		String host = "127.0.0.1";
		PrintWriter s_out = null;
		BufferedReader s_in = null;
		
		
//		private YaksConfiguration config = YaksConfiguration.getInstance();
		
		DatagramOutputStream data_out = null;

		try 
		{
//			Yaks y = new YaksSocketClient();
			s.connect(new InetSocketAddress(host, 7887));
			System.out.println("0. Connected!");

			//writer for socket
			s_out = new PrintWriter(s.getOutputStream(), true);

			//Reader for socket
			s_in = new BufferedReader(new InputStreamReader(s.getInputStream()));
			
		} 
		// Host not found exception
		catch(UnknownHostException e) 
		{
			System.err.println("Don't know about host: "+host);
			System.exit(1);
		}

		String message = "GET / HTTP/1.1\r\n\r\n";
		s_out.println(message);

		System.out.println("2. Message send!");

		//Get response from server
		String response;

		System.out.println("3. Get Response!");
		while((response = s_in.readLine()) != null) 
		{
			System.out.println(response);
		}
		
		//closing the i/o streams
		s_out.close();
		s_in.close();
		
		//close the socket
		s.close();
	}
}
