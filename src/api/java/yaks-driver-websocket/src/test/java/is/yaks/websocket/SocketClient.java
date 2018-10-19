package is.yaks.websocket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import org.apache.logging.log4j.core.net.DatagramOutputStream;

public class SocketClient 
{

	public static void main(String[] args) throws IOException 
	{
		Socket s = new Socket();
//		String host = "www.google.com";
		String host = "localhost";
		PrintWriter s_out = null;
		BufferedReader s_in = null;
		
		DatagramOutputStream data_out = null;

		try 
		{
//			s.connect(new InetSocketAddress(host, 80));
			s.connect(new InetSocketAddress(host, 5000));
			System.out.println("1. Connected!");

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
