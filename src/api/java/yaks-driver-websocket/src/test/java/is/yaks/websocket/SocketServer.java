package is.yaks.websocket;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;

import org.apache.logging.log4j.core.net.DatagramOutputStream;

public class SocketServer {


	public static void main(String[] args) {

		ServerSocket 	s 	 	= null;
		Socket 			conn 	= null;
		//		PrintStream 	out 	= null;
		//		BufferedReader  in 		= null;
		//		String 			message = null;

		try 
		{
			//1. Create a server socket. First parameter is port number
			s =  new ServerSocket(5000, 10);

			//2. Wait for an incoming connection
			echo("ServerSocket created. Waiting for incoming connections...");
			//get the connection socket
			while(true) {
				conn = s.accept();
				// print the host name and port number of the connection
				echo("Connection received from: "+conn.getInetAddress()+ " : "+conn.getPort());

				//create new thread to handle client
				new ClientHandler(conn).start();

				//3. Get Input and Output streams
				//			out = new PrintStream(conn.getOutputStream());
				//			out.flush();
				//			in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
				//			out.println("Welcome. Server version 1.0");
				//			out.flush();

				//4. The two parts communicate via the input and output streams
				//			do 
				//			{
				//				//read input from the client
				//				message = (String)in.readLine();
				//				echo("client> "+ message);
				//				
				//				if(message != null) 
				//				{
				//					out.println(message);
				//				}
				//				else
				//				{
				//					echo("Client has disconnected");
				//					break;
				//				}
				//			}
				//			while(!message.equals("bye"));
			}
		}
		//
		catch(IOException e) 
		{
			System.err.println("IOException: " + e.getMessage());
		}
		//5. Close the connections and streams
		try
		{
			//				in.close();
			//				out.close();
			s.close();
		}
		// Close Exception
		catch(IOException e) 
		{
			System.err.println("Unable to close. IOException: " + e.getMessage());
		}		
	}

	public static void echo(String message) 
	{
		System.out.println(message);
	}

}


class ClientHandler extends Thread 
{
	private Socket conn;

	public ClientHandler(Socket conn) 
	{
		this.conn = conn;
	}

	public void run() 
	{

		String line;
		String input = "";
		OutputStream out_stream = null;
		DatagramOutputStream data_out = null;
		
		ByteArrayOutputStream byte_out  = null;
		
		int number = 0;
		try 
		{
			//get socket writing and reading streams
			DataInputStream in = new DataInputStream(conn.getInputStream());
			BufferedReader buff = new BufferedReader(new InputStreamReader(in));

			PrintStream	out = new PrintStream(conn.getOutputStream());

			byte_out =  new ByteArrayOutputStream();
			
			//Send welcome message to client
			System.out.println("Welcome to the server.");

			//Now start reading input from client
			while((line = buff.readLine()) != null && !line.equals(".")) 
			{
				//reply with the same message. adding some text
				out.println("I got: " + line);
				if(Integer.parseInt(line) >= 0) 
				{
					byte_out.write(number);
					
					System.out.println("Input Number: " + number + " Byte array: [" +byte_out.toString()+"]");
					
					byte[] bytes = byte_out.toByteArray();
					
					for(int i=0; i < bytes.length; i++ ) 
					{
						System.out.println(" Byte array: [" +i+"] : "+ bytes[i]);
						
					}
				}
				
			}
			conn.close();
		}
		//Exception
		catch(IOException e) 
		{
			System.err.println("IOException : "+ e.getMessage());	
			e.printStackTrace();
		}
	}

}
