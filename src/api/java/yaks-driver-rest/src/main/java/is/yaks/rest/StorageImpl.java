package is.yaks.rest;

import java.net.HttpURLConnection;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import javax.ws.rs.core.MediaType;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import is.yaks.Storage;
import is.yaks.rest.utils.Utils;

public class StorageImpl extends Utils implements Storage {

	private String location;
	private String storageId;
	

	public StorageImpl() {
	}
	
	public StorageImpl(String storageId, String location) {
		this.location = location;
		this.storageId = storageId;
	}

	public StorageImpl(String storageId) {
		this.storageId = storageId;
	}

	@Override
	public void dispose() {
		assert storageId != null;

		CompletableFuture<Void> futureDispose = CompletableFuture.runAsync(new Runnable() {
			@Override
			public void run() {
				WebResource wr = config.getClient()
						.resource(config.getYaksUrl())
						.path("/yaks/storages/"+storageId);
				
				ClientResponse response = wr					
						.accept(MediaType.APPLICATION_JSON_TYPE)
						.delete(ClientResponse.class);

				switch (response.getStatus()) {
				case HttpURLConnection.HTTP_NO_CONTENT:
					return;				
				case HttpURLConnection.HTTP_NOT_FOUND:			
				default:
					fail("Storage dispose failed with\n code: " + response.getStatus()
							+ "\nbody: " + response.getEntity(String.class));								
				}
			}			
		});
		
		try {
			futureDispose.get(5, TimeUnit.SECONDS);
		} catch (Exception e) {
			fail("Storage fail to dispose " + e.getMessage());
		}
	}
}
