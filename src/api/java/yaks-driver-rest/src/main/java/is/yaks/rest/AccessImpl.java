package is.yaks.rest;

import java.net.HttpURLConnection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.ws.rs.core.Cookie;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import is.yaks.Access;
import is.yaks.Selector;
import is.yaks.rest.utils.Utils;

public class AccessImpl extends Utils implements Access {

	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="accessId", alternate={"id"})
	private String accessId;

	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="path", alternate={"scopePath"})
	private String scopePath;	

	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="cacheSize", alternate={"cache","size"})
	private long cacheSize;

	//@Expose(serialize = true, deserialize = true)
	//@SerializedName(value="subscriptions", alternate={"subs","subsId"})
	private Map<String,Selector> subscriptions = new HashMap<String, Selector>();

	private String location;

	// called when loading rest data from Yaks
	public AccessImpl() {}

	//create access
	public AccessImpl(String scopePath, long cacheSize) {		
		this.scopePath = scopePath;
		this.cacheSize = cacheSize;
	}

	//create access
	public AccessImpl(String accessId, String scopePath, long cacheSize) {
		this.accessId = accessId;
		this.scopePath = scopePath;
		this.cacheSize = cacheSize;
	}

	@Override
	public Future<Access> put(Selector selector, Object value){
		CompletableFuture<Access> completableFuture = CompletableFuture.supplyAsync(() -> {
			WebResource wr = config.getClient()
					.resource(config.getYaksUrl())
					.path(selector.path);			
			ClientResponse response = wr
					.cookie(new Cookie("is.yaks.access",accessId))					
					.type(MediaType.APPLICATION_JSON_TYPE)
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.put(ClientResponse.class, config.getGson().toJson(value));

			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_NO_CONTENT:
				return this;				
			case HttpURLConnection.HTTP_BAD_REQUEST:
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Failed to put data :\ncode: " + response.getStatus()
				+"\nbody: "+response.getEntity(String.class));		
				return null;				
			}			
		});
		return completableFuture;
	}


	@Override
	public Future<Access> deltaPut(Selector selector, Object delta) {
		CompletableFuture<Access> completableFuture = CompletableFuture.supplyAsync(() -> {
			WebResource wr = config.getClient()
					.resource(config.getYaksUrl())
					.path(selector.path);
			ClientResponse response = wr
					.cookie(new Cookie("is.yaks.access", accessId))
					.type(MediaType.APPLICATION_JSON_TYPE)
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.method("PATCH", 
							ClientResponse.class, 
							config.getGson().toJson(delta));

			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_NO_CONTENT:
				return this;				
			case HttpURLConnection.HTTP_BAD_REQUEST:
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Failed to put data :\ncode: " + response.getStatus()
				+"\nbody: "+response.getEntity(String.class));
				return null;				
			}			
		});
		return completableFuture;
	}


	@Override
	public Future<Map<String, byte[]>> get(Selector selector) {
		CompletableFuture<Map<String, byte[]>> completableFuture = CompletableFuture.supplyAsync(() -> {
			WebResource wr = config.getClient()
					.resource(config.getYaksUrl())
					.path(selector.path);

			ClientResponse response = wr				
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.cookie(new Cookie("is.yaks.access", accessId))
					.get(ClientResponse.class);	

			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_OK:				
				Map<String, byte[]> map = config.getGson().fromJson(
						response.getEntity(String.class), 
						gsonTypes.MAP_KV);
				return map;
			case HttpURLConnection.HTTP_NOT_FOUND:
			case HttpURLConnection.HTTP_BAD_REQUEST:
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus()
				+"\nbody: "+response.getEntity(String.class));				
				return null;
			}
		});
		return completableFuture;
	}


	@Override
	public <T> Future<Map<String, T>> get(Selector selector, Class<T> c) {
		Future<Map<String, T>> completableFuture = CompletableFuture.supplyAsync(() -> {
			WebResource wr = config.getClient()
					.resource(config.getYaksUrl())
					.path(selector.path);
			ClientResponse response = wr				
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.cookie(new Cookie("is.yaks.access", accessId))
					.get(ClientResponse.class);
			String data = response.getEntity(String.class);			
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_OK:
				Map<String, String> jsonMap = config.getGson().fromJson(data, gsonTypes.MAP_KV);
				Map<String, T> map = jsonMap.entrySet()
						.parallelStream()
						.collect(Collectors.toMap(
								entry -> String.valueOf(entry.getKey()), 
								entry -> config.getGson().fromJson(String.valueOf(entry.getValue()), c)));
				return map;	
			case HttpURLConnection.HTTP_NOT_FOUND:
			case HttpURLConnection.HTTP_BAD_REQUEST:
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus()
				+"\nbody: " + data);				
				return null;
			}
		});
		return completableFuture;
	}


	@Override
	public void dispose() {
		CompletableFuture<Void> futureDispose = CompletableFuture.runAsync(new Runnable() {			
			@Override
			public void run() {
				WebResource wr = config.getClient()
						.resource(config.getYaksUrl())
						.path("/yaks/access/"+accessId);
				ClientResponse response = wr					
						.accept(MediaType.APPLICATION_JSON_TYPE)
						.delete(ClientResponse.class);

				switch (response.getStatus()) {
				case HttpURLConnection.HTTP_NO_CONTENT:
					break;				
				case HttpURLConnection.HTTP_NOT_FOUND:			
				default:
					fail("Access dispose failed with\n code: " + response.getStatus()
					+ "\nbody: " + response.getEntity(String.class));								
				}
			}			
		});

		try {
			futureDispose.get(5, TimeUnit.SECONDS);
		} catch (Exception e) {
			fail("Access Failed to dispose " + e.getMessage());
		}
	}

	@Override
	public Future<Access> remove(Selector selector) {
		CompletableFuture<Access> completableFuture = CompletableFuture.supplyAsync(() -> {
			WebResource wr = config.getClient()
					.resource(config.getYaksUrl())
					.path(selector.path);

			ClientResponse response = wr
					.type(MediaType.APPLICATION_JSON_TYPE)
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.cookie(new Cookie("is.yaks.access", accessId))
					.delete(ClientResponse.class);	

			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_NO_CONTENT:
				return this;			
			case HttpURLConnection.HTTP_BAD_REQUEST:
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus()
				+"\nbody: "+response.getEntity(String.class));				
				return null;
			}
		});
		return completableFuture;
	}

	@Override
	public Future<Long> subscribe(Selector selector) {
		Future<Long> completableFuture = CompletableFuture.supplyAsync(() -> {
			WebResource wr = config.getClient()
					.resource(config.getYaksUrl())
					.path("/yaks/access/"+accessId+"/subs")
					.queryParam("selector", selector.path);
			ClientResponse response = wr.post(ClientResponse.class);
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_CREATED:
				MultivaluedMap<String, String> headers = response.getHeaders();
				String location = getCookie(headers, "Location");
				subscriptions.put(location, selector);
				return new Long(location);
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Failed to subscribe to selector:\ncode: " + response.getStatus()
				+"\nbody: "+response.getEntity(String.class));				
				return null;				
			}			
		});
		return completableFuture;
	}

	@Override
	public Future<Map<String, Selector>> getSubscriptions() {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path("/yaks/access/"+accessId+"/subs");

		CompletableFuture<Map<String, Selector>> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr				
					.type(MediaType.APPLICATION_JSON_TYPE)
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);	

			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_OK:				
				Map<String, String> map = config.getGson().fromJson(
						response.getEntity(String.class), 
						gsonTypes.MAP_KV_STRING);
				subscriptions.putAll(
						map.entrySet().parallelStream().collect(Collectors.toMap(
								p -> String.valueOf(p.getKey()), 
								p -> Selector.path(p.getValue())))
						);				
				return subscriptions;
			default:
			case HttpURLConnection.HTTP_NOT_FOUND:
				fail("Failed to get subscrition list :\ncode: " + response.getStatus()
				+"\nbody: "+response.getEntity(String.class));					
				return null;
			}
		});

		return completableFuture;
	}

	@Override
	public void unsubscribe(long subscriptionId) {
		CompletableFuture<Void> completableFuture = CompletableFuture.runAsync( new Runnable() {

			@Override
			public void run() {
				WebResource wr = config.getClient()
						.resource(config.getYaksUrl())
						.path("/yaks/access/"+accessId+"/subs/"+subscriptionId);

				ClientResponse response = wr
						.type(MediaType.APPLICATION_JSON_TYPE)
						.accept(MediaType.APPLICATION_JSON_TYPE)
						.delete(ClientResponse.class);	

				switch (response.getStatus()) {
				case HttpURLConnection.HTTP_NO_CONTENT:
					return;			
				case HttpURLConnection.HTTP_NOT_FOUND:
				default:
					fail("Failed to unsubscribe:\ncode: " + response.getStatus()
					+"\nbody: "+response.getEntity(String.class));				
					return;
				}
			}
		});

		try {
			completableFuture.get(5, TimeUnit.SECONDS);
		} catch (Exception e) {
			fail("Access Failed to unsubscribe " + e.getMessage());
		}
	}

	@Override
	public void eval(Selector selector, Function<Selector, Object> computation) {
		// TODO Auto-generated method stub

	}

	@Override
	public void close() {
		// TODO Auto-generated method stub
	}

	public long getCacheSize() {
		return cacheSize;
	}

	public void setAccessId(String accessId) {		
		this.accessId = accessId;
	}

	public String getAccessId() {
		return accessId;
	}

	public void setLocation(String location) {
		this.location = location;		
	}

	@Override
	public String toString() {	
		return "{id:"+ accessId+", cache:" + cacheSize + ", scopePath:" + scopePath+", location: "+location+"}";
	}

}
