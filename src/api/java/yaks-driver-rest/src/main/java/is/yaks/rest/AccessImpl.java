package is.yaks.rest;

import java.net.HttpURLConnection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.function.Function;

import javax.net.ssl.HttpsURLConnection;
import javax.ws.rs.core.Cookie;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;
import com.sun.jersey.api.client.Client;
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
	private Set<String> path = new HashSet<String>();	


	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="cacheSize", alternate={"cache","size"})
	private long cacheSize;

	//@Expose(serialize = true, deserialize = true)
	//@SerializedName(value="subscriptions", alternate={"subs","subsId"})
	private Map<String,Selector> subscriptions = new HashMap<String, Selector>();

	// called when loading rest data from Yaks
	public AccessImpl() {}

	//create access
	public AccessImpl(String scopePath, long cacheSize) {		
		Client client = config.getClient();		
		String yaksUrl = config.getYaksUrl();
		WebResource wr = client.resource(yaksUrl);

		ClientResponse response = wr.path("/yaks/access")
				.queryParam("path", scopePath)
				.queryParam("cacheSize", cacheSize + "")
				.post(ClientResponse.class);

		LOG.info(response.getEntity(String.class));

		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_CREATED :			
			MultivaluedMap<String, String> headers = response.getHeaders();			
			accessId = getCookieData(headers, "Set-Cookie", "is.yaks.access");
			path.add(getCookie(headers, "Location"));			
			this.cacheSize = cacheSize;			
			LOG.info("CREATE ({}:AccessImpl {location:\"{}\", cacheSize:\"{}\"})", getAccessId(), path, this.cacheSize);			
			break;
		case HttpURLConnection.HTTP_FORBIDDEN:
		case HttpsURLConnection.HTTP_BAD_REQUEST:
		case 507: //507 (Insufficient Storage): if requested cacheSize is too large
		default:
			fail("Access creation failed with code : " + response.getStatus());
		}	

	}

	//create access
	public AccessImpl(String id, String scopePath, long cacheSize) {
		Client client = config.getClient();		
		String yaksUrl = config.getYaksUrl();
		ClientResponse response = client.resource(yaksUrl)
				.path("/yaks/access/"+id)
				.queryParam("path", scopePath)
				.queryParam("cacheSize", cacheSize + "")
				.put(ClientResponse.class);

		LOG.info(response.getEntity(String.class));
		MultivaluedMap<String, String> headers;

		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_OK:
			headers = response.getHeaders();			
			accessId = getCookieData(headers, "Set-Cookie", "is.yaks.access");			
			this.cacheSize = cacheSize;
			break;
		case HttpURLConnection.HTTP_CREATED :
			headers = response.getHeaders();						
			accessId = getCookieData(headers, "Set-Cookie", "is.yaks.access");			
			path.add(getCookie(headers, "Location"));
			this.cacheSize = cacheSize;			
			break;
		case HttpURLConnection.HTTP_FORBIDDEN:
		case HttpsURLConnection.HTTP_BAD_REQUEST:
		case 507: //507 (Insufficient Storage): if requested cacheSize is too large
		default:
			fail("Access creation failed with code : " + response.getStatus());
		}		
	}

	@Override
	public Future<Access> put(Selector selector, Object value){
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(selector.path);

		CompletableFuture<Access> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr
					.cookie(new Cookie("is.yaks.access",accessId))
					.type(MediaType.APPLICATION_JSON_TYPE)
					.put(ClientResponse.class, value);
			
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_NO_CONTENT:
				return this;				
			case HttpURLConnection.HTTP_BAD_REQUEST:
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				
				fail("Access<V> put NOT OK: " + response.getStatus() + "\n" + response.getEntity(String.class));
				return null;				
			}			
		});
		return completableFuture;
	}


	@Override
	public Future<Access> deltaPut(Selector selector, Object delta) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(selector.path);

		CompletableFuture<Access> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr
					.cookie(new Cookie("is.yaks.access", accessId))
					.type(MediaType.APPLICATION_JSON_TYPE)
					.method("PATCH", ClientResponse.class, delta);
			
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_NO_CONTENT:
				return this;				
			case HttpURLConnection.HTTP_BAD_REQUEST:
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Access<V> put NOT OK: " + response.getStatus());
				return null;				
			}			
		});
		return completableFuture;
	}

	@Override
	 public Future<Map<Selector, byte[]>> get(Selector selector) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(selector.path);
		CompletableFuture<Map<Selector, byte[]>> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr				
					.type(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);	
			
			if(response.getStatus() == HttpURLConnection.HTTP_OK) {	
				Map<Selector, byte[]> map = config.getGson().fromJson(
						response.getEntity(String.class), 
						gsonTypes.MAP_KV); //TODO change MAP_KV
				return map;
			} else {
				fail("Access<V> get NOT OK: " + response.getStatus());				
				return null;
			}
		});

		return completableFuture;
	}
	
	@Override
	public <T> Future<Map<Selector, T>> get(Selector Selector, Class c) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Future<Long> subscribe(Selector selector) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path("yaks/access/"+accessId+"/subs")
				.queryParam("selector", selector.path);

		Future<Long> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr.post(ClientResponse.class);
			
			//System.out.println(response.getEntity(String.class));
			
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_CREATED:
				MultivaluedMap<String, String> headers = response.getHeaders();
				String subPath = getCookie(headers, "Location");
				subscriptions.put(subPath, selector);
				return new Long(subPath);
			case HttpURLConnection.HTTP_PRECON_FAILED:
			default:
				fail("Access<V> put NOT OK: " + response.getStatus() + "\n" + response.getEntity(String.class));
				return null;				
			}			
		});
		return completableFuture;
	}

	@Override
	public Future<Map<String, Selector>> getSubscriptions() {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path("yaks/access/"+accessId+"/subs");

		CompletableFuture<Map<String, Selector>> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr				
					.type(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);	
			
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_OK:				
				Map<String, String> map = config.getGson().fromJson(
						response.getEntity(String.class), 
						gsonTypes.MAP_SELECTOR_BY_SUBS);				
				map.forEach( (idSub, selectorPath) -> subscriptions.put(idSub, Selector.path(selectorPath)) );
				
				return subscriptions;
			default:
			case HttpURLConnection.HTTP_NOT_FOUND:
				fail("No subscriptions with specified id exists " + response.getStatus());				
				return null;
			}
		});

		return completableFuture;
	}

	@Override
	public Future<Access> remove(Selector selector) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void eval(Selector selector, Function<Selector, Object> computation) {
		// TODO Auto-generated method stub

	}

	@Override
	public void unsubscribe(long sid) {
		// TODO Auto-generated method stub

	}

	@Override
	public void dispose() {		
		// TODO Auto-generated method stub
	}

	@Override
	public void close() {
		// TODO Auto-generated method stub

	}

	public long getCacheSize() {
		return cacheSize;
	}

	public void setCacheSize(long cacheSize) {
		this.cacheSize = cacheSize;
	}

	public Set<String> getPath() {
		return path;
	}

	public void setPath(Set<String> path) {
		this.path = path;
	}

	public void setAccessId(String accessId) {		
		this.accessId = accessId;
	}

	public String getAccessId() {
		return accessId;
	}

	@Override
	public String toString() {	
		return "{id:"+ getAccessId()+", cache:" + getCacheSize() + ", path:" + getPath()+"}";
	}

	
}
