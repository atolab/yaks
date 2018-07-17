package is.yaks.rest;

import java.net.HttpURLConnection;
import java.util.Map;
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
import is.yaks.Listener;
import is.yaks.Path;
import is.yaks.Selector;
import is.yaks.rest.utils.GsonTypeToken;
import is.yaks.rest.utils.Utils;
import is.yaks.rest.utils.YaksConfiguration;

public class AccessImpl implements Access {

	private YaksConfiguration config = YaksConfiguration.getInstance();
	private GsonTypeToken gsonTypes = GsonTypeToken.getInstance();

	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="accessId", alternate={"id"})
	private String accessId;

	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="path", alternate={"scopePath"})
	private String scopePath;	

	@Expose(serialize = true, deserialize = true)
	@SerializedName(value="cacheSize", alternate={"cache","size"})
	private long cacheSize;

	//create access
	public AccessImpl(String accessId, Path scopePath, long cacheSize) {
		this.accessId = accessId;
		this.scopePath = scopePath.toString();
		this.cacheSize = cacheSize;
	}
	
	@Override
	public Access put(Selector selector, Object value) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(selector.toString());			
		ClientResponse response = wr
				.cookie(new Cookie("is.yaks.access", accessId))					
				.type(MediaType.APPLICATION_JSON_TYPE)
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.put(ClientResponse.class, config.getGson().toJson(value));

		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_NO_CONTENT:
			return this;				
		case HttpURLConnection.HTTP_BAD_REQUEST:
		case HttpURLConnection.HTTP_PRECON_FAILED:
		default:
			Utils.fail("Failed to put data :\ncode: " + response.getStatus()
			+"\nbody: "+response.getEntity(String.class));		
			return null;				
		}
	}

	@Override
	public Access deltaPut(Selector selector, Object delta) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(selector.toString());
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
			Utils.fail("Failed to put data :\ncode: " + response.getStatus()
			+"\nbody: "+response.getEntity(String.class));
			return null;				
		}
	}

	@Override
	public Access remove(Selector selector) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(selector.toString());

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
			Utils.fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus()
			+"\nbody: "+response.getEntity(String.class));				
			return null;
		}
	}

	@Override
	public Long subscribe(Selector selector) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path("/yaks/access/"+accessId+"/subs")
				.queryParam("selector", selector.toString());
		ClientResponse response = wr.post(ClientResponse.class);
		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_CREATED:
			MultivaluedMap<String, String> headers = response.getHeaders();
			String location = Utils.getHeader(headers, "Location");			
			return new Long(location);
		case HttpURLConnection.HTTP_PRECON_FAILED:
		default:
			Utils.fail("Failed to subscribe to selector:\ncode: " + response.getStatus()
			+"\nbody: "+response.getEntity(String.class));				
			return null;				
		}
	}

	@Override
	public <T> Long subscribe(Selector selector, Listener<T> listener) {
		// TODO Auto-generated method stub
		return null;		
	}

	@Override
	public Map<String, Selector> getSubscriptions() {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path("/yaks/access/"+accessId+"/subs");

		ClientResponse response = wr				
				.type(MediaType.APPLICATION_JSON_TYPE)
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.get(ClientResponse.class);	

		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_OK:				
			Map<String, String> map = config.getGson().fromJson(
					response.getEntity(String.class), 
					gsonTypes.MAP_KV_STRING);

			Map<String, Selector> ret = map.entrySet()
					.parallelStream()
					.collect(
						Collectors.toMap(
						p -> String.valueOf(p.getKey()), 
						p -> Selector.ofString((p.getValue()))));
			return ret;

		default:
		case HttpURLConnection.HTTP_NOT_FOUND:
			Utils.fail("Failed to get subscrition list :\ncode: " + response.getStatus()
			+"\nbody: "+response.getEntity(String.class));					
			return null;
		}
	}

	@Override
	public void unsubscribe(long subscriptionId) {
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
			Utils.fail("Failed to unsubscribe:\ncode: " + response.getStatus()
			+"\nbody: "+response.getEntity(String.class));				
			return;
		}

	}

	@Override
	public <T> Map<Path, T> get(Selector selector, Class<T> c) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(selector.toString());
		ClientResponse response = wr				
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.cookie(new Cookie("is.yaks.access", accessId))
				.get(ClientResponse.class);
		String data = response.getEntity(String.class);			
		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_OK:
			Map<String, String> jsonMap = config.getGson().fromJson(data, gsonTypes.MAP_KV);
			Map<Path, T> map = jsonMap.entrySet()
					.parallelStream()
					.collect(Collectors.toMap(
							entry -> Path.ofString(String.valueOf(entry.getKey())), 
							entry -> config.getGson().fromJson(String.valueOf(entry.getValue()), c)));
			return map;	
		case HttpURLConnection.HTTP_NOT_FOUND:
		case HttpURLConnection.HTTP_BAD_REQUEST:
		case HttpURLConnection.HTTP_PRECON_FAILED:
		default:
			Utils.fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus()
			+"\nbody: " + data);				
			return null;
		}
	}

	@Override
	public <T> T get(Path path, Class<T> c) {
		WebResource wr = config.getClient()
				.resource(config.getYaksUrl())
				.path(path.toString());
		ClientResponse response = wr				
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.cookie(new Cookie("is.yaks.access", accessId))
				.get(ClientResponse.class);
		String data = response.getEntity(String.class);			
		
		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_OK:			
			return config.getGson().fromJson(data, c);	
		case HttpURLConnection.HTTP_NOT_FOUND:
		case HttpURLConnection.HTTP_BAD_REQUEST:
		case HttpURLConnection.HTTP_PRECON_FAILED:
		default:
			Utils.fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus()
			+"\nbody: " + data);				
			return null;
		}
	}

	@Override
	public void eval(Selector selector, Function<Path, Object> computation) {
		// TODO Auto-generated method stub

	}

	@Override
	public void close() {
		// TODO Auto-generated method stub

	}

	@Override
	public void dispose() {
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
			Utils.fail("Access dispose failed with\n code: " + response.getStatus()
			+ "\nbody: " + response.getEntity(String.class));								
		}
	}

}
