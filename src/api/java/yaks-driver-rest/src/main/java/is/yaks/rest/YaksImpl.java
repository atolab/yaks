package is.yaks.rest;

import java.net.HttpURLConnection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HttpsURLConnection;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.core.util.MultivaluedMapImpl;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.Storage;
import is.yaks.Yaks;
import is.yaks.rest.utils.GsonTypeToken;
import is.yaks.rest.utils.Utils;
import is.yaks.rest.utils.YaksConfiguration;

public class YaksImpl implements Yaks {

	private WebResource webResource;
	private Map<String, Access> accessById = new HashMap<String, Access>();
	private Map<String, Storage> storageById = new HashMap<String, Storage>();

	private YaksConfiguration config = YaksConfiguration.getInstance();
	private GsonTypeToken gsonTypes = GsonTypeToken.getInstance();


	private YaksImpl(String... args) {
		if(args.length == 0) {
			System.out.println("Usage: <yaksUrl>");
			System.exit(-1);
		}		
		String yaksUrl = args[0];
		if(yaksUrl.isEmpty()) {
			System.exit(-1);
		}

		config.setYaksUrl(yaksUrl);
		webResource = config.getClient().resource(config.getYaksUrl());

		registerShutdownHook();
	}


	@Override
	public Access createAccess(Path scopePath, long cacheSize, Encoding encoding) {
		switch (encoding) {
		case JSON:
		default:
			ClientResponse response = webResource.path("/yaks/access")
			.queryParam("path", scopePath.toString())
			.queryParam("cacheSize", cacheSize + "")
			.accept(MediaType.APPLICATION_JSON_TYPE)
			.post(ClientResponse.class);

			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_CREATED :			
				MultivaluedMap<String, String> headers = response.getHeaders();			
				String accessId = Utils.getValueFromHeaderKey(headers, "Set-Cookie", "is.yaks.access");
				String location = Utils.getHeader(headers, "Location");
				assert !String.valueOf(accessId).isEmpty() && !String.valueOf(location).isEmpty();
				AccessImpl access = new AccessImpl(accessId, scopePath, cacheSize);				
				accessById.put(accessId, access);
				return access;
			case HttpURLConnection.HTTP_FORBIDDEN:
			case HttpsURLConnection.HTTP_BAD_REQUEST:
			case 507: //507 (Insufficient Storage): if requested cacheSize is too large
			default:
				Utils.fail("Access creation failed, code: " + response.getStatus() + "\n" 
						+ "body: " + response.getEntity(String.class));
				return null;
			}
		}
	}


	@Override
	public Access createAccess(String id, Path scopePath, long cacheSize, Encoding encoding) {
		switch (encoding) {
		case JSON:
		default:
			ClientResponse response = webResource			
			.path("/yaks/access/"+id)
			.queryParam("path", scopePath.toString())
			.queryParam("cacheSize", cacheSize + "")
			.accept(MediaType.APPLICATION_JSON_TYPE)			
			.put(ClientResponse.class);

			MultivaluedMap<String, String> headers;
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_OK:
				headers = response.getHeaders();			
				String accessId = Utils.getValueFromHeaderKey(headers, "Set-Cookie", "is.yaks.access");				
				assert !String.valueOf(accessId).isEmpty();
				AccessImpl access = new AccessImpl(accessId, scopePath, cacheSize);
				accessById.put(accessId, access);
				return access;				
			case HttpURLConnection.HTTP_CREATED :
				headers = response.getHeaders();						
				accessId = Utils.getValueFromHeaderKey(headers, "Set-Cookie", "is.yaks.access");
				String location = Utils.getHeader(headers, "Location");
				assert !String.valueOf(accessId).isEmpty() && String.valueOf(location).equals(".");
				access = new AccessImpl(accessId, scopePath, cacheSize);
				access.setLocation(location);
				accessById.put(accessId, access);
				return access;				
			case HttpURLConnection.HTTP_FORBIDDEN:
			case HttpsURLConnection.HTTP_BAD_REQUEST:
			case 507: //507 (Insufficient Storage): if requested cacheSize is too large
			default:
				Utils.fail("Access creation with id "+id+" failed, code: " + response.getStatus() + "\n" 
						+ "body: " + response.getEntity(String.class));
				return null;
			}
		}
	}


	@Override
	public List<String> getAccess() {
		WebResource wr = webResource.path("/yaks/access");
		ClientResponse response = wr
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.get(ClientResponse.class);

		if (response.getStatus() == HttpURLConnection.HTTP_OK) {
			List<String> idList = config.getGson().fromJson(
					response.getEntity(String.class),
					gsonTypes.getCollectionTypeToken(String.class));
			return idList;
		} else {
			Utils.fail("Yaks instance failed to getAccess():\ncode: "+response.getStatus()+"\n" 
					+ "body: "+response.getEntity(String.class));
			return null;
		}
	}


	@Override
	public Access getAccess(String id) {
		Access ret = accessById.get(id);
		if(ret != null) {
			return ret;
		}

		WebResource wr = webResource.path("/yaks/access/"+id);				
		ClientResponse response = wr
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.get(ClientResponse.class);

		String data = response.getEntity(String.class);		
		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_OK:
			Access access = config.getGson().fromJson(
					data,
					gsonTypes.getTypeToken(is.yaks.rest.AccessImpl.class));			
			accessById.put(id, access);				
			return access;
		case HttpURLConnection.HTTP_NOT_FOUND:
		default:
			Utils.fail("Yaks instance failed to getAccess("+id+"):\ncode: "+response.getStatus()+"\n" 
					+ "body: "+data);
			return null;
		}
	}


	@Override
	public Storage createStorage(Path path, Properties option) {
		MultivaluedMap<String, String> mapOptions = new MultivaluedMapImpl();			
		option.forEach( (key, value) -> mapOptions.add(String.valueOf(key), String.valueOf(value)));			
		WebResource wr = webResource.path("/yaks/storages")
				.queryParam("path", path.toString())
				.queryParams(mapOptions);

		ClientResponse response = wr
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.post(ClientResponse.class);

		String data = response.getEntity(String.class);
		MultivaluedMap<String, String> headers = response.getHeaders();
		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_CREATED:				
			String storageId = Utils.getValueFromHeaderKey(headers, "Set-Cookie", "is.yaks.storage");
			String location = Utils.getHeader(headers, "Location");
			assert !String.valueOf(storageId).isEmpty() && !String.valueOf(location).isEmpty();
			StorageImpl storage = new StorageImpl(storageId, location);
			storageById.put(storageId, storage);				
			return storage;
		case HttpURLConnection.HTTP_NOT_IMPLEMENTED:
		case HttpURLConnection.HTTP_FORBIDDEN:
		case HttpURLConnection.HTTP_BAD_REQUEST:
		default:
			Utils.fail("Yaks instance failed to create storage with id:\ncode: "+response.getStatus()+"\n" 
					+ "body: "+data);
			return null;
		}
	}


	@Override
	public Storage createStorage(String id, Path path, Properties option) {
		MultivaluedMap<String, String> mapOptions = new MultivaluedMapImpl();			
		option.forEach( (key, value) -> mapOptions.add(String.valueOf(key), String.valueOf(value)));			
		WebResource wr = webResource.path("/yaks/storages/"+id)
				.queryParam("path", path.toString())
				.queryParams(mapOptions);

		ClientResponse response = wr
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.post(ClientResponse.class);
		String data = response.getEntity(String.class);

		MultivaluedMap<String, String> headers = response.getHeaders();
		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_CREATED:				
			String storageId = Utils.getValueFromHeaderKey(headers, "Set-Cookie", "is.yaks.storage");
			String location = Utils.getHeader(headers, "Location");				
			assert !String.valueOf(storageId).isEmpty() && String.valueOf(location).equals(".");
			StorageImpl storage = new StorageImpl(storageId, location);
			storageById.put(storageId, storage);				
			return storage;				
		case HttpURLConnection.HTTP_OK:				
			storageId = Utils.getValueFromHeaderKey(headers, "Set-Cookie", "is.yaks.storage");
			assert !String.valueOf(storageId).isEmpty();
			storage = new StorageImpl(storageId);
			storageById.put(id, storage);				
			return storage;
		case HttpURLConnection.HTTP_NOT_IMPLEMENTED:
		case HttpURLConnection.HTTP_FORBIDDEN:
		case HttpURLConnection.HTTP_BAD_REQUEST:
		default:
			Utils.fail("Yaks instance failed to create storage with id:\ncode: "+response.getStatus()+"\n" 
					+ "body: "+data);
			return null;
		}
	}


	@Override
	public List<String> getStorages() {
		WebResource wr = webResource.path("/yaks/storages");
		ClientResponse response = wr
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.get(ClientResponse.class);

		if (response.getStatus() == HttpURLConnection.HTTP_OK) {
			List<String> idList = config.getGson().fromJson(
					response.getEntity(String.class),
					gsonTypes.getCollectionTypeToken(String.class));

			return idList;
		} else {
			Utils.fail("Yaks instance failed to getStorage():\ncode: "+response.getStatus()+"\n" 
					+ "body: "+response.getEntity(String.class));
			return null;
		}
	}


	@Override
	public Storage getStorage(String id) {
		Storage ret = storageById.get(id);
		if(ret != null) {
			return ret;
		}

		WebResource wr = webResource.path("/yaks/storages/"+id);				
		ClientResponse response = wr
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.get(ClientResponse.class);
		String data = response.getEntity(String.class);
		switch (response.getStatus()) {
		case HttpURLConnection.HTTP_OK:
			StorageImpl storage = config.getGson().fromJson(
					data,
					gsonTypes.getTypeToken(is.yaks.rest.StorageImpl.class));
			assert storage != null;
			storageById.put(id, storage);				
			return storage;
		case HttpURLConnection.HTTP_NOT_FOUND:
		default:
			Utils.fail("Yaks instance failed to getStorage("+id+"):\ncode: "+response.getStatus()+"\n" 
					+ "body: "+data);
			return null;
		}

	}


	private void registerShutdownHook() {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {				
				ForkJoinPool.commonPool().awaitQuiescence(5,TimeUnit.SECONDS);
				YaksConfiguration.getInstance().getClient().destroy();
				YaksConfiguration.getInstance().getExecutorService().shutdown();
			}
		});
	}
}
