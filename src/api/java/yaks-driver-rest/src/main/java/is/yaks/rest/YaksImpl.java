package is.yaks.rest;

import java.net.HttpURLConnection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HttpsURLConnection;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.core.util.MultivaluedMapImpl;

import is.yaks.Access;
import is.yaks.Encoding;
import is.yaks.Storage;
import is.yaks.Yaks;
import is.yaks.rest.utils.Utils;

public class YaksImpl implements Yaks {

	private WebResource webResource;
	private Map<String, Access> accessById = new HashMap<String, Access>();
	private Map<String, Storage> storageById = new HashMap<String, Storage>();

	private YaksConfiguration config = YaksConfiguration.getInstance();
	private GsonTypeToken gsonTypes = GsonTypeToken.getInstance();

	public YaksImpl(String... args) {
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
	public Future<Access> createAccess (String scopePath, long cacheSize, Encoding encoding){		
		CompletableFuture<Access> completableFuture = CompletableFuture.supplyAsync(() -> {
			switch (encoding) {
			case JSON:
			default:
				ClientResponse response = webResource.path("/yaks/access")
				.queryParam("path", scopePath)
				.queryParam("cacheSize", cacheSize + "")
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.post(ClientResponse.class);

				switch (response.getStatus()) {
				case HttpURLConnection.HTTP_CREATED :			
					MultivaluedMap<String, String> headers = response.getHeaders();			
					String accessId = Utils.getCookieData(headers, "Set-Cookie", "is.yaks.access");
					String location = Utils.getCookie(headers, "Location");
					assert !String.valueOf(accessId).isEmpty() && !String.valueOf(location).isEmpty();
					AccessImpl access = new AccessImpl(accessId, scopePath, cacheSize);
					access.setLocation(location);
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
		});
		return completableFuture;		
	}


	@Override
	public Future<Access> createAccess (String id, String scopePath, long cacheSize, Encoding encoding){
		CompletableFuture<Access> completableFuture = CompletableFuture.supplyAsync(() -> {
			switch (encoding) {
			case JSON:
			default:
				ClientResponse response = webResource
				.path("/yaks/access/"+id)
				.queryParam("path", scopePath)
				.queryParam("cacheSize", cacheSize + "")
				.accept(MediaType.APPLICATION_JSON_TYPE)
				.put(ClientResponse.class);

				MultivaluedMap<String, String> headers;
				switch (response.getStatus()) {
				case HttpURLConnection.HTTP_OK:
					headers = response.getHeaders();			
					String accessId = Utils.getCookieData(headers, "Set-Cookie", "is.yaks.access");				
					assert !String.valueOf(accessId).isEmpty();
					AccessImpl access = new AccessImpl(accessId, scopePath, cacheSize);
					accessById.put(accessId, access);
					return access;
				case HttpURLConnection.HTTP_CREATED :
					headers = response.getHeaders();						
					accessId = Utils.getCookieData(headers, "Set-Cookie", "is.yaks.access");
					String location = Utils.getCookie(headers, "Location");
					access = new AccessImpl(id, scopePath, cacheSize);
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
		});
		return completableFuture;		
	}


	@Override
	public Future<List<String>> getAccess() {		
		Future<List<String>> completableFuture = CompletableFuture.supplyAsync(() -> {
			WebResource wr = webResource.path("/yaks/access");
			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);

			if (response.getStatus() == HttpURLConnection.HTTP_OK) {
				List<String> idList = config.getGson().fromJson(
						response.getEntity(String.class),
						gsonTypes.COLLECTION_ID);

				return idList;
			} else {
				Utils.fail("Yaks instance failed to getAccess():\ncode: "+response.getStatus()+"\n" 
						+ "body: "+response.getEntity(String.class));
				return null;
			}
		});
		return completableFuture;
	}



	@Override
	public Future<Access> getAccess(String accessId) {		
		Access ret = accessById.get(accessId);
		if(ret != null) {
			return CompletableFuture.completedFuture(ret);
		}
		Future<Access> completableFuture = CompletableFuture.supplyAsync(() ->{
			WebResource wr = webResource.path("/yaks/access/"+accessId);				
			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);
			String data = response.getEntity(String.class);
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_OK:
				AccessImpl access = config.getGson().fromJson(
						data,
						gsonTypes.ACCESS);
				access.setAccessId(accessId);
				accessById.put(access.getAccessId(), access);				
				return access;
			case HttpURLConnection.HTTP_NOT_FOUND:
			default:
				Utils.fail("Yaks instance failed to getAccess("+accessId+"):\ncode: "+response.getStatus()+"\n" 
						+ "body: "+data);
				return null;
			}
		});
		return completableFuture;
	}


	@Override
	public Future<Storage> createStorage(String path, Properties option){
		Future<Storage> completableFuture = CompletableFuture.supplyAsync(() -> {			
			MultivaluedMap<String, String> mapOptions = new MultivaluedMapImpl();			
			option.forEach( (key, value) -> mapOptions.add(String.valueOf(key), String.valueOf(value)));			
			WebResource wr = webResource.path("/yaks/storages")
					.queryParam("path", path)
					.queryParams(mapOptions);

			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.post(ClientResponse.class);
			String data = response.getEntity(String.class);

			MultivaluedMap<String, String> headers = response.getHeaders();
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_CREATED:				
				String location = Utils.getCookie(headers, "Location");
				String storageId = Utils.getCookieData(headers, "Set-Cookie", "is.yaks.storage");
				assert !String.valueOf(storageId).isEmpty() && !String.valueOf(location).isEmpty();
				StorageImpl storage = new StorageImpl(storageId, location);
				storageById.put(storageId, storage);				
				return storage;				
			case HttpURLConnection.HTTP_OK:				
				storageId = Utils.getCookieData(headers, "Set-Cookie", "is.yaks.storage");
				assert !String.valueOf(storageId).isEmpty();
				storage = new StorageImpl(storageId);
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
		});
		return completableFuture;
	}


	@Override
	public Future<Storage> createStorage(String id, String path, Properties option) {
		Future<Storage> completableFuture = CompletableFuture.supplyAsync(() -> {			
			MultivaluedMap<String, String> mapOptions = new MultivaluedMapImpl();			
			option.forEach( (key, value) -> mapOptions.add(String.valueOf(key), String.valueOf(value)));			
			WebResource wr = webResource.path("/yaks/storages/"+id)
					.queryParam("path", path)
					.queryParams(mapOptions);

			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.post(ClientResponse.class);
			String data = response.getEntity(String.class);

			MultivaluedMap<String, String> headers = response.getHeaders();
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_CREATED:				
				String location = Utils.getCookie(headers, "Location");
				String storageId = Utils.getCookieData(headers, "Set-Cookie", "is.yaks.storage");	
				assert !String.valueOf(storageId).isEmpty() && !String.valueOf(location).isEmpty();
				StorageImpl storage = new StorageImpl(storageId, location);
				storageById.put(storageId, storage);				
				return storage;				
			case HttpURLConnection.HTTP_OK:				
				storageId = Utils.getCookieData(headers, "Set-Cookie", "is.yaks.storage");
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
		});
		return completableFuture;
	}


	public Future<List<String>> getStorage(){
		WebResource wr = webResource.path("/yaks/storages");
		Future<List<String>> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);

			if (response.getStatus() == HttpURLConnection.HTTP_OK) {
				List<String> idList = config.getGson().fromJson(
						response.getEntity(String.class),
						gsonTypes.COLLECTION_ID);

				return idList;
			} else {
				Utils.fail("Yaks instance failed to getStorage():\ncode: "+response.getStatus()+"\n" 
						+ "body: "+response.getEntity(String.class));
				return null;
			}
		});
		return completableFuture;
	}


	public Future<Storage> getStorage(String storageId){
		Storage ret = storageById.get(storageId);
		if(ret != null) {
			return CompletableFuture.completedFuture(ret);
		}
		Future<Storage> completableFuture = CompletableFuture.supplyAsync(() ->{
			WebResource wr = webResource.path("/yaks/storages/"+storageId);				
			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);
			String data = response.getEntity(String.class);
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_OK:
				StorageImpl storage = config.getGson().fromJson(
						data,
						gsonTypes.ACCESS);
				assert storage != null;
				storageById.put(storageId, storage);				
				return storage;
			case HttpURLConnection.HTTP_NOT_FOUND:
			default:
				Utils.fail("Yaks instance failed to getStorage("+storageId+"):\ncode: "+response.getStatus()+"\n" 
						+ "body: "+data);
				return null;
			}
		});
		return completableFuture;
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
