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
import is.yaks.Selector;
import is.yaks.Storage;
import is.yaks.Yaks;
import is.yaks.rest.utils.Utils;

public class YaksImpl extends Utils implements Yaks {

	private WebResource webResource;
	private Map<String, Access> accessById = new HashMap<String, Access>();
	private Map<String, Storage> storageById = new HashMap<String, Storage>();

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

		onShutdown();
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
					String accessId = getCookieData(headers, "Set-Cookie", "is.yaks.access");
					String location = getCookie(headers, "Location");
					assert accessId != null && location != null;
					AccessImpl access = new AccessImpl(accessId, scopePath, cacheSize);
					access.setLocation(location);
					accessById.put(accessId, access);
					return access;
				case HttpURLConnection.HTTP_FORBIDDEN:
				case HttpsURLConnection.HTTP_BAD_REQUEST:
				case 507: //507 (Insufficient Storage): if requested cacheSize is too large
				default:
					fail("Access creation failed, code: " + response.getStatus() + "\n" 
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
					String accessId = getCookieData(headers, "Set-Cookie", "is.yaks.access");				
					AccessImpl access = new AccessImpl(id, scopePath, cacheSize);
					accessById.put(accessId, access);
					return access;
				case HttpURLConnection.HTTP_CREATED :
					headers = response.getHeaders();						
					accessId = getCookieData(headers, "Set-Cookie", "is.yaks.access");
					String location = getCookie(headers, "Location");
					access = new AccessImpl(id, scopePath, cacheSize);
					access.setLocation(location);
					accessById.put(accessId, access);
					return access;
				case HttpURLConnection.HTTP_FORBIDDEN:
				case HttpsURLConnection.HTTP_BAD_REQUEST:
				case 507: //507 (Insufficient Storage): if requested cacheSize is too large
				default:
					fail("Access creation with id "+id+" failed, code: " + response.getStatus() + "\n" 
							+ "body: " + response.getEntity(String.class));
					return null;
				}
			}
		});
		return completableFuture;		
	}


	@Override
	public Future<List<String>> getAccess() {
		WebResource wr = webResource.path("/yaks/access");
		Future<List<String>> completableFuture = CompletableFuture.supplyAsync(() -> {
			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.get(ClientResponse.class);

			if (response.getStatus() == HttpURLConnection.HTTP_OK) {
				List<String> idList = config.getGson().fromJson(
						response.getEntity(String.class),
						gsonTypes.COLLECTION_ACCESS_ID);

				return idList;
			} else {
				fail("Yaks instance failed to getAccess():\ncode: "+response.getStatus()+"\n" 
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
						gsonTypes.ACCESS_DATA);
				access.setAccessId(accessId);
				accessById.put(access.getAccessId(), access);				
				return access;
			case HttpURLConnection.HTTP_NOT_FOUND:
			default:
				fail("Yaks instance failed to getAccess("+accessId+"):\ncode: "+response.getStatus()+"\n" 
						+ "body: "+data);
				return null;
			}
		});
		return completableFuture;
	}


	@Override
	public Future<Storage> createStorage(String id, Selector path, Properties option) {
		Future<Storage> completableFuture = CompletableFuture.supplyAsync(() -> {			
			MultivaluedMap<String, String> mapOptions = new MultivaluedMapImpl();			
			option.forEach( (key, value) -> mapOptions.add(String.valueOf(key), String.valueOf(value)));			
			WebResource wr = webResource.path("/yaks/storages")
					.queryParam("path", path.path)
					.queryParams(mapOptions);
			
			ClientResponse response = wr
					.accept(MediaType.APPLICATION_JSON_TYPE)
					.post(ClientResponse.class);
			String data = response.getEntity(String.class);
			switch (response.getStatus()) {
			case HttpURLConnection.HTTP_CREATED:
				StorageImpl storage = new StorageImpl();
				storageById.put(id, storage);				
				return storage;
			case HttpURLConnection.HTTP_NOT_IMPLEMENTED:
			case HttpURLConnection.HTTP_FORBIDDEN:
			case HttpURLConnection.HTTP_BAD_REQUEST:
			default:
				fail("Yaks instance failed to create storage with id:\ncode: "+response.getStatus()+"\n" 
						+ "body: "+data);
				return null;
			}
		});
		return completableFuture;
	}

	@Override
	public Future<Storage> createStorage(String path, Properties option){
		//TODO
		return null;
	}


	private void onShutdown() {
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
