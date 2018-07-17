package is.yaks.rest.async;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import com.sun.jersey.api.client.WebResource;

import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.async.Access;
import is.yaks.async.Storage;
import is.yaks.async.Yaks;
import is.yaks.rest.utils.GsonTypeToken;
import is.yaks.rest.utils.YaksConfiguration;

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
	public CompletableFuture<Access> createAccess(Path scopePath, long cacheSize, Encoding encoding) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CompletableFuture<Access> createAccess(String id, Path scopePath, long cacheSize, Encoding encoding) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CompletableFuture<List<String>> getAccess() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CompletableFuture<Access> getAccess(String id) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CompletableFuture<is.yaks.Storage> createStorage(Path path, Properties option) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CompletableFuture<is.yaks.Storage> createStorage(String id, Path path, Properties option) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CompletableFuture<List<String>> getStorages() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CompletableFuture<is.yaks.Storage> getStorage(String id) {
		// TODO Auto-generated method stub
		return null;
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
