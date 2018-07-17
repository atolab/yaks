package is.yaks.rest.async;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import is.yaks.Storage;
import is.yaks.rest.utils.GsonTypeToken;
import is.yaks.rest.utils.Utils;
import is.yaks.rest.utils.YaksConfiguration;

public class StorageImpl implements Storage {

	private YaksConfiguration config = YaksConfiguration.getInstance();
	private GsonTypeToken gsonTypes = GsonTypeToken.getInstance();

	private String location;
	private String storageId;
	private is.yaks.rest.StorageImpl syncStorage;	

	public StorageImpl() {
	}

	public StorageImpl(String storageId, String location) {
		this.location = location;
		this.storageId = storageId;
		syncStorage = new is.yaks.rest.StorageImpl(storageId, location);
	}

	public StorageImpl(String storageId) {
		this.storageId = storageId;
		syncStorage = new is.yaks.rest.StorageImpl(storageId);
	}

	@Override
	public void dispose() {
		assert storageId != null;

		CompletableFuture<Void> futureDispose = CompletableFuture.runAsync(new Runnable() {
			@Override
			public void run() {
				syncStorage.dispose();
			}			
		});

		try {
			futureDispose.get(5, TimeUnit.SECONDS);
		} catch (Exception e) {
			Utils.fail("Storage fail to dispose " + e.getMessage());
		}
	}
}
