package is.yaks.rest;

import is.yaks.Storage;

public class StorageImpl implements Storage {

	private String location;
	private String storageId;

	public StorageImpl() {
	}
	
	public StorageImpl(String location, String storageId) {
		this.location = location;
		this.storageId = storageId;
	}

	public StorageImpl(String storageId) {
		this.storageId = storageId;
	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

}
