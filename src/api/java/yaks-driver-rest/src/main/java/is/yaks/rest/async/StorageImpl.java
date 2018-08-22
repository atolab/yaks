package is.yaks.rest.async;

import java.util.concurrent.CompletableFuture;

import is.yaks.async.Storage;

public class StorageImpl implements Storage {

    private is.yaks.rest.StorageImpl syncStorage;

    public StorageImpl(is.yaks.rest.StorageImpl storage) {
        syncStorage = storage;
    }

    @Override
    public CompletableFuture<Void> dispose() {
        return CompletableFuture.runAsync(() -> {
            syncStorage.dispose();
        });
    }
}
