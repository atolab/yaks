package is.yaks.rest.async;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.CompletableFuture;

import is.yaks.Encoding;
import is.yaks.Path;
import is.yaks.async.Access;
import is.yaks.async.Storage;
import is.yaks.async.Yaks;

public class YaksImpl implements Yaks {

    private is.yaks.rest.YaksImpl syncYaks;
    private Map<String, Access> accessById = new HashMap<String, Access>();
    private Map<String, Storage> storageById = new HashMap<String, Storage>();

    private YaksImpl(String... args) {
        if (args.length == 0) {
            System.out.println("Usage: <yaksUrl>");
            System.exit(-1);
        }
        String yaksUrl = args[0];
        if (yaksUrl.isEmpty()) {
            System.exit(-1);
        }
        syncYaks = (is.yaks.rest.YaksImpl) is.yaks.Yaks.getInstance("is.yaks.rest.YaksImpl",
                getClass().getClassLoader(), args);
    }

    @Override
    public CompletableFuture<Access> createAccess(Path scopePath, long cacheSize, Encoding encoding) {
        return CompletableFuture.supplyAsync(() -> {
            is.yaks.rest.AccessImpl access = (is.yaks.rest.AccessImpl) syncYaks.createAccess(scopePath, cacheSize,
                    encoding);
            assert access != null;
            Access asyncAccess = new AccessImpl(access);
            accessById.put(access.getAccessId(), asyncAccess);
            return asyncAccess;
        });
    }

    @Override
    public CompletableFuture<Access> createAccess(String id, Path scopePath, long cacheSize, Encoding encoding) {
        return CompletableFuture.supplyAsync(() -> {
            is.yaks.rest.AccessImpl access = (is.yaks.rest.AccessImpl) syncYaks.createAccess(id, scopePath, cacheSize,
                    encoding);
            assert access != null;
            Access asyncAccess = new AccessImpl(access);
            accessById.put(id, asyncAccess);
            return asyncAccess;
        });
    }

    @Override
    public CompletableFuture<List<String>> getAccess() {
        return CompletableFuture.supplyAsync(() -> {
            return syncYaks.getAccess();
        });
    }

    @Override
    public CompletableFuture<Access> getAccess(String id) {
        Access ret = accessById.get(id);
        if (ret != null) {
            return CompletableFuture.completedFuture(ret);
        }

        return CompletableFuture.supplyAsync(() -> {
            is.yaks.rest.AccessImpl access = (is.yaks.rest.AccessImpl) syncYaks.getAccess(id);
            assert access != null;
            Access accessAsync = new AccessImpl(access);
            accessById.put(id, accessAsync);
            return accessAsync;
        });
    }

    @Override
    public CompletableFuture<List<String>> getStorages() {
        return CompletableFuture.supplyAsync(() -> {
            return syncYaks.getStorages();
        });
    }

    @Override
    public CompletableFuture<Storage> createStorage(Path path, Properties option) {
        return CompletableFuture.supplyAsync(() -> {
            is.yaks.rest.StorageImpl storage = (is.yaks.rest.StorageImpl) syncYaks.createStorage(path, option);
            assert storage != null;
            StorageImpl asyncStorage = new StorageImpl(storage);
            storageById.put(storage.getStorageId(), asyncStorage);
            return asyncStorage;
        });
    }

    @Override
    public CompletableFuture<Storage> createStorage(String id, Path path, Properties option) {
        return CompletableFuture.supplyAsync(() -> {
            is.yaks.rest.StorageImpl storage = (is.yaks.rest.StorageImpl) syncYaks.createStorage(id, path, option);
            assert storage != null;
            StorageImpl asyncStorage = new StorageImpl(storage);
            storageById.put(id, asyncStorage);
            return asyncStorage;
        });
    }

    @Override
    public CompletableFuture<Storage> getStorage(String id) {
        Storage ret = storageById.get(id);
        if (ret != null) {
            return CompletableFuture.completedFuture(ret);
        }

        return CompletableFuture.supplyAsync(() -> {
            is.yaks.rest.StorageImpl storage = (is.yaks.rest.StorageImpl) syncYaks.getStorage(id);
            assert storage != null;
            StorageImpl asyncStorage = new StorageImpl(storage);
            storageById.put(id, asyncStorage);
            return asyncStorage;
        });
    }
}
