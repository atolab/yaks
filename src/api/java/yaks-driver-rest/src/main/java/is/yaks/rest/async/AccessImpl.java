package is.yaks.rest.async;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import is.yaks.Listener;
import is.yaks.Path;
import is.yaks.Selector;
import is.yaks.async.Access;

public class AccessImpl implements Access {

    private is.yaks.rest.AccessImpl syncAccess;

    // no modifier, only visible in class and package
    AccessImpl(is.yaks.rest.AccessImpl access) {
        syncAccess = access;
    }

    @Override
    public CompletableFuture<Access> put(Selector selector, Object value) {
        CompletableFuture<Access> future = CompletableFuture.supplyAsync(() -> {
            syncAccess = (is.yaks.rest.AccessImpl) syncAccess.put(selector, value);
            return this;
        });
        return future;
    }

    @Override
    public CompletableFuture<Access> deltaPut(Selector selector, Object delta) {
        CompletableFuture<Access> future = CompletableFuture.supplyAsync(() -> {
            syncAccess = (is.yaks.rest.AccessImpl) syncAccess.deltaPut(selector, delta);
            return this;
        });
        return future;
    }

    @Override
    public CompletableFuture<Access> remove(Selector selector) {
        CompletableFuture<Access> future = CompletableFuture.supplyAsync(() -> {
            syncAccess = (is.yaks.rest.AccessImpl) syncAccess.remove(selector);
            return this;
        });
        return future;
    }

    @Override
    public CompletableFuture<Long> subscribe(Selector selector) {
        CompletableFuture<Long> future = CompletableFuture.supplyAsync(() -> {
            return syncAccess.subscribe(selector);
        });
        return future;
    }

    @Override
    public <T> CompletableFuture<Long> subscribe(Selector selector, Listener<T> listener) {
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<Map<String, Selector>> getSubscriptions() {
        CompletableFuture<Map<String, Selector>> future = CompletableFuture.supplyAsync(() -> {
            return syncAccess.getSubscriptions();
        });
        return future;
    }

    @Override
    public CompletableFuture<Void> unsubscribe(long sid) {
        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
            syncAccess.unsubscribe(sid);
        });
        return future;
    }

    @Override
    public <T> CompletableFuture<Map<Path, T>> get(Selector selector, Class<T> c) {
        CompletableFuture<Map<Path, T>> future = CompletableFuture.supplyAsync(() -> {
            return syncAccess.get(selector, c);
        });
        return future;
    }

    @Override
    public <T> CompletableFuture<T> get(Path path, Class<T> c) {
        CompletableFuture<T> future = CompletableFuture.supplyAsync(() -> {
            return syncAccess.get(path, c);
        });
        return future;
    }

    @Override
    public CompletableFuture<Void> eval(Selector selector, Function<Path, Object> computation) {
        throw new UnsupportedOperationException();
    }

    @Override
    public CompletableFuture<Void> close() {
        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
            syncAccess.close();
        });
        return future;
    }

    @Override
    public CompletableFuture<Void> dispose() {
        CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
            syncAccess.dispose();
        });
        return future;
    }
}
