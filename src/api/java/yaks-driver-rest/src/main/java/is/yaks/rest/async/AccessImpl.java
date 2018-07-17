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

	//create access
	public AccessImpl(String accessId, Path scopePath, long cacheSize) {
		syncAccess = new is.yaks.rest.AccessImpl(accessId, scopePath, cacheSize);
	}

	@Override
	public CompletableFuture<Access> put(Selector selector, Object value) {
		CompletableFuture<Access> future = CompletableFuture.supplyAsync(()->{
			return (Access) syncAccess.put(selector, value);
		});
		return future;
	}

	@Override
	public CompletableFuture<Access> deltaPut(Selector selector, Object delta) {
		CompletableFuture<Access> future = CompletableFuture.supplyAsync(()->{
			return (Access) syncAccess.deltaPut(selector, delta);
		});
		return future;
	}

	@Override
	public CompletableFuture<Access> remove(Selector selector) {
		CompletableFuture<Access> future = CompletableFuture.supplyAsync(()->{
			return (Access) syncAccess.remove(selector);
		});
		return future;
	}

	@Override
	public CompletableFuture<Long> subscribe(Selector selector) {
		CompletableFuture<Long> future = CompletableFuture.supplyAsync(()->{
			return syncAccess.subscribe(selector);
		});
		return future;
	}

	@Override
	public <T> CompletableFuture<Long> subscribe(Selector selector, Listener<T> listener) {
		CompletableFuture<Long> future = CompletableFuture.supplyAsync(()->{			 
			return syncAccess.subscribe(selector, listener);
		});
		return future;
	}

	@Override
	public CompletableFuture<Map<String, Selector>> getSubscriptions() {
		CompletableFuture<Map<String, Selector>> future = CompletableFuture.supplyAsync(()->{
			return syncAccess.getSubscriptions();
		});
		return future;
	}

	@Override
	public CompletableFuture<Void> unsubscribe(long sid) {
		CompletableFuture<Void> future = CompletableFuture.runAsync(new Runnable() {
			public void run() {
				syncAccess.unsubscribe(sid);
			}			
		});
		return future;
	}

	@Override
	public <T> CompletableFuture<Map<Path, T>> get(Selector selector, Class<T> c) {
		CompletableFuture<Map<Path, T>> future = CompletableFuture.supplyAsync(()->{			
			return syncAccess.get(selector, c);			
		});
		return future;
	}

	@Override
	public <T> CompletableFuture<T> get(Path path, Class<T> c) {
		CompletableFuture<T> future = CompletableFuture.supplyAsync(()->{			
			return syncAccess.get(path, c);			
		});
		return future;
	}

	@Override
	public CompletableFuture<Void> eval(Selector selector, Function<Path, Object> computation) {
		CompletableFuture<Void> future = CompletableFuture.runAsync(new Runnable() {
			public void run() {
				syncAccess.eval(selector, computation);
			}
		});

		return future;
	}

	@Override
	public CompletableFuture<Void> close() {
		CompletableFuture<Void> future = CompletableFuture.runAsync(new Runnable() {
			public void run() {
				syncAccess.close();
			}
		});
		return future;
	}

	@Override
	public CompletableFuture<Void> dispose() {
		CompletableFuture<Void> future = CompletableFuture.runAsync(new Runnable() {
			public void run() {
				syncAccess.dispose();
			}
		});
		return future;
	}
}
