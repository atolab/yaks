package is.yaks.websocket;

import java.util.Map;
import java.util.function.Function;

import is.yaks.Access;
import is.yaks.Listener;
import is.yaks.Path;
import is.yaks.Selector;

public class AccessImpl implements Access {

	@Override
	public Access put(Selector selector, Object value) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Access deltaPut(Selector selector, Object delta) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Access remove(Selector selector) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Long subscribe(Selector selector) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> Long subscribe(Selector selector, Listener<T> listener) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, Selector> getSubscriptions() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void unsubscribe(long subscriptionId) {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> Map<Path, T> get(Selector Selector, Class<T> c) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> T get(Path path, Class<T> c) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void eval(Selector selector, Function<Path, Object> computation) {
		// TODO Auto-generated method stub

	}

	@Override
	public void close() {
		// TODO Auto-generated method stub

	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

}
