package is.yaks.rest;

import java.net.HttpURLConnection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import javax.ws.rs.core.Cookie;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.SerializedName;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import is.yaks.Access;
import is.yaks.Listener;
import is.yaks.Path;
import is.yaks.Selector;
import is.yaks.rest.utils.GsonTypeToken;
import is.yaks.rest.utils.Utils;
import is.yaks.rest.utils.YaksConfiguration;

public class AccessImpl implements Access {

    @Expose(serialize = true, deserialize = true)
    @SerializedName(value = "accessId", alternate = { "id" })
    private String accessId;

    @Expose(serialize = true, deserialize = true)
    @SerializedName(value = "alias")
    private String alias;

    @Expose(serialize = true, deserialize = true)
    @SerializedName(value = "path", alternate = { "scopePath" })
    private String scopePath;

    @Expose(serialize = true, deserialize = true)
    @SerializedName(value = "cacheSize", alternate = { "cache", "size" })
    private long cacheSize;

    // no modifier, only visible in class and package
    AccessImpl(String accessId, Path scopePath, long cacheSize) {
        this(accessId, null, scopePath, cacheSize);
    }

    AccessImpl(String accessId, String alias, Path scopePath, long cacheSize) {
        this.accessId = accessId;
        this.alias = alias;
        this.scopePath = scopePath.toString();
        this.cacheSize = cacheSize;
    }

    private YaksConfiguration getConfig() {
        return YaksConfiguration.getInstance();
    }

    @Override
    public Access put(Selector selector, Object value) {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl()).path(selector.toString());
        ClientResponse response = wr.cookie(new Cookie(Utils.IS_YAKS_ACCESS, accessId))
                .type(MediaType.APPLICATION_JSON_TYPE).accept(MediaType.APPLICATION_JSON_TYPE)
                .put(ClientResponse.class, getConfig().getGson().toJson(value));

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_NO_CONTENT:
            return this;
        case HttpURLConnection.HTTP_BAD_REQUEST:
        case HttpURLConnection.HTTP_PRECON_FAILED:
        default:
            Utils.fail("Failed to put data :\ncode: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));
            return null;
        }
    }

    @Override
    public Access deltaPut(Selector selector, Object delta) {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl()).path(selector.toString());
        ClientResponse response = wr.cookie(new Cookie(Utils.IS_YAKS_ACCESS, accessId))
                .type(MediaType.APPLICATION_JSON_TYPE).accept(MediaType.APPLICATION_JSON_TYPE)
                .method("PATCH", ClientResponse.class, getConfig().getGson().toJson(delta));

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_NO_CONTENT:
            return this;
        case HttpURLConnection.HTTP_BAD_REQUEST:
        case HttpURLConnection.HTTP_PRECON_FAILED:
        default:
            Utils.fail("Failed to put data :\ncode: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));
            return null;
        }
    }

    @Override
    public Access remove(Selector selector) {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl()).path(selector.toString());
        ClientResponse response = wr.type(MediaType.APPLICATION_JSON_TYPE).accept(MediaType.APPLICATION_JSON_TYPE)
                .cookie(new Cookie(Utils.IS_YAKS_ACCESS, accessId)).delete(ClientResponse.class);

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_NO_CONTENT:
            return this;
        case HttpURLConnection.HTTP_BAD_REQUEST:
        case HttpURLConnection.HTTP_PRECON_FAILED:
        default:
            Utils.fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));
            return null;
        }
    }

    @Override
    public Long subscribe(Selector selector) {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl())
                .path("/yaks/access/" + accessId + "/subs").queryParam("selector", selector.toString());
        ClientResponse response = wr.type(MediaType.APPLICATION_JSON_TYPE).accept(MediaType.APPLICATION_JSON_TYPE)
                .post(ClientResponse.class);

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_CREATED:
            MultivaluedMap<String, String> headers = response.getHeaders();
            String headerLocation = Utils.getHeader(headers, "Location");
            assert !String.valueOf(headerLocation).isEmpty();
            return new Long(headerLocation);
        case HttpURLConnection.HTTP_PRECON_FAILED:
        default:
            Utils.fail("Failed to subscribe to selector:\ncode: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));
            return null;
        }
    }

    @Override
    public <T> Long subscribe(Selector selector, Listener<T> listener) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, Selector> getSubscriptions() {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl())
                .path("/yaks/access/" + accessId + "/subs");
        ClientResponse response = wr.type(MediaType.APPLICATION_JSON_TYPE).accept(MediaType.APPLICATION_JSON_TYPE)
                .get(ClientResponse.class);

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_OK:
            return getConfig().getGson().fromJson(response.getEntity(String.class),
                    GsonTypeToken.getInstance().<String, Selector> getMapTypeToken());
        default:
        case HttpURLConnection.HTTP_NOT_FOUND:
            Utils.fail("Failed to get subscrition list :\ncode: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));
            return null;
        }
    }

    @Override
    public void unsubscribe(long subscriptionId) {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl())
                .path("/yaks/access/" + accessId + "/subs/" + subscriptionId);
        ClientResponse response = wr.type(MediaType.APPLICATION_JSON_TYPE).accept(MediaType.APPLICATION_JSON_TYPE)
                .delete(ClientResponse.class);

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_NO_CONTENT:
            return;
        case HttpURLConnection.HTTP_NOT_FOUND:
        default:
            Utils.fail("Failed to unsubscribe:\ncode: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));
            return;
        }

    }

    @Override
    public <T> Map<Path, T> get(Selector selector, Class<T> c) {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl()).path(selector.toString());
        ClientResponse response = wr.accept(MediaType.APPLICATION_JSON_TYPE)
                .cookie(new Cookie(Utils.IS_YAKS_ACCESS, accessId)).get(ClientResponse.class);

        String data = response.getEntity(String.class);
        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_OK:
            // TODO: replace this tmp Map with a smarter Gson decoder that directly
            // constructs a Map<Path, T> from {"k1":"v1","k2":"v2"}
            Map<String, T> tmp = getConfig().getGson().fromJson(data,
                    GsonTypeToken.getInstance().<String, T> getMapTypeToken());
            Map<Path, T> result = new HashMap<Path, T>();
            for (Map.Entry<String, T> e : tmp.entrySet()) {
                result.put(Path.ofString(e.getKey()), e.getValue());
            }

            return result;
        case HttpURLConnection.HTTP_NOT_FOUND:
        case HttpURLConnection.HTTP_BAD_REQUEST:
        case HttpURLConnection.HTTP_PRECON_FAILED:
        default:
            Utils.fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus() + "\nbody: "
                    + data);
            return null;
        }
    }

    @Override
    public <T> T get(Path path, Class<T> c) {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl()).path(path.toString());
        ClientResponse response = wr.accept(MediaType.APPLICATION_JSON_TYPE)
                .cookie(new Cookie(Utils.IS_YAKS_ACCESS, accessId)).get(ClientResponse.class);

        String data = response.getEntity(String.class);
        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_OK:
            Map<Path, T> tmp = getConfig().getGson().fromJson(data,
                    GsonTypeToken.getInstance().<Path, T> getMapTypeToken());
            // because tmp.get(path) returned an internal TreeMap for Foo object
            String tmpString = getConfig().getGson().toJson(tmp.get(path));
            return getConfig().getGson().fromJson(tmpString, c);
        case HttpURLConnection.HTTP_NOT_FOUND:
        case HttpURLConnection.HTTP_BAD_REQUEST:
        case HttpURLConnection.HTTP_PRECON_FAILED:
        default:
            Utils.fail("Failed to get data map corresponding to selector:\ncode: " + response.getStatus() + "\nbody: "
                    + data);
            return null;
        }
    }

    @Override
    public void eval(Selector selector, Function<Path, Object> computation) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void close() {
        // nothing to do
    }

    @Override
    public void dispose() {
        WebResource wr = getConfig().getClient().resource(getConfig().getYaksUrl()).path("/yaks/access/" + accessId);
        ClientResponse response = wr.accept(MediaType.APPLICATION_JSON_TYPE).delete(ClientResponse.class);

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_NO_CONTENT:
            break;
        case HttpURLConnection.HTTP_NOT_FOUND:
        default:
            Utils.fail("Access dispose failed with\n code: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));
        }
    }

    public String getAccessId() {
        return accessId;
    }

    public String getAlias() {
        return alias;
    }

    public String getScopePath() {
        return scopePath;
    }

    public long getCacheSize() {
        return cacheSize;
    }

}
