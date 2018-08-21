package is.yaks.rest;

import java.net.HttpURLConnection;

import javax.ws.rs.core.MediaType;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import is.yaks.Storage;
import is.yaks.rest.utils.Utils;
import is.yaks.rest.utils.YaksConfiguration;

public class StorageImpl implements Storage {

    private YaksConfiguration config = YaksConfiguration.getInstance();

    private String storageId;

    private String alias;

    StorageImpl(String storageId) {
        this(storageId, null);
    }

    StorageImpl(String storageId, String alias) {
        this.storageId = storageId;
        this.alias = alias;
    }

    @Override
    public void dispose() {
        assert storageId != null;
        WebResource wr = config.getClient().resource(config.getYaksUrl()).path("/yaks/storages/" + storageId);

        ClientResponse response = wr.accept(MediaType.APPLICATION_JSON_TYPE).delete(ClientResponse.class);

        switch (response.getStatus()) {
        case HttpURLConnection.HTTP_NO_CONTENT:
            return;
        case HttpURLConnection.HTTP_NOT_FOUND:
        default:
            Utils.fail("Storage dispose failed with\n code: " + response.getStatus() + "\nbody: "
                    + response.getEntity(String.class));

        }
    }

    public String getStorageId() {
        return storageId;
    }

    public String getAlias() {
        return alias;
    }
}
