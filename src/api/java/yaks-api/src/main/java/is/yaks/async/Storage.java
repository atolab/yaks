package is.yaks.async;

import java.util.concurrent.CompletableFuture;

public interface Storage {

    /**
     * Dispose the Storage.
     */
    public CompletableFuture<Void> dispose();
}