package is.yaks.rest.utils;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.google.gson.Gson;
import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.client.urlconnection.URLConnectionClientHandler;

public class YaksConfiguration {

	private String yaksUrl;
	private ExecutorService executorService = Executors.newFixedThreadPool(5);
	private Client client;
	private Gson gson = new Gson();

	private YaksConfiguration() {
		DefaultClientConfig configClient = new DefaultClientConfig();
		configClient.getProperties().put(URLConnectionClientHandler.PROPERTY_HTTP_URL_CONNECTION_SET_METHOD_WORKAROUND, true);
		client = Client.create(configClient);		
	}
     
	// first load at the call of YaksConfiguration.getInstance()
	// work in multithread env
    private static class YaksConfigurationHolder
    {
        private final static YaksConfiguration instance = new YaksConfiguration();
    }

    public static YaksConfiguration getInstance()
    {
        return YaksConfigurationHolder.instance;
    }

	public Client getClient() {
		return client;
	}

	public String getYaksUrl() {
		return this.yaksUrl;
	}

	public void setYaksUrl(String yaksUrl) {
		this.yaksUrl = yaksUrl;		
	}

	public ExecutorService getExecutorService() {		
		return executorService ;
	}
	
	public Gson getGson() {
		return gson;
	}
}
