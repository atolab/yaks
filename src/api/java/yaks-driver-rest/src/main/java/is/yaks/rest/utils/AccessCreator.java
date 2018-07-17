package is.yaks.rest.utils;

import java.lang.reflect.Type;

import com.google.gson.InstanceCreator;

import is.yaks.Access;
import is.yaks.Yaks;

public class AccessCreator implements InstanceCreator<Access> {

	/*
	 *	Could be used like this
	 *	Needed when instancing new AccessImpl class from REST data and:
	 *		1) setting up new created AccessImpl class with API context
	 *  	2) updating API Context   

		GsonBuilder gsonBuilder = new GsonBuilder();
		gsonBuilder.registerTypeAdapter(Access.class, new AccessCreator());
		Gson gson  = gsonBuilder.create();
	 */


	private Yaks yaks;

	public AccessCreator() {
		//TODO change/add constructor if needed 
	}

	public AccessCreator(Yaks yaksImpl) {
		this.yaks = yaksImpl;
	}

	@Override
	public Access createInstance(Type type) {
		//AccessImpl access = new AccessImpl();	
		return null;
	}		
}