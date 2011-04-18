package org.integratedmodelling.aries.webapp.webapps;

import org.integratedmodelling.thinklab.http.ThinklabWebApplication;
import org.integratedmodelling.thinklab.http.extensions.WebApplication;
import org.java.plugin.Plugin;
import org.java.plugin.registry.Extension;


@WebApplication(
		name="aries", 
		description="ARIES Ecosystem Services Explorer")
public class ARIESWebApplication extends ThinklabWebApplication {

	public ARIESWebApplication(Plugin plugin, Extension ext) {
		super(plugin, ext);
		// TODO Auto-generated constructor stub
	}

}
