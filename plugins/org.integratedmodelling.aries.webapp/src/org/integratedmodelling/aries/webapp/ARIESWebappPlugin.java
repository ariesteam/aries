package org.integratedmodelling.aries.webapp;

import org.integratedmodelling.thinkcap.core.ThinkcapPlugin;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESWebappPlugin extends ThinkcapPlugin {

	public static final String PLUGIN_ID = 
		"org.integratedmodelling.aries.webapp";
	
	public ARIESWebappPlugin get() {
		return (ARIESWebappPlugin) getPlugin(PLUGIN_ID);
	}
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		// TODO Auto-generated method stub

	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

}
