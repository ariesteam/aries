package org.integratedmodelling.aries.administration;

import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESAdministrationPlugin extends ThinklabPlugin {

	public static final String PLUGIN_ID = "org.integratedmodelling.aries.administration";
	
	public static ARIESAdministrationPlugin get() {
		return (ARIESAdministrationPlugin) getPlugin(PLUGIN_ID);
	}
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

}
