package org.integratedmodelling.aries.admin;

import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESAdminPlugin extends ThinklabPlugin {

	public static final String ID = "org.integratedmodelling.aries.admin";
	
	public static ARIESAdminPlugin get() {
		return (ARIESAdminPlugin) getPlugin(ID);
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
