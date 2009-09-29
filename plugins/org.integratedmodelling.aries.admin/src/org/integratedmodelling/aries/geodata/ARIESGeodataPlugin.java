package org.integratedmodelling.aries.geodata;

import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESGeodataPlugin extends ThinklabPlugin {

	public static final String ID = "org.integratedmodelling.aries.geodata";
	
	public static ARIESGeodataPlugin get() {
		return (ARIESGeodataPlugin) getPlugin(ID);
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
