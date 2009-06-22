package org.integratedmodelling.aries.valuation;

import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.Thinklab;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESValuationPlugin extends ThinklabPlugin {

	public static final String ID = "org.integratedmodelling.aries.valuation";
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		// TODO Auto-generated method stub

	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

	public static ARIESValuationPlugin get() {
		return (ARIESValuationPlugin) Thinklab.getPlugin(ID);
	}
	
}
