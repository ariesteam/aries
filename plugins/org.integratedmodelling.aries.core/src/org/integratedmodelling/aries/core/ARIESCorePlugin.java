package org.integratedmodelling.aries.core;

import org.integratedmodelling.aries.core.datastructures.demo.ARIESDemoKbox;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.gazetteers.SimpleGazetteer;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.kbox.KBoxManager;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESCorePlugin extends ThinklabPlugin {
	
	public static final String PLUGIN_ID = 
		"org.integratedmodelling.aries.core";
	
	public static ARIESCorePlugin get() {
		return (ARIESCorePlugin) getPlugin(PLUGIN_ID);
	}
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {

		Geospace.get().addGazetteer("aries-inline", new SimpleGazetteer(getProperties()));
		KBoxManager.get().installKbox(
				"kbox://www.integratedmodelling.org/aries/demo", 
				new ARIESDemoKbox());
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub
	}
	
}
