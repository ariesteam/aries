package org.integratedmodelling.aries;

import java.io.File;

import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESPlugin extends ThinklabPlugin {
	
	public static final String PLUGIN_ID = 
		"org.integratedmodelling.aries.aries";
	
	public static ARIESPlugin get() {
		return (ARIESPlugin) getPlugin(PLUGIN_ID);
	}
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		
		/*
		 * official gazetteers
		 */
		Geospace.get().loadGazetteersFromDirectory(getScratchPath());
		
		/*
		 * load any personal gazetteers
		 */
		Geospace.get().loadGazetteersFromDirectory(new File(System.getProperty("user.home")));
		
	}

	@Override
	protected void unload() throws ThinklabException {
	}


}
