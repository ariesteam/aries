package org.integratedmodelling.aries.core;

import java.util.HashMap;

import org.integratedmodelling.geospace.literals.ShapeValue;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabResourceNotFoundException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;
import org.integratedmodelling.utils.MiscUtilities;

public class ARIESCorePlugin extends ThinklabPlugin {
	
	HashMap<String, ShapeValue> locations = new HashMap<String, ShapeValue>();
	
	public static final String PLUGIN_ID = 
		"org.integratedmodelling.aries.core";
	
	public static ARIESCorePlugin get() {
		return (ARIESCorePlugin) getPlugin(PLUGIN_ID);
	}
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		loadLocations();
	}

	private void loadLocations() throws ThinklabValidationException {
		
		for (Object p : getProperties().keySet()) {
			
			String prop = p.toString();
			
			if (prop.startsWith("aries.location")) {
				String loc = MiscUtilities.getFileExtension(prop);
				ShapeValue shape = new ShapeValue(getProperties().getProperty(prop));
				locations.put(loc, shape);
			}
		}
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

	public ShapeValue requireLocation(String loc) throws ThinklabResourceNotFoundException {
		ShapeValue ret = locations.get(loc);
		if (loc == null) {
			throw new ThinklabResourceNotFoundException("location " + loc + " is unknown");
		}
		return ret;
	}

	public HashMap<String, ShapeValue> getLocations() {
		return locations;
	}

}
