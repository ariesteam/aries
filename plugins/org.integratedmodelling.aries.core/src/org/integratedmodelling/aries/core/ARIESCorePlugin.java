package org.integratedmodelling.aries.core;

import org.integratedmodelling.aries.core.datastructures.demo.ARIESDemoKbox;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.gazetteers.SimpleGazetteer;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.thinklab.kbox.KBoxManager;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;
import org.integratedmodelling.thinklab.transformations.TransformationFactory;

public class ARIESCorePlugin extends ThinklabPlugin {
	
	static public final String ARIES_KBOX_PROPERTY = "aries.kbox";
	
	IKBox workingKbox = null;
	
	
	
	public static final String PLUGIN_ID = 
		"org.integratedmodelling.aries.core";
	
	public static ARIESCorePlugin get() {
		return (ARIESCorePlugin) getPlugin(PLUGIN_ID);
	}
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {

		Geospace.get().addGazetteer("aries-inline", new SimpleGazetteer(getProperties()));
//		KBoxManager.get().installKbox(
//				"kbox://www.integratedmodelling.org/aries/demo", 
//				new ARIESDemoKbox());
		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		TransformationFactory.get().loadTransformationMappings(getProperties());
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub
	}

	public IKBox getWorkingKBox() throws ThinklabException {

		if (workingKbox == null) {
			
			String kboxname = getProperties().getProperty(ARIES_KBOX_PROPERTY);
			if (kboxname != null)
				workingKbox = KBoxManager.get().requireGlobalKBox(kboxname);
			if (workingKbox /* still */ == null) {
				workingKbox = KBoxManager.get();
			}
		}
		return workingKbox;
	}
	
}
