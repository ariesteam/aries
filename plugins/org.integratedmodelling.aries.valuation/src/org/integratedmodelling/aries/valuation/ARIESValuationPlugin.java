package org.integratedmodelling.aries.valuation;

import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;
import org.integratedmodelling.thinklab.transformations.TransformationFactory;

public class ARIESValuationPlugin extends ThinklabPlugin {

	public static final String ID = "org.integratedmodelling.aries.valuation";
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		TransformationFactory.get().loadTransformationMappings(getProperties());
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

	public static ARIESValuationPlugin get() {
		return (ARIESValuationPlugin)getPlugin(ID);
	}
	
}
