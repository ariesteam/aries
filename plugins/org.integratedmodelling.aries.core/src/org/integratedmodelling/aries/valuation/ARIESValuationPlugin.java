package org.integratedmodelling.aries.valuation;

import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;
import org.integratedmodelling.thinklab.transformations.TransformationFactory;

public class ARIESValuationPlugin extends ThinklabPlugin {

	public static final String ID = "org.integratedmodelling.aries.valuation";
	static IConcept esvconcept = null;
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		
		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		TransformationFactory.get().loadTransformationMappings(getProperties());
		
		esvconcept = km.requireConcept("esvaluation:HistoricESValue");
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

	public static ARIESValuationPlugin get() {
		return (ARIESValuationPlugin)getPlugin(ID);
	}
	
	public static IConcept ESVConcept() {
		return esvconcept;
	}
	
}
