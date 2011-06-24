package org.integratedmodelling.aries.marine;

import org.integratedmodelling.aries.marine.spank.SubsistenceFisheries;
import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.modelling.agents.SPANK;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESMarinePlugin extends ThinklabPlugin {

	// concepts of top-level models of interest
	public final static String subsistenceModel = "fisheries:SubsistenceFishProvision";
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {

		Metadata.loadPredefinedOrderings(getProperties());
		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		SPANK.registerSpankClass(
				km.requireConcept(subsistenceModel), SubsistenceFisheries.class);
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

}
