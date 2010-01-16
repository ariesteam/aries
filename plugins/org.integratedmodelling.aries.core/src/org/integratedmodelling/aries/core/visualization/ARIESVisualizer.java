package org.integratedmodelling.aries.core.visualization;

import org.integratedmodelling.aries.core.ARIESNamespace;
import org.integratedmodelling.corescience.interfaces.IObservation;
import org.integratedmodelling.modelling.visualization.FileBasedDataset;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

public class ARIESVisualizer extends FileBasedDataset {

	public ARIESVisualizer(IObservation obs) throws ThinklabException {
		super(obs);
	}

	@Override
	public int getColorForConcept(IConcept observable) {
		
		if (observable.is(ARIESNamespace.BENEFIT_SOURCE_TYPE)) 
			return GREEN;
		else if (observable.is(ARIESNamespace.BENEFIT_USE_TYPE)) 
			return BLUE;
		else if (observable.is(ARIESNamespace.BENEFIT_SINK_TYPE)) 
			return YELLOW;
		else if (observable.is(ARIESNamespace.BENEFIT_FLOW_TYPE)) 
			return GREY;
		
		return super.getColorForConcept(observable);
	}

	
	
}
