package org.integratedmodelling.aries.core.visualization;

import org.integratedmodelling.corescience.interfaces.IObservation;
import org.integratedmodelling.modelling.visualization.FileBasedDataset;
import org.integratedmodelling.thinklab.exception.ThinklabException;

public class ARIESVisualizer extends FileBasedDataset {
	
	public ARIESVisualizer(IObservation obs) throws ThinklabException {
		super(obs);
	}	
	
}
