package org.integratedmodelling.aries.core.harmonization;

import java.util.Collection;

import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.interfaces.IObservation;
import org.integratedmodelling.databridge.ObservationFactory;
import org.integratedmodelling.geospace.values.ShapeValue;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.IInstance;
import org.integratedmodelling.thinklab.interfaces.ISession;
import org.integratedmodelling.thinklab.interfaces.ITask;
import org.integratedmodelling.utils.Polylist;

/**
 * HarmonizeObservations task
 * 
 * @input observations a collection of observations (instances) that we want harmonized.
 * @input regionOfInterest a ShapeValue that represents the spatial context of the final observation
 * @input maximumLinearResolution the number of cells we want on the longest axis. 
 * @output harmonizedObservation a single observation with all the passed ones as contingencies, with
 * 		   their states set to represent the common context in the main observation.
 * 
 * @author Ferdinando
 *
 */
public class HarmonizeObservations implements ITask {

	Collection<IInstance> observations = null;
	ShapeValue regionOfInterest = null;
	
	IInstance result = null;
	private int maxLinearResolution;
	
	public void setObservations(Collection<IInstance> observations) {
		this.observations = observations;
	}
	
	public void setRegionOfInterest(ShapeValue where) {
		this.regionOfInterest = where;
	}
	
	public void setMaximumLinearResolution(int res) {
		this.maxLinearResolution = res;
	}
	
	public IInstance getHarmonizedObservation() {
		return result;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {

		/*
		 * create main observation for benefit
		 */
		Polylist dset = ObservationFactory.createIdentification();
		
		/*
		 * add all data as dependencies of the main benefit identification
		 */
		for (IInstance data : observations) {
			dset = ObservationFactory.addDependency(dset, data);
		}
		
		/*
		 * insert desired spatial context
		 */
		dset =
			ObservationFactory.setSpatialContext(
					dset, regionOfInterest, maxLinearResolution);
		
		/*
		 * initialize 
		 */
		IInstance obs = session.createObject(dset);

		IObservation o = (IObservation) obs.getImplementation();
		o.contextualize();
		
		result = obs;		
	}

}
