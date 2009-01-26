package org.integratedmodelling.aries.core.tasks.harmonization;

import java.util.Map;

import org.integratedmodelling.corescience.interfaces.observation.IObservation;
import org.integratedmodelling.geospace.values.ShapeValue;
import org.integratedmodelling.modelling.observations.ObservationFactory;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.applications.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
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
@TaskNamespace(ns = "aries")
public class HarmonizeObservations implements ITask {

	Map<IConcept, IInstance> observations = null;
	ShapeValue regionOfInterest = null;
	
	IInstance result = null;
	private int maxLinearResolution;
	private IConcept observable;
	
	public void setObservable(IConcept observable) {
		this.observable = observable;
	}
	
	public void setObservations(Map<IConcept, IInstance> observations) {
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
		Polylist dset = ObservationFactory.createIdentification(observable.toString());
		
		/*
		 * add all data as dependencies of the main benefit identification
		 */
		for (IInstance data : observations.values()) {
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
