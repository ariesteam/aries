package org.integratedmodelling.aries.core.span;

import java.util.Map;

import org.integratedmodelling.corescience.interfaces.IObservation;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

/**
 * Used to talk to the GSSM Clojure side. This is used in a proxy form to yield an object that can be used
 * to run GSSM from Java.
 * 
 * @author Ferdinando Villa
 *
 */
public interface SPANProxy {

	/**
	 * Run a GSSM simulation from four observations of source, sink, use and flow. Choose the flow model 
	 * according to the observable of the flow observation.
	 * 
	 * @param source
	 * @param use
	 * @param sink
	 * @param flow
	 * @return a flow map, from which results can be obtained.
	 */
	public abstract Map<?,?> runSPAN(
			IObservation observation, 
			IConcept sourceConcept, IConcept useConcept, IConcept sinkConcept, IConcept flowConcept,
			Map<?,?> flowParams);
	
}
