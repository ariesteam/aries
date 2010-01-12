package org.integratedmodelling.aries.core.implementations.observations;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map;

import org.integratedmodelling.aries.core.ARIESNamespace;
import org.integratedmodelling.aries.core.span.SPANProxy;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.implementations.observations.Observation;
import org.integratedmodelling.corescience.interfaces.IObservation;
import org.integratedmodelling.corescience.interfaces.IObservationContext;
import org.integratedmodelling.corescience.interfaces.internal.TransformingObservation;
import org.integratedmodelling.corescience.literals.GeneralClassifier;
import org.integratedmodelling.modelling.ObservationFactory;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.InstanceImplementation;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.utils.Pair;

/**
 * Support for running the Clojure SPAN models and making lazy observation proxies to its
 * results.  
 * @author Ferdinando
 */
@InstanceImplementation(concept="aries:SPANTransformer")
public class SPANTransformer 
	extends Observation 
	implements TransformingObservation {
	
	private static SPANProxy span = null;
	
	// the following 5 fields are set at instance creation through reflection, as 
	// directed by SPANModel
	IConcept sourceConcept = null;
	IConcept sinkConcept = null;
	IConcept useConcept = null;
	IConcept flowConcept = null;
	Map<?,?> flowParams;
	
	/*
	 * called by the Clojure side to give us a bridge to the SPAN system
	 */
	public static void setSPANProxy(SPANProxy proxy) {
		span = proxy;
	}
	
	ArrayList<Pair<GeneralClassifier, IConcept>> classifiers = 
		new ArrayList<Pair<GeneralClassifier,IConcept>>();
	
	HashSet<IConcept> outputStates = new HashSet<IConcept>();
	IConcept cSpace = null;
	
	@Override
	public IObservationContext getTransformedContext(IObservationContext context)
			throws ThinklabException {
		return context;
	}

	@Override
	public IConcept getTransformedObservationClass() {
		return CoreScience.Observation();
	}

	@Override
	public IInstance transform(IInstance sourceObs, ISession session,
			IObservationContext context) throws ThinklabException {

		System.out.println("eccomi");
		
		/*
		 * run the proxy, obtain a map of closures 
		 */
		Map<?,?> closures = 
			span.runSPAN(
				ObservationFactory.getObservation(sourceObs), 
				sourceConcept, 
				useConcept,
				sinkConcept, 
				flowConcept, 
				flowParams);
		/*
		 * create dependencies by making datasources from closures
		 */
		for (Object k : closures.keySet()) {
			
		}
		
		return null;
		
	}
}