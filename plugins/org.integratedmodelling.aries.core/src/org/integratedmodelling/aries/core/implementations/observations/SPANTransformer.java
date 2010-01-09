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
	
	private static SPANProxy gssm = null;
	
	// these are taken from the dependencies after analyzing the observables
	private IObservation source = null;
	private IObservation sink = null;
	private IObservation use = null;
	private IObservation flow = null;

	
	// this is set at instance creation through reflection
	Map<?,?> flowParams;
	
	/*
	 * called by the Clojure side to give us a bridge to the gssm system
	 */
	public static void setSPANProxy(SPANProxy proxy) {
		gssm = proxy;
	}
	
	ArrayList<Pair<GeneralClassifier, IConcept>> classifiers = 
		new ArrayList<Pair<GeneralClassifier,IConcept>>();
	
	HashSet<IConcept> outputStates = new HashSet<IConcept>();
	IConcept cSpace = null;
	
	@Override
	public void initialize(IInstance i) throws ThinklabException {
		super.initialize(i);
		
		/*
		 * recover all observations from the dependencies.
		 */
		for (IObservation o : getDependencies()) {
			if (o.getObservable().is(ARIESNamespace.BENEFIT_SOURCE_TYPE))
				source = o;
			else if (o.getObservable().is(ARIESNamespace.BENEFIT_USE_TYPE))
				use = o;
			else if (o.getObservable().is(ARIESNamespace.BENEFIT_SINK_TYPE))
				sink = o;
			else 
				/*
				 * TODO we should accommodate more than one "other" dependency,
				 * and merge them all into a single observation.
				 */
				flow = o;
		}
	}

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

		/*
		 * run the proxy, obtain a map of closures 
		 */
		Map<?,?> closures = 
			gssm.runSPAN(
				ObservationFactory.getObservation(sourceObs), 
				source == null ? null : source.getObservableClass(), 
				use == null ? null : use.getObservableClass(),
				sink == null ? null : sink.getObservableClass(), 
				flow == null ? null : flow.getObservableClass(), 
				flowParams);
		/*
		 * create dependencies by passing the closures
		 */
		for (Object k : closures.keySet()) {
			
		}
		
		return null;
		
	}
}