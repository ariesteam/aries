package org.integratedmodelling.aries.core.implementations.observations;

import java.util.ArrayList;
import java.util.HashSet;

import org.integratedmodelling.aries.core.gssm.GSSMProxy;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.implementations.observations.Observation;
import org.integratedmodelling.corescience.interfaces.IObservation;
import org.integratedmodelling.corescience.interfaces.IObservationContext;
import org.integratedmodelling.corescience.interfaces.internal.TransformingObservation;
import org.integratedmodelling.corescience.literals.GeneralClassifier;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.annotations.InstanceImplementation;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.Polylist;

/**
 * Support for running the Clojure SPAN models and making lazy observation proxies to its
 * results.  
 * @author Ferdinando
 */
@InstanceImplementation(concept="aries:GSSMTransformer")
public class GSSMTransformer 
	extends Observation 
	implements TransformingObservation {
	
	// these are all specialized dependencies
	public static final String SOURCE_OBS_PROPERTY = "aries:hasSourceObservation";
	public static final String SINK_OBS_PROPERTY   = "aries:hasSinkObservation";
	public static final String USE_OBS_PROPERTY    = "aries:hasUseObservation";
	public static final String FLOW_OBS_PROPERTY   = "aries:hasFlowObservation";
	
	private static GSSMProxy gssm = null;
	
	private IObservation source = null;
	private IObservation sink = null;
	private IObservation use = null;
	private IObservation flow = null;
	
	private double sink_threshold = 0.0;
	private double use_threshold = 0.0;
	private double source_threshold = 0.0;
	private double decay_rate = 0.0;
	
	/*
	 * called by the Clojure side to give us a bridge to the gssm system
	 */
	public static void setGSSMProxy(GSSMProxy proxy) {
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
	}

	@Override
	public IObservationContext getTransformedContext(IObservationContext context)
			throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IConcept getTransformedObservationClass() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IInstance transform(IInstance sourceObs, ISession session,
			IObservationContext context) throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}
}