package org.integratedmodelling.aries.core.implementations.observations;

import java.util.ArrayList;
import java.util.HashSet;

import org.integratedmodelling.aries.core.gssm.GSSMProxy;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.implementations.observations.Observation;
import org.integratedmodelling.corescience.interfaces.cmodel.IConceptualModel;
import org.integratedmodelling.corescience.interfaces.cmodel.TransformingConceptualModel;
import org.integratedmodelling.corescience.interfaces.context.IObservationContext;
import org.integratedmodelling.corescience.interfaces.data.IContextualizedState;
import org.integratedmodelling.corescience.interfaces.data.IDataSource;
import org.integratedmodelling.corescience.interfaces.data.IStateAccessor;
import org.integratedmodelling.corescience.interfaces.observation.IObservation;
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
 * Support for the modelling/bayesian form.
 * 
 * @author Ferdinando
 */
@InstanceImplementation(concept="aries:GSSMTransformer")
public class GSSMTransformer 
	extends Observation 
	implements IConceptualModel, TransformingConceptualModel {
	
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
	public IStateAccessor getStateAccessor(IConcept stateType,
			IObservationContext context) {
		// we contextualize as an identification, so no accessor is required.
		return null;
	}

	@Override
	public IConcept getStateType() {
		return cSpace;
	}

	@Override
	public void initialize(IInstance i) throws ThinklabException {
		super.initialize(i);
		
		/*
		 * recover all observations from the dependencies.
		 */
	}
	
	@Override
	public void handshake(IDataSource<?> dataSource,
			IObservationContext observationContext,
			IObservationContext overallContext) throws ThinklabException {
	}

	@Override
	public void validate(IObservation observation)
			throws ThinklabValidationException {
	}

	@Override
	public IContextualizedState createContextualizedStorage(int size)
			throws ThinklabException {
		// we contextualize this as an identification, so no storage is needed. 
		return null;
	}

	@Override
	public Polylist getTransformedConceptualModel() {
		// this should be OK, we just transform to an identification.
		return null;
	}

	@Override
	public IConcept getTransformedObservationClass() {
		return CoreScience.Observation();
	}

	@Override
	public IInstance transformObservation(IInstance inst, ISession session)
			throws ThinklabException {
		

		return null;
	}
}