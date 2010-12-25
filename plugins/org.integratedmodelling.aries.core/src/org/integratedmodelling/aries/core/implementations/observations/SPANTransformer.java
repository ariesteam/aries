package org.integratedmodelling.aries.core.implementations.observations;

import java.util.ArrayList;
import java.util.Map;

import org.integratedmodelling.aries.core.ARIESNamespace;
import org.integratedmodelling.aries.core.implementations.datasources.SPANDistributionState;
import org.integratedmodelling.aries.core.span.SPANProxy;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.context.ObservationContext;
import org.integratedmodelling.corescience.implementations.observations.Observation;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.corescience.interfaces.IExtent;
import org.integratedmodelling.corescience.interfaces.IObservationContext;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.corescience.interfaces.internal.TransformingObservation;
import org.integratedmodelling.corescience.literals.GeneralClassifier;
import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.extents.GridExtent;
import org.integratedmodelling.modelling.ModelFactory;
import org.integratedmodelling.modelling.ObservationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabMalformedSemanticTypeException;
import org.integratedmodelling.thinklab.exception.ThinklabNoKMException;
import org.integratedmodelling.thinklab.exception.ThinklabResourceNotFoundException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.annotations.InstanceImplementation;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.thinklab.interfaces.knowledge.IRelationship;
import org.integratedmodelling.utils.CamelCase;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.Polylist;

import clojure.lang.IFn;

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
	public IConcept sourceConcept = null;
	public IConcept sinkConcept = null;
	public IConcept useConcept = null;
	public IConcept flowConcept = null;
	public IConcept flowDataConcept = null;
	public Map<?,?> flowParams;
	
	/*
	 * called by the Clojure side to give us a bridge to the SPAN system
	 */
	public static void setSPANProxy(SPANProxy proxy) {
		span = proxy;
	}
	
	@Override
	public void initialize(IInstance i) throws ThinklabException {

		super.initialize(i);
		
		
		/*
		 * read the result concepts we want to compute after the flows have
		 * been computed
		 */
		for (IRelationship r : i.getRelationships(ModelFactory.RETAINS_STATES)) {
			outputStates.add(KnowledgeManager.get().requireConcept(r.getValue().toString()));
		}
		
	}
	
	ArrayList<Pair<GeneralClassifier, IConcept>> classifiers = 
		new ArrayList<Pair<GeneralClassifier,IConcept>>();
	
	ArrayList<IConcept> outputStates = new ArrayList<IConcept>();
	IConcept cSpace = null;
	
	@Override
	public IConcept getTransformedObservationClass() {
		return CoreScience.Observation();
	}

	/**
	 * Return the observable concept in the flow's concept space that matches
	 * the keyword: e.g. if we get actual-source, we look for a child of the
	 * source concept that is subsumed by the corresponding trait
	 * (eserv:ActualSource) and return it. If there's no such concept, we return
	 * null, meaning that this concept makes no sense for this model and
	 * therefore a state should not be created for it.
	 * 
	 * @param keyword
	 * @return
	 * @throws ThinklabNoKMException
	 * @throws ThinklabResourceNotFoundException
	 * @throws ThinklabMalformedSemanticTypeException
	 */
	public IConcept defineObservable(String keyword) throws ThinklabException {

		String cn = "eserv:" + CamelCase.toUpperCamelCase(keyword, '-');
		IConcept concept = KnowledgeManager.get().requireConcept(cn);
		IConcept ret = null;

//		if (concept.is(ARIESNamespace.SOURCE_TRAIT)) {
//			source = sourceConcept;
//		} else if (concept.is(ARIESNamespace.USE_TRAIT)) {
//			source = useConcept;
//		} else if (concept.is(ARIESNamespace.SINK_TRAIT)) {
//			source = sinkConcept;
//		} else if (concept.is(ARIESNamespace.FLOW_TRAIT)) {
//			source = flowConcept;
//		}
//		
		/*
		 * new simpler strategy: the modeler names the concepts she wants
		 * computed, which must derive from the given traits. If any of the
		 * keepers are children of that trait, we compute it.
		 */
		for (IConcept c : outputStates) {
			if (c.is(concept)) {
				ret = c;
				break;
			}
		}

		return ret;
	}

	@Override
	public IContext transform(IObservationContext sourceCtx, ISession session,
			IContext context) throws ThinklabException {
		
		/*
		 * TODO find out rows and cols from context and ensure all extents are
		 * OK.
		 */
		IExtent extent = context.getExtent(Geospace.get().SpaceObservable());
		
		if ( extent == null || !(extent instanceof GridExtent) || 
				!(extent.getValueCount() == ((IObservationContext)context).getMultiplicity()))
			throw new ThinklabValidationException("span model run in a non-spatial context or with non-spatial extents");

		int rows = ((GridExtent)extent).getYCells();
		int cols = ((GridExtent)extent).getXCells();
		
		/*
		 * run the proxy, obtain a map of closures 
		 */
		Map<?,?> closures = 
			span.runSPAN(
				sourceCtx, 
				sourceConcept, 
				useConcept,
				sinkConcept, 
				flowDataConcept, 
				flowParams);
		
		/*
		 * create states from closures where the concept space of the observable
		 * contains an observable for the result type.
		 */
		ArrayList<Pair<IConcept, IState>> states = 
			new ArrayList<Pair<IConcept,IState>>();
		
		for (Object k : closures.keySet()) {
			
			IConcept observable = defineObservable(k.toString().substring(1));
			
			if (observable != null) {

				System.out.println(
						k.toString().substring(1) + 
						" being stored as an observation of " + 
						observable);

				Object closure = closures.get(k);
				IFn clojure = (IFn) closure;
				IState state = 
					new SPANDistributionState(observable, rows, cols, clojure, 
							(ObservationContext) context);
				state.getMetadata().put(Metadata.DEFINING_MODEL, this);
				
				states.add(new Pair<IConcept, IState>(observable, state));
				
			} else {
				System.out.println("No observable defined for " + k + ": skipping");
			}
		}
		
		ObservationContext ret = new ObservationContext(context.getExtents());
		ret.setObservation(this);
		for (Pair<IConcept, IState> st : states) {
			ret.addState(st.getSecond());
		}
		
		return ret;
		
	}
}