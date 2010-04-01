package org.integratedmodelling.aries.core.models;

import java.util.ArrayList;

import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.ObservationFactory;
import org.integratedmodelling.modelling.DefaultAbstractModel;
import org.integratedmodelling.modelling.interfaces.IModel;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.utils.Polylist;

import clojure.lang.Keyword;
import clojure.lang.PersistentArrayMap;

public class SPANModel extends DefaultAbstractModel {

	public final static String DOWNSCALE_PROPERTY_PREFIX = "aries.model.downsample.";

	/**
	 * these are reclassified in the context of the main observable, so that the
	 * corresponding parameters can be communicated to users appropriately.
	 */
	private static final String SOURCE_THRESHOLD_CONCEPT = "eserv:SourceThreshold";
	private static final String SINK_THRESHOLD_CONCEPT = "eserv:SinkThreshold";
	private static final String USE_THRESHOLD_CONCEPT = "eserv:UseThreshold";
	private static final String TRANSITION_THRESHOLD_CONCEPT = "eserv:TransitionThreshold";
	
	IConcept sourceObservableId = null;
	IConcept sinkObservableId = null;
	IConcept useObservableId = null;
	IConcept flowObservableId = null;
	IConcept flowDataObservableId = null;
	
	IConcept sourceObservable = null;
	IConcept sinkObservable = null;
	IConcept useObservable = null;
	IConcept flowObservable = null;
	IConcept flowDataObservable = null;
	private PersistentArrayMap flowParams = new PersistentArrayMap(new Object[]{});
	
	/*
	 * this is the actual way to pass parameters
	 */
	ArrayList<Polylist> parameters = new ArrayList<Polylist>();
	
	static Keyword downscalingFactor = Keyword.intern(null, "downscaling-factor");

	@Override
	protected void validateMediatedModel(IModel model)
			throws ThinklabValidationException {
	}

	@Override
	public IConcept getCompatibleObservationType(ISession session) {
		// TODO Auto-generated method stub
		return null;
	}

	public void setFlowObservables(IConcept source, IConcept use, IConcept sink, IConcept flow, IConcept flowData) {
		this.sourceObservable = source;
		this.useObservable = use;
		this.sinkObservable = sink;
		this.flowObservable = flow;
		this.flowDataObservable = flowData;
	}
	
	@Override
	public IModel getConfigurableClone() {
		
		SPANModel ret = new SPANModel();
		ret.copy(this);
		ret.flowParams = flowParams;
		ret.sourceObservable = sourceObservable;
		ret.useObservable = useObservable;
		ret.sinkObservable = sinkObservable;
		ret.flowObservable = flowObservable;
		ret.flowDataObservable = flowDataObservable;
		
		return ret; 
	}
	
	/**
	 * Return the specialized type of the general passed parameter, or null if it 
	 * does not apply to this model.
	 * 
	 * @param concept
	 * @return
	 */
	public String  getParameterType(String concept) {
		
		/**
		 * TODO implement
		 */
		
		return concept;
	}

	@Override
	public void applyClause(String keyword, Object argument)
			throws ThinklabException {
		
		if (keyword.equals(":source-threshold")) {
			String c = getParameterType(SOURCE_THRESHOLD_CONCEPT);
			if (c != null)
				parameters.add(
					ObservationFactory.createRanking(c, ((Number)argument).doubleValue()));
		}
	
		if (keyword.equals(":sink-threshold")) {
			String c = getParameterType(SINK_THRESHOLD_CONCEPT);
			if (c != null)
				parameters.add(
					ObservationFactory.createRanking(SINK_THRESHOLD_CONCEPT, ((Number)argument).doubleValue()));			
		}
		
		if (keyword.equals(":use-threshold")) {
			String c = getParameterType(USE_THRESHOLD_CONCEPT);
			if (c != null)
				parameters.add(
					ObservationFactory.createRanking(USE_THRESHOLD_CONCEPT, ((Number)argument).doubleValue()));			
		}
		
		if (keyword.equals(":trans-threshold")) {
			String c = getParameterType(TRANSITION_THRESHOLD_CONCEPT);
			if (c != null)
				parameters.add(
					ObservationFactory.createRanking(TRANSITION_THRESHOLD_CONCEPT, ((Number)argument).doubleValue()));			
		}
				
		// TODO remove the four thresholds when the above work properly.
		if (keyword.equals(":source-threshold") ||
			keyword.equals(":sink-threshold") ||
			keyword.equals(":use-threshold") ||
			keyword.equals(":trans-threshold") ||
			keyword.equals(":sink-type") ||
			keyword.equals(":use-type") ||
			keyword.equals(":benefit-type") ||
			keyword.equals(":downscaling-factor") ||
			keyword.equals(":rv-max-states")) {
			
			// these must be sent back to Clojure, and a normal Map won't do - what a pain
			flowParams = 
				(PersistentArrayMap) 
					flowParams.assoc(Keyword.intern(null, keyword.substring(1)), argument);
			
		} else {
			super.applyClause(keyword, argument);
		}
	}

	@Override
	public Polylist buildDefinition(IKBox kbox, ISession session) throws ThinklabException {

		ArrayList<Object> arr = new ArrayList<Object>();
		
		// TODO if someone has overridden the downsampling factor for this model, put it in
		// fucking FIXME - the stupid thing still has null name even if we set it everywhere.
//		String pmod = DOWNSCALE_PROPERTY_PREFIX + this.name.replaceAll("\\/", "-");
//		
//		String ns = ARIESCorePlugin.get().getProperties().getProperty(pmod);
//		if (ns != null) {
//			Integer nds = Integer.parseInt(ns);
//			flowParams.put(downscalingFactor, nds);
//		}
			
		arr.add("aries:SPANTransformer");
		arr.add(Polylist.list(
				CoreScience.HAS_OBSERVABLE,
				Polylist.list(getObservable())));
		
		// set flow parameters directly into instance implementation (SPANTransformer) 
		// using reflection. Non-serializable, but who cares.
		// TODO/FIXME - these should be dependent obs eventually
		arr.add(Polylist.list(":flowParams", flowParams));
		arr.add(Polylist.list(":sourceConcept", sourceObservable));
		arr.add(Polylist.list(":useConcept",    useObservable));
		arr.add(Polylist.list(":sinkConcept",   sinkObservable));
		arr.add(Polylist.list(":flowConcept",   flowObservable));
		arr.add(Polylist.list(":flowDataConcept",   flowDataObservable));

		return Polylist.PolylistFromArrayList(arr);
	}

	@Override
	public Polylist conceptualize() throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void validateSemantics(ISession session) throws ThinklabException {
		
		// TODO take the concept IDs and annotate them as usual. NOTE: the 
		// concepts themselves should actually be observables of dependents, so
		// the whole thing needs attention.
		
	}

}
