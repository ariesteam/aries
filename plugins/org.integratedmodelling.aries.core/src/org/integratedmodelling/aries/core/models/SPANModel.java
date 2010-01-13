package org.integratedmodelling.aries.core.models;

import java.util.ArrayList;
import java.util.Map;

import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.modelling.DefaultStatefulAbstractModel;
import org.integratedmodelling.modelling.interfaces.IModel;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.utils.Polylist;

public class SPANModel extends DefaultStatefulAbstractModel {

	IConcept sourceObservable = null;
	IConcept sinkObservable = null;
	IConcept useObservable = null;
	IConcept flowObservable = null;
	private Map<?, ?> flowParams;
	
	@Override
	protected void validateMediatedModel(IModel model)
			throws ThinklabValidationException {
	}

	@Override
	protected Object validateState(Object state)
			throws ThinklabValidationException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IConcept getCompatibleObservationType(ISession session) {
		// TODO Auto-generated method stub
		return null;
	}

	public void setFlowObservables(IConcept source, IConcept use, IConcept sink, IConcept flow) {
		this.sourceObservable = source;
		this.useObservable = use;
		this.sinkObservable = sink;
		this.flowObservable = flow;
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
		
		return ret; 
	}

	/*
	 * these come from the specs, must go straight to the SPAN proxy.
	 */
	public void setFlowParams(Map<?,?> flowParams) {
		this.flowParams = flowParams;
	}
	
	@Override
	public Polylist buildDefinition(IKBox kbox, ISession session) throws ThinklabException {

		ArrayList<Object> arr = new ArrayList<Object>();
		
		arr.add("aries:SPANTransformer");
		arr.add(Polylist.list(
				CoreScience.HAS_OBSERVABLE,
				Polylist.list(getObservable())));
		
		// set flow parameters directly into instance implementation (SPANTransformer) 
		// using reflection. Non-serializable, but who cares.
		arr.add(Polylist.list(":flowParams", flowParams));
		arr.add(Polylist.list(":sourceConcept", sourceObservable));
		arr.add(Polylist.list(":useConcept",    useObservable));
		arr.add(Polylist.list(":sinkConcept",   sinkObservable));
		arr.add(Polylist.list(":flowConcept",   flowObservable));

		return Polylist.PolylistFromArrayList(arr);
	}

	@Override
	public Polylist conceptualize() throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

}
