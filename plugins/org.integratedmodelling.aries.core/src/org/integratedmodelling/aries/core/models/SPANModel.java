package org.integratedmodelling.aries.core.models;

import java.util.ArrayList;

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
	
	@Override
	public void applyClause(String keyword, Object argument)
			throws ThinklabException {
		
		if (keyword.equals(":source-model")) {
			addDependentModel((IModel) argument);
			sourceObservable = ((IModel)argument).getObservable();
		} else if (keyword.equals(":sink-model")) {
			addDependentModel((IModel) argument);
			sinkObservable = ((IModel)argument).getObservable();
		} else if (keyword.equals(":use-model")) {
			addDependentModel((IModel) argument);
			useObservable = ((IModel)argument).getObservable();
		} else if (keyword.equals(":flow-model")) {
			addDependentModel((IModel) argument);
			flowObservable = ((IModel)argument).getObservable();
		} else if (keyword.equals(":use-threshold")) {
		} else if (keyword.equals(":sink-threshold")) {
		} else if (keyword.equals(":source-threshold")) {
		} else if (keyword.equals(":decay-rate")) {
		} else super.applyClause(keyword, argument);
	}

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

	@Override
	public IModel getConfigurableClone() {
		
		SPANModel ret = new SPANModel();
		ret.copy(this);

		return ret; 
	}

	@Override
	public Polylist buildDefinition(IKBox kbox, ISession session) throws ThinklabException {

		ArrayList<Object> arr = new ArrayList<Object>();
		
		arr.add("aries:SPANModel");

		return Polylist.PolylistFromArrayList(arr);
	}

	@Override
	public Polylist conceptualize() throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

}
