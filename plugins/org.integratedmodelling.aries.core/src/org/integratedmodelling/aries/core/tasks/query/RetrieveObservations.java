package org.integratedmodelling.aries.core.tasks.query;

import java.util.Map;

import org.integratedmodelling.aries.core.datastructures.DependencyTree;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.IConcept;
import org.integratedmodelling.thinklab.interfaces.IInstance;
import org.integratedmodelling.thinklab.interfaces.ISession;
import org.integratedmodelling.thinklab.interfaces.ITask;

public class RetrieveObservations implements ITask {

	public void setDependencyTree(DependencyTree tree) {
		
	}
	
	public void setExhaustive(boolean b) {
		
	}
	
	/**
	 * Return all available data to characterize a model of the root observable according to
	 * the passed dependency tree. If "exhaustive" has been set to true, the observation set
	 * will also contain available observations for observables that already have data available,
	 * to be used for model training.
	 * 
	 * @return
	 */
	public Map<IConcept, IInstance> getObservations() {
		return null;
	}
	
	@Override
	public void run(ISession session)  throws ThinklabException {
		// TODO Auto-generated method stub

	}

}
