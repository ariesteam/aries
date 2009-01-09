package org.integratedmodelling.aries.core.tasks.query;

import java.util.HashMap;
import java.util.Map;

import org.integratedmodelling.aries.core.datastructures.DependencyTree;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.applications.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;

@TaskNamespace(ns = "aries")
public class RetrieveObservations implements ITask {

	DependencyTree dTree = null;
	boolean exhaustive = false;
	HashMap<IConcept, IInstance> results = new HashMap<IConcept, IInstance>();
	
	public void setDependencyTree(DependencyTree tree) {
		this.dTree = tree;
	}
	
	public void setExhaustive(boolean b) {
		this.exhaustive = b;
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
