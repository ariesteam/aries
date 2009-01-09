package org.integratedmodelling.aries.core.tasks.model;

import org.integratedmodelling.aries.core.datastructures.DependencyTree;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.applications.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;


/**
 * Given a concept of interest, generate a dependency tree with that concept at its root. Any
 * model of that concept will be composed of observations of those concepts. 
 * 
 * @author Ferdinando
 *
 */
@TaskNamespace(ns = "aries")
public class GenerateDependencyTree implements ITask {

	public void setTargetConcept(IConcept c) {	
	}
	
	public DependencyTree getDependencyTree() {
		return null;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		// TODO Auto-generated method stub

	}

}
