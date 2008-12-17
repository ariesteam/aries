package org.integratedmodelling.aries.core.tasks.model;

import org.integratedmodelling.aries.core.datastructures.DependencyTree;
import org.integratedmodelling.thinklab.interfaces.IConcept;
import org.integratedmodelling.thinklab.interfaces.ITask;


/**
 * Given a concept of interest, generate a dependency tree with that concept at its root. Any
 * model of that concept will be composed of observations of those concepts. 
 * 
 * @author Ferdinando
 *
 */
public class GenerateDependencyTree implements ITask {

	public void setTargetConcept(IConcept c) {	
	}
	
	public DependencyTree getDependencyTree() {
		return null;
	}
	
	@Override
	public void run() {
		// TODO Auto-generated method stub

	}

}
