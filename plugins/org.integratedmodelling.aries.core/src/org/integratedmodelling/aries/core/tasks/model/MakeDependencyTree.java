package org.integratedmodelling.aries.core.tasks.model;

import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.graph.KnowledgeGraph;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;


/**
 * Given a concept of interest, generate a dependency tree with that concept at its root. Any
 * model of that concept will be composed of observations of those concepts. 
 * 
 * TODO this needs to know more context and will eventually incorporate the probabilistic reasoning
 * engine to select the appropriate model based on context.
 * 
 * @author Ferdinando
 *
 */
@TaskNamespace(ns = "aries")
public class MakeDependencyTree implements ITask {

	public void setTargetConcept(IConcept c) {	
	}
	
	public KnowledgeGraph getDependencyTree() {
		return null;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		// TODO Auto-generated method stub

	}

}
