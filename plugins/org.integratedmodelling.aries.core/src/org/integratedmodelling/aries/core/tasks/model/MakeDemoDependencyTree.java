package org.integratedmodelling.aries.core.tasks.model;

import org.integratedmodelling.aries.core.datastructures.demo.DemoDependencyTree;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.graph.KnowledgeGraph;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.applications.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;


/**
 * Given a concept of interest, generate a dependency tree with that concept at its root. Any
 * model of that concept will be composed of observations of those concepts. 
 * 
 * Use pre-defined dependency trees stored in the demo knowledge base. If trees have not been stored
 * for the passed concept, run() will raise an exception.
 * 
 * @author Ferdinando
 *
 */
@TaskNamespace(ns = "aries")
public class MakeDemoDependencyTree implements ITask {

	KnowledgeGraph tree = null;
	IConcept root = null;
	
	public void setTargetConcept(IConcept c) {	
		this.root = c;
	}
	
	public KnowledgeGraph getDependencyTree() {
		return tree;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		tree = new DemoDependencyTree(root);
	}

}