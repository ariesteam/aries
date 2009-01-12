package org.integratedmodelling.aries.core.tasks.query;

import java.util.HashMap;
import java.util.Map;

import org.integratedmodelling.databridge.ObservationFactory;
import org.integratedmodelling.geospace.values.ShapeValue;
import org.integratedmodelling.thinklab.constraint.Constraint;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.graph.KnowledgeGraph;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.applications.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.thinklab.interfaces.query.IQueryResult;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;

/**
 * Return all available data to characterize a model of the root observable according to
 * the passed dependency tree. Includes a scoring algorithm to use if the kbox contains more
 * than one observation for this observable in the passed context. 
 * 
 * If "exhaustive" has been set to true, the observation set
 * will also contain available observations for observables that already have data available,
 * to be used for model training.
 * 
 * TODO implement scoring algorithm
 * TODO implement smarter handling of partial overlap between high-score result and lower-score
 * results by iteratively burning all available pixels of better result over next best result.
 * TODO both algorithms should have configurable parameters
 * 
 * @author Ferdinando Villa
 *
 */
@TaskNamespace(ns = "aries")
public class RetrieveObservations implements ITask {

	KnowledgeGraph dTree = null;
	boolean exhaustive = false;
	HashMap<IConcept, IInstance> results = new HashMap<IConcept, IInstance>();
	private ShapeValue where;
	private IKBox kbox;
	
	public void setKbox(IKBox kbox) {
		this.kbox = kbox;
	}

	public void setDependencyTree(KnowledgeGraph tree) {
		this.dTree = tree;
	}
	
	public void setRegionOfInterest(ShapeValue where) {
		this.where = where;
	}
	
	public void setExhaustive(boolean b) {
		this.exhaustive = b;
	}
	
	/**

	 * 
	 * @return
	 */
	public Map<IConcept, IInstance> getObservations() {
		return results;
	}
	
	private void collectObservationFor(IConcept observable, ISession session) throws ThinklabException {
		
		/* you never know */
		if (results.containsKey(observable))
			return;
		
		Constraint query = 
			ObservationFactory.queryObservation(kbox, observable.toString(), where);

		IQueryResult result = this.kbox.query(query);
		
		IInstance ret = selectBestCandidate(result, session);

		if (ret != null) 
			results.put(observable, ret);
	
		if (exhaustive || ret == null) {
			for (Object edge : dTree.outgoingEdgesOf(observable)) {
				collectObservationFor((IConcept) dTree.getEdgeTarget(edge), session);
			}
		}
	}
	
	/*
	 * pick the most current, larger coverage, higher quality, or lower cost according to 
	 * priorities when we have more than one dataset per observable. 
	 */
	private IInstance selectBestCandidate(IQueryResult result, ISession session) throws ThinklabException {
		
		IInstance ret = null;
		
		if (result == null || result.getResultCount() == 0)
			return null;
		
		if (result.getResultCount() > 1) {
			
			/* 
			 * TODO compute score for each observations and assign it to result using
			 * result.setResultScore()
			 */
			
		}
		
		ret = result.getBestResult(session).asObjectReference().getObject();
		
		return ret;
	}
	
	@Override
	public void run(ISession session)  throws ThinklabException {
		
		collectObservationFor((IConcept) dTree.getRoot(), session);

	}

}
