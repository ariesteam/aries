package org.integratedmodelling.aries.core.tasks.model.smile;

import java.util.HashMap;

import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

import smile.Network;

@TaskNamespace(ns = "aries")
public class GetMarginalsTable implements ITask {
	
	private HashMap<String, Double> result = null;
	private Network inference;
	private Object nodeId;

	public void setInference(Network inference) {
		this.inference = inference;
	}
	
	public void setNode(Object nodeId) {
		this.nodeId = nodeId;
	}
		
	@Override
	public void run(ISession session) throws ThinklabException {
		result = new HashMap<String, Double>(inference.getOutcomeCount(nodeId.toString()));
		double[] vals = inference.getNodeValue(nodeId.toString());
		int i = 0;
		for (String s : inference.getOutcomeIds(nodeId.toString())) {
			result.put(s, vals[i++]);
		}
	}
	
	public HashMap<String, Double> getResult() {
		return result;
	}

}
