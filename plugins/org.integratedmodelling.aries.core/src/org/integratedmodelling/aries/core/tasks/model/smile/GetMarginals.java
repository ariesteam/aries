package org.integratedmodelling.aries.core.tasks.model.smile;

import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

import smile.Network;

@TaskNamespace(ns = "aries")
public class GetMarginals implements ITask {
	
	private Double result = null;
	private Network inference;
	private Object nodeId;
	private String stateId;

	public void setInference(Network inference) {
		this.inference = inference;
	}
	
	public void setNode(Object nodeId) {
		this.nodeId = nodeId;
	}
	
	public void setState(String stateId) {
		this.stateId = stateId;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {

		double[] vals = inference.getNodeValue(nodeId.toString());
		int i = 0;
		for (String s : inference.getOutcomeIds(nodeId.toString())) {
			if (s.equals(stateId)) {
				result = vals[i++];
				break;
			}
		}
	}
	
	public Double getResult() {
		return result;
	}

}
