package org.integratedmodelling.aries.core.tasks.model.bayes;

import org.integratedmodelling.riskwiz.jtree.JTInference;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

@TaskNamespace(ns = "aries")
public class GetMarginals implements ITask {
	
	private Double result = null;
	private JTInference<?> inference;
	private Object nodeId;
	private String stateId;

	public void setInference(JTInference<?> inference) {
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
		result = inference.getMarginal(nodeId.toString()).getDomainValuePairs().get(stateId);
	}
	
	public Double getResult() {
		return result;
	}

}
