package org.integratedmodelling.aries.core.tasks.model.bayes;

import java.util.HashMap;

import org.integratedmodelling.riskwiz.jtree.JTInference;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

@TaskNamespace(ns = "aries")
public class GetMarginalsTable implements ITask {
	
	private HashMap<String, Double> result = null;
	private JTInference<?> inference;
	private Object nodeId;

	public void setInference(JTInference<?> inference) {
		this.inference = inference;
	}
	
	public void setNode(Object nodeId) {
		this.nodeId = nodeId;
	}
		
	@Override
	public void run(ISession session) throws ThinklabException {
		result = inference.getMarginal(nodeId.toString()).getDomainValuePairs();
	}
	
	public HashMap<String, Double> getResult() {
		return result;
	}

}
