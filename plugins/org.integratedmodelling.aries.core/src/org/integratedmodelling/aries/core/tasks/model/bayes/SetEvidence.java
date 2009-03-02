package org.integratedmodelling.aries.core.tasks.model.bayes;

import java.util.Map;

import org.integratedmodelling.riskwiz.jtree.JTInference;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

@TaskNamespace(ns = "aries")
public class SetEvidence implements ITask {

	private JTInference<?> inference = null;
	private Map<Object,Object> stateMap = null;

	public void setInference(JTInference<?> inference) {
		this.inference  = inference;
	}
	
	public void setStateMap(Map<Object,Object> stateMap) {
		this.stateMap  = stateMap;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		for (Object state : stateMap.keySet()) {
			inference.setObservation(state.toString(), stateMap.get(state).toString());
		}	
	}
	
	public JTInference<?> getInference() {
		return inference;
	}

}
