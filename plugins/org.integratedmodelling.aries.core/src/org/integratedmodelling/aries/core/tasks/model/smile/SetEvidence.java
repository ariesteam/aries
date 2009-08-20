package org.integratedmodelling.aries.core.tasks.model.smile;

import java.util.Map;

import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

import smile.Network;

@TaskNamespace(ns = "aries")
public class SetEvidence implements ITask {

	private Network inference = null;
	private Map<Object,Object> stateMap = null;

	public void setInference(Network inference) {
		this.inference  = inference;
	}
	
	public void setStateMap(Map<Object,Object> stateMap) {
		this.stateMap  = stateMap;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		
		for (Object state : stateMap.keySet()) {
			inference.setEvidence(state.toString(), stateMap.get(state).toString());
		}	
	}
	
	public Network getInference() {
		return inference;
	}

}
