package org.integratedmodelling.aries.core.tasks.model.smile;

import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

import smile.Network;

@TaskNamespace(ns = "aries")
public class RunInference implements ITask {

	private Network inference = null;

	public void setInference(Network inference) {
		this.inference  = inference;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		inference.updateBeliefs();
	}
	
	public Network getInference() {
		return inference;
	}

}
