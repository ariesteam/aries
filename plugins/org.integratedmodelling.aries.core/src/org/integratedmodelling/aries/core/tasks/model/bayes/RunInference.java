package org.integratedmodelling.aries.core.tasks.model.bayes;

import org.integratedmodelling.riskwiz.jtree.JTInference;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

@TaskNamespace(ns = "aries")
public class RunInference implements ITask {

	private JTInference<?> inference = null;

	public void setInference(JTInference<?> inference) {
		this.inference  = inference;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		inference.run();
	}
	
	public JTInference<?> getInference() {
		return inference;
	}

}
