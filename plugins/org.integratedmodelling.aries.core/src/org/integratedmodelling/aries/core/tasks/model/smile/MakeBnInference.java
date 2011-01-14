package org.integratedmodelling.aries.core.tasks.model.smile;

import java.io.File;

import org.integratedmodelling.aries.core.ARIESCorePlugin;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

import smile.Network;

@TaskNamespace(ns = "aries")
public class MakeBnInference implements ITask {

	private IConcept benefit = null;
	private File dataDir = null;
	private Network result = null;

	public MakeBnInference() {
		
		/*
		 * find demo model dir; if nowhere, complain
		 */
		dataDir  = 
			new File(ARIESCorePlugin.get().getLoadDirectory() + "/demo/bn");
		
		if (!dataDir.exists() || !dataDir.isDirectory() || !dataDir.canRead())
			throw new ThinklabRuntimeException(
					"aries: demo data directory " +
					dataDir +
					" is not readable");
	}
	
	public void setBenefit(IConcept benefit) {
		this.benefit = benefit;
	}
	
	private Network requireBeliefNetwork(IConcept concept) {
		
		Network ret = new Network();
		
		String fname = benefit.toString().replace(':', '_');
		try {
			if (new File(dataDir + "/" + fname + ".xdsl").exists()) {
				ret.readFile(dataDir + "/" + fname + ".xdsl");
			}
		} catch (Exception e) {
			throw new ThinklabRuntimeException(e);
		}
			
		if (ret == null)
			throw new ThinklabRuntimeException(
					"aries: cannot find a network model for " + 
					concept +
					" in " +
					dataDir);
			
		return ret;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {
		result = requireBeliefNetwork(benefit);
	}
	
	public Network getInference() {
		return result;
	}
}
