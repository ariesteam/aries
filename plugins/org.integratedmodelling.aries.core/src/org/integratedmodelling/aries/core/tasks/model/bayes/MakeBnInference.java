/*
Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)

This file is part of ARIES.

ARIES is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License,
or (at your option) any later version.

ARIES is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.integratedmodelling.aries.core.tasks.model.bayes;

import java.io.File;
import java.util.HashMap;

import org.integratedmodelling.aries.core.ARIESCorePlugin;
import org.integratedmodelling.riskwiz.bn.BeliefNetwork;
import org.integratedmodelling.riskwiz.inference.ls.JoinTreeCompiler;
import org.integratedmodelling.riskwiz.io.genie.GenieReader;
import org.integratedmodelling.riskwiz.io.riskwiz.RiskWizReader;
import org.integratedmodelling.riskwiz.jtree.JTInference;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

@TaskNamespace(ns = "aries.riskwiz")
public class MakeBnInference implements ITask {

	private IConcept benefit = null;
	private File dataDir = null;
	private JTInference<?> result = null;

	private static HashMap<IConcept, BeliefNetwork> networks = new HashMap<IConcept, BeliefNetwork>();
	
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
	
	private BeliefNetwork requireBeliefNetwork(IConcept concept) {
		
		BeliefNetwork ret = networks.get(concept);
		
		if (ret == null) {

			String fname = benefit.toString().replace(':', '_');
			try {
				if (new File(dataDir + "/" + fname + ".xdsl").exists()) {
					GenieReader gReader = new GenieReader();
					ret = gReader.loadFromFile(dataDir + "/" + fname + ".xdsl");
				} else if (new File(dataDir + "/" + fname + ".rwz").exists()) {
					RiskWizReader gReader = new RiskWizReader();
					ret = gReader.loadFromFile(dataDir + "/" + fname + ".rwz");
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
			
			networks.put(concept, ret);
		}
		
		return ret;
	}
	
	@Override
	public void run(ISession session) throws ThinklabException {

		BeliefNetwork nw = requireBeliefNetwork(benefit);
		result = new JTInference();
		try {
			result.initialize(nw, new JoinTreeCompiler());
		} catch (Exception e) {
			throw new ThinklabRuntimeException(e);
		}
	}
	
	public JTInference<?> getInference() {
		return result;
	}
}
