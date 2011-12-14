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
