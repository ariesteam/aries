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

import java.util.Map;

import org.integratedmodelling.riskwiz.jtree.JTInference;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;

@TaskNamespace(ns = "aries.riskwiz")
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
