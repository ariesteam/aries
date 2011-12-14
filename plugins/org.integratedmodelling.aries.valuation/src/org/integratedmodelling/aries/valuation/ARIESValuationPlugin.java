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

package org.integratedmodelling.aries.valuation;

import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;
import org.integratedmodelling.thinklab.transformations.TransformationFactory;

public class ARIESValuationPlugin extends ThinklabPlugin {

	public static final String ID = "org.integratedmodelling.aries.valuation";
	static IConcept esvconcept = null;
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		
		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		TransformationFactory.get().loadTransformationMappings(getProperties());
		
		esvconcept = km.requireConcept("esvaluation:HistoricESValue");
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

	public static ARIESValuationPlugin get() {
		return (ARIESValuationPlugin)getPlugin(ID);
	}
	
	public static IConcept ESVConcept() {
		return esvconcept;
	}
	
}
