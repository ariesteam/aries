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

package org.integratedmodelling.aries.marine;

import org.integratedmodelling.aries.marine.spank.SubsistenceFisheries;
import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.modelling.agents.SPANK;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;

public class ARIESMarinePlugin extends ThinklabPlugin {

	// concepts of top-level models of interest
	public final static String subsistenceModel = "fisheries:SubsistenceFishProvision";
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {

		Metadata.loadPredefinedOrderings(getProperties());
		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		SPANK.registerSpankClass(
				km.requireConcept(subsistenceModel), SubsistenceFisheries.class);
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

}
