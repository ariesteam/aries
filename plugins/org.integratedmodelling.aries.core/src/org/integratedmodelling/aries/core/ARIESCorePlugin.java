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

package org.integratedmodelling.aries.core;

import org.integratedmodelling.corescience.metadata.Metadata;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.gazetteers.SimpleGazetteer;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.thinklab.kbox.KBoxManager;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;
import org.integratedmodelling.thinklab.transformations.TransformationFactory;

public class ARIESCorePlugin extends ThinklabPlugin {
	
	static public final String ARIES_KBOX_PROPERTY = "aries.kbox";
	
	IKBox workingKbox = null;
	
	public static final String PLUGIN_ID = 
		"org.integratedmodelling.aries.core";
	
	public static ARIESCorePlugin get() {
		return (ARIESCorePlugin) getPlugin(PLUGIN_ID);
	}
	
	@Override
	protected void preStart() throws Exception {
		// load locations in advance of loading bindings, so that contexts can
		// use them.
		Geospace.get().addGazetteer("aries-inline", new SimpleGazetteer(getProperties()));
	}

	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {

		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		Metadata.loadPredefinedOrderings(getProperties());
		TransformationFactory.get().loadTransformationMappings(getProperties());
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub
	}

	public IKBox getWorkingKBox() throws ThinklabException {

		if (workingKbox == null) {
			
			String kboxname = getProperties().getProperty(ARIES_KBOX_PROPERTY);
			if (kboxname != null)
				workingKbox = KBoxManager.get().requireGlobalKBox(kboxname);
			if (workingKbox /* still */ == null) {
				workingKbox = KBoxManager.get();
			}
		}
		return workingKbox;
	}
	
}
