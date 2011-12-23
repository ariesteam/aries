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

package org.integratedmodelling.aries.webapp;

import java.util.HashMap;

import org.geotools.referencing.CRS;
import org.integratedmodelling.aries.webapp.view.storyline.StorylineView;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.interfaces.IGazetteer;
import org.integratedmodelling.geospace.literals.ShapeValue;
import org.integratedmodelling.modelling.visualization.VisualizationFactory;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabResourceNotFoundException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.datastructures.IntelligentMap;
import org.integratedmodelling.thinklab.interfaces.query.IQueryResult;
import org.integratedmodelling.thinklab.literals.BooleanValue;
import org.integratedmodelling.thinklab.plugin.ThinklabPlugin;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;

public class ARIESWebappPlugin extends ThinklabPlugin {

	public static final String PLUGIN_ID = 
		"org.integratedmodelling.aries.http.explorer";
	
	public static final String WMS_SERVER_PROPERTY = 
		"aries.wms.server";
	public static final String GOOGLE_MAPS_KEY =
		"google.maps.key";
	public static final String TESTING_PROPERTY = 
		"aries.testing";
	
	private boolean testingMode = false;
	
	public boolean testingMode() {
		return this.testingMode ;
	}
	
	private IntelligentMap<Class<? extends StorylineView>> storylineViewClass = 
		new IntelligentMap<Class<? extends StorylineView>>();
	
	private static final String COVERAGE_GAZETTEER_PREFIX = "aries-coverage-";
	private HashMap<String, ShapeValue> moduleCoverage = new HashMap<String, ShapeValue>();
	
	public MathTransform geoToGoogleTransform = null;
	
	public static ARIESWebappPlugin get() {
		return (ARIESWebappPlugin) getPlugin(PLUGIN_ID);
	}
	
	public static String getGoogleKey() {
		return get().getProperties().getProperty(GOOGLE_MAPS_KEY);
	}
	
	@Override
	protected void load(KnowledgeManager km) throws ThinklabException {
		// TODO Auto-generated method stub
		try {
			geoToGoogleTransform = 
				CRS.findMathTransform(Geospace.get().getStraightGeoCRS(), Geospace.get().getGoogleCRS());
			
		} catch (FactoryException e) {
			throw new ThinklabResourceNotFoundException(e);
		}
		
		VisualizationFactory.get().loadColormapDefinitions(getProperties());
		
		testingMode = 
			BooleanValue.parseBoolean(getProperties().getProperty(TESTING_PROPERTY, "false"));
		
	}

	public ShapeValue getModuleCoverage(String module) throws ThinklabException {

		if (moduleCoverage.containsKey(module))
			return moduleCoverage.get(module);

		synchronized (moduleCoverage) {
			
			ShapeValue ret = null;
			String covid = COVERAGE_GAZETTEER_PREFIX + module;
			IQueryResult c = Geospace.get().lookupFeature(covid);
			if (c != null && c.getTotalResultCount() > 0) {
				ret = (ShapeValue) c.getResultField(0, IGazetteer.SHAPE_FIELD);
			}
			/* put in the null as well */
			moduleCoverage.put(module,ret);
			return ret;
		}
		
	}
	
	public void registerViewClass(IConcept concept, Class<? extends StorylineView> view) {
		storylineViewClass.put(concept, view);
	}
	
	public Class<? extends StorylineView> getViewClass(IConcept concept) {
		return storylineViewClass.get(concept);
	}

	@Override
	protected void unload() throws ThinklabException {
		// TODO Auto-generated method stub

	}

	public static int getOnlineUsersCount() {
		// TODO Auto-generated method stub
		return 1;
	}



}
