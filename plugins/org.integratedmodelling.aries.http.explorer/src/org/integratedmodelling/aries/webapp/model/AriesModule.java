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

package org.integratedmodelling.aries.webapp.model;

import java.util.ArrayList;
import java.util.Collection;

import org.integratedmodelling.aries.core.ARIESCorePlugin;
import org.integratedmodelling.aries.webapp.ARIESWebappPlugin;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.geospace.literals.ShapeValue;
import org.integratedmodelling.modelling.interfaces.IModel;
import org.integratedmodelling.modelling.model.ModelFactory;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

/**
 * The model class for an ARIES module, giving access to a series of tasks and having its representation
 * in an AriesModuleView.
 * 
 * Each module may have a model associated, which can be set in the properties. To do so, the 
 * properties of the ARIES plugin should contain the string "aries.model.pluginName=model/name". When 
 * that is present, the model will be computed automatically when the user switches the module on. 
 * 
 * @author Ferdinando
 *
 */
public class AriesModule {

	public static final String ARIES_MODEL_PREFIX = "aries.model.";
	
	public static final int STATUS_IDLE = 0;
	public static final int STATUS_BUSY = 1;
	public static final int STATUS_FAIL = 2;
	public static final int STATUS_DONE = 3;
	public static final int STATUS_WARN = 4;
	public static final int STATUS_READY = 5;
	public static final int STATUS_DISABLED = 6;
	public static final int STATUS_UNAUTHORIZED = 7;

	/*
	 * we distinguish the storylines that are not computable in this context for information.
	 */
	ArrayList<Storyline> inactiveStorylines = new ArrayList<Storyline>();
	ArrayList<Storyline> activeStorylines = new ArrayList<Storyline>();

	/**
	 * We compute models serially in a module but concurrently across modules, 
	 * so each module needs its own session.
	 */
	protected ISession session = null;

	/*
	 * this storyline has the ones for each model as children
	 */
	private Storyline storyline;
	
	public AriesModule(Storyline s) {
		this.storyline = s;
	}
	
	public Storyline getStoryline() {
		return storyline;
	}

	/**
	 * All modules must expose a unique ID that can be used in a property - 
	 * a simple identifier.
	 * 
	 * @return
	 */
	public String getModuleId() {
		return storyline.getTemplate().getId();
	}
	
	public ShapeValue getCoverage() throws ThinklabException {
		return storyline.getCoverage();
	}
	
	/*
	 * Notify a context; rebuild the initial set of storylines and their coverage;
	 * return the module's storyline with all its children.
	 */
	public Storyline notifyContext(IContext context) throws ThinklabException {
		
		/*
		 * choose all models and check their coverage; all that match, build a storyline 
		 * for them so that the view can be updated.
		 */
		for (Storyline sl : storyline.getChildren()) {
			if (sl.isCovered() && sl.isAuthorized(session))
				activeStorylines.add(sl);
			else
				inactiveStorylines.add(sl);
		}
		
		return this.storyline;
	}

	protected boolean checkEnabled(ShapeValue region) throws ThinklabException {

		boolean ret = false;
		ShapeValue coverage = ARIESWebappPlugin.get().getModuleCoverage(getModuleId());
		
		if (coverage != null) {
			ret = coverage.contains(region);
		}
		
		return ret;
	}
	
	public void clearAnalysis() {
		session = null;
		storyline = null;
		inactiveStorylines.clear();
		activeStorylines.clear();
	}

	/**
	 * Return a new collection of all models configured for this module.
	 */
	public Collection<IModel> getModels() throws ThinklabException {
		
		ArrayList<IModel> ret = new ArrayList<IModel>();
		
		String modelId = ARIESCorePlugin.get().getProperties().getProperty(
				ARIES_MODEL_PREFIX + getModuleId());
		
		if (modelId != null) {
			
			String[] models = modelId.split(",");
			for (String mid : models) {
				ret.add(ModelFactory.get().requireModel(mid));
			}
		}
		return ret;
	}

	public Collection<Storyline> getStorylines() {
		return activeStorylines;
	}

}
