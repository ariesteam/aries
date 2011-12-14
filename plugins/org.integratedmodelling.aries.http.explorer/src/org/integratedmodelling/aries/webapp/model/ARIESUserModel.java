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

import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;

import org.integratedmodelling.aries.webapp.ARIESWebappPlugin;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.geospace.Geospace;
import org.integratedmodelling.geospace.literals.ShapeValue;
import org.integratedmodelling.modelling.context.Context;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.modelling.storyline.StorylineFactory;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabResourceNotFoundException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.http.ThinklabWebModel;
import org.integratedmodelling.thinklab.http.ThinklabWebSession;
import org.integratedmodelling.thinklab.http.geospace.model.IGeolocatedModel;
import org.integratedmodelling.thinklab.interfaces.literals.IValue;

public class ARIESUserModel extends ThinklabWebModel implements IGeolocatedModel {

	private static final String MAX_RESOLUTION_PROPERTY = "aries.resolution.default";
	private static final String ROOT_STORYLINE_PROPERTY = "aries.storyline";
	
	private int maxResolution = 512;
	ShapeValue roi = null;
	
	// all our modules, created by the User model according to privileges and
	// preferences.
	private ArrayList<AriesModule> modules = new ArrayList<AriesModule>();
	// synchronized access to module index for the browser
	private Hashtable<String, AriesModule> moduleIndex = new Hashtable<String, AriesModule>();
	
	private ThinklabWebSession session = null;
	
	/*
	 * The "root" storyline which contains the others, and correspond to the info panel for the 
	 * case study. It will describe the status of all modules.
	 */
	Storyline storyline = null;
	
	private IContext context;
	
	public static class Location {
		public IValue id;
		public IValue label;
		public IValue description;
		
		public String toString() {
			return label.toString();
		}
	}

	/**
	 * Return the maximum linear resolution for this user, defaulting to 128. Maximum raster
	 * size will be this squared.
	 * 
	 * @return
	 */
	public int getResolution() {
		return maxResolution;
	}
	
	@Override
	public void initialize(ThinklabWebSession session) throws ThinklabException {
			
		String slPath = 
			ARIESWebappPlugin.get().getProperties().getProperty(
				ROOT_STORYLINE_PROPERTY, "aries");
		
		this.session = session;
		this.maxResolution = 
			Integer.parseInt(
					ARIESWebappPlugin.get().getProperties().getProperty(MAX_RESOLUTION_PROPERTY, 
					"384"));
		
		this.storyline = StorylineFactory.getStorylines(slPath);
		
		if (this.storyline == null) {
			throw new ThinklabResourceNotFoundException("storyline path: " + slPath);
		}
	}

	@Override
	public void persist(String authenticatedUser) throws ThinklabException {
		// TODO Auto-generated method stub

	}

	@Override
	public void restore(String authenticatedUser) throws ThinklabException {
		// TODO check persistent storage; load settings and previous session data
	}

	@Override
	public InputStream getInputStream() {
		return null;
	}

	@Override
	public PrintStream getOutputStream() {
		return null;
	}

	@Override
	public ShapeValue getRegionOfInterest() throws ThinklabException {
		return roi;
	}

	@Override
	public void subtractRegionOfInterest(ShapeValue region) throws ThinklabException {
		
		if (roi != null)
			roi = roi.difference(region);
	}
	
	@Override
	public boolean addRegionOfInterest(ShapeValue region) throws ThinklabException {
		
		System.out.println("ROI: " + region.transform(Geospace.get().getDefaultCRS()));
		boolean ret = roi != null;
		roi = roi == null ? region : roi.union(region);
		return ret;
	}
	
	@Override
	public void resetRegionOfInterest(ShapeValue region) throws ThinklabException {

		if (roi != null) {
			System.out.println("ROI: " + roi.transform(Geospace.get().getDefaultCRS()));
		}
		
		roi = region; 	
	}
	
	public void addModule(AriesModule module) {
		modules.add(module);
		moduleIndex.put(module.getModuleId(), module);
	}
	
	public AriesModule getModule(String id) {
		return moduleIndex.get(id);
	}
	
	public Collection<AriesModule> getAllModules() {
		return modules;
	}
	
	public void initializeModules() {
		modules.clear();
		moduleIndex.clear();
		for (Storyline s : this.storyline.getChildren()) {
			
			if (!s.isEnabled())
				continue;
			/*
			 * use the class specified in the module storyline if any is
			 * given; if not, create a stock AriesModule.
			 */
			AriesModule mod = null;
			String scl = s.getTemplate().
				getDefault("module-class", AriesModule.class.getCanonicalName());
			try {
				Class<?> amc = Class.forName(scl, true, ARIESWebappPlugin.get().getClassLoader());
	            Constructor<?> ct = amc.getConstructor(Storyline.class);
				mod = (AriesModule) ct.newInstance(s);
			} catch (Exception e) {
				throw new ThinklabRuntimeException(e);
			}
			addModule(mod);
		}
	}

	public void startAnalysis() throws ThinklabException {
		
		/*
		 * transform to WSG84 just before starting, ensuring predictable axis order
		 */
		roi = roi.transform(Geospace.get().getStraightGeoCRS());

		/*
		 * build a context and build the user storyline in it.
		 * TODO set any other user properties and defaults in the context.
		 */
		setCurrentContext(Context.getContext(this.roi, getResolution()));
		
		/*
		 * communicate region to all modules; they will enable as allowed by data availability
		 */
		for (AriesModule module : modules) {
			Storyline storyline = module.notifyContext(context);
			if (storyline != null) {
				this.storyline.add(storyline);
			}
		}
	}
	
	private void setCurrentContext(IContext context) throws ThinklabException {

		this.context = context;
		this.storyline.setContext(context);
		
	}
	
	public void clearAnalysis() throws ThinklabException {
		
		for (AriesModule module : modules) {
			module.clearAnalysis();
		}
		roi = null;
		storyline = 			
			StorylineFactory.getStorylines(
				ARIESWebappPlugin.get().getProperties().getProperty(
						ROOT_STORYLINE_PROPERTY, "aries"));
	}

	public String toString() {
		return "ARIES anonymous user";
	}

	/**
	 * Return the saved location IDs for this user, or null.
	 * @return
	 */
	public ArrayList<Location> getDefaultLocations() {
		
		ArrayList<Location> ret = new ArrayList<Location>();
		
		/*
		 * TODO fill it in as configured for user
		 */
		
		return ret;
	}

	public Storyline getStoryline() {
		return storyline;
	}

}
