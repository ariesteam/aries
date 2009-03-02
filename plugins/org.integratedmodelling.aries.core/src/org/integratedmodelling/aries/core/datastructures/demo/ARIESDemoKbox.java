/**
 * ARIESDemoKbox.java
 * ----------------------------------------------------------------------------------
 * 
 * Copyright (C) 2008 www.integratedmodelling.org
 * Created: Apr 25, 2008
 *
 * ----------------------------------------------------------------------------------
 * This file is part of ARIES.
 * 
 * ARIES is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * ARIES is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * ----------------------------------------------------------------------------------
 * 
 * @copyright 2008 www.integratedmodelling.org
 * @author    Ferdinando Villa (fvilla@uvm.edu)
 * @date      Apr 25, 2008
 * @license   http://www.gnu.org/licenses/gpl.txt GNU General Public License v3
 * @link      http://www.integratedmodelling.org
 **/
package org.integratedmodelling.aries.core.datastructures.demo;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.integratedmodelling.aries.core.ARIESCorePlugin;
import org.integratedmodelling.aries.core.exceptions.ARIESException;
import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.opal.OPALValidator;
import org.integratedmodelling.thinklab.constraint.Constraint;
import org.integratedmodelling.thinklab.constraint.Restriction;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabIOException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IInstance;
import org.integratedmodelling.thinklab.interfaces.query.IQuery;
import org.integratedmodelling.thinklab.interfaces.query.IQueryResult;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.thinklab.interfaces.storage.IKBoxCapabilities;
import org.integratedmodelling.thinklab.kbox.KBoxManager;
import org.integratedmodelling.thinklab.kbox.ListQueryResult;
import org.integratedmodelling.utils.Polylist;
import org.integratedmodelling.utils.instancelist.InstanceList;

/**
 * The kbox used for the demo data. Returns sets of data observations by just matching the
 * observable benefit passed. The real kboxes can be implemented in the same way as a 
 * KBoxWrapper over an ObservationKBox.
 * 
 * @author Ferdinando Villa
 *
 */
public class ARIESDemoKbox implements IKBox {

	File dataDir = null;
	
	ArrayList<Polylist> data = new ArrayList<Polylist>();
	
	public ARIESDemoKbox() throws ThinklabException {
		
		/*
		 * find demo data dir; if nowhere, complain
		 */
		dataDir = 
			new File(ARIESCorePlugin.get().getLoadDirectory() + "/demo/kbox");
		
		if (!dataDir.exists() || !dataDir.isDirectory() || !dataDir.canRead())
			throw new ARIESException(
					"aries: demo data directory " +
					dataDir +
					" is not readable");
		/*
		 * Read in all XML files found in demo kbox directory
		 */
		for (String s : dataDir.list()) {
			if (s.endsWith(".xml")) {
				
				URL url;
				try {
					url = new File(dataDir + File.separator + s).toURI().toURL();
				} catch (MalformedURLException e) {
					throw new ThinklabIOException(e);
				}
				
				Collection<Polylist> obss =
					new OPALValidator().validateToLists(url);

				if (obss != null && obss.size() > 0)
					data.addAll(obss);
			}
		}
	}
	
	public IKBoxCapabilities getKBoxCapabilities() {
		// TODO Auto-generated method stub
		return null;
	}

	public Polylist getObjectAsListFromID(String id,
			HashMap<String, String> refTable) throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public IInstance getObjectFromID(String id, ISession session)
			throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public IInstance getObjectFromID(String id, ISession session,
			HashMap<String, String> refTable) throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public String storeObject(Polylist list, ISession session)
			throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public String storeObject(Polylist list, ISession session,
			HashMap<String, String> refTable) throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public String storeObject(IInstance object, ISession session)
			throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public String storeObject(IInstance object, ISession session,
			HashMap<String, String> references) throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public IQuery parseQuery(String toEval) throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * We only serve global data, so we ignore everything except the observable;
	 * plus, we only care for benefit observables, and we return predefined
	 * data for each one, loaded from an OPAL file with the same name as the
	 * benefit (- replaces : in the semantic type) and .xml as extension, in 
	 * a directory set by a plugin property.
	 */
	public IQueryResult query(IQuery q) throws ThinklabException {
		
		/*
		 * check if we're asking for something that observes a benefit
		 * if so, return all data we have for it
		 * otherwise, return null.
		 */
		Constraint c = (Constraint) q;
		
		/*
		 * extract the observable from the query, ignore the rest
		 */
		Restriction rs = c.findRestriction(CoreScience.HAS_OBSERVABLE);
		IConcept observable = rs.getSubConstraint().getConcept();
		
		/* retrieve all lists representing observations with given observable */
		ArrayList<Polylist> results = new ArrayList<Polylist>();
		
		for (Polylist pl : data) {

			InstanceList il = new InstanceList(pl);
			if (il.getTargetConcept(CoreScience.HAS_OBSERVABLE).equals(observable)) {
				results.add(pl);
			}
		}
		
		return new ListQueryResult(q, this, results);
			
	}

	public IQueryResult query(IQuery q, int offset, int maxResults)
			throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public IQueryResult query(IQuery q, Polylist resultSchema, int offset,
			int maxResults) throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	public Polylist getMetadataSchema() throws ThinklabException {
		// TODO Auto-generated method stub
		return KBoxManager.get().getDefaultSchema();
	}

	@Override
	public String getUri() {
		// TODO Auto-generated method stub
		return null;
	}

}
