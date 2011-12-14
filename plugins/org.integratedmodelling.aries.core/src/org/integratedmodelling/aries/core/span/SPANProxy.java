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

package org.integratedmodelling.aries.core.span;

import java.util.Map;
import java.util.Collection;

import org.integratedmodelling.corescience.interfaces.IObservationContext;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

/**
 * Used to talk to the GSSM Clojure side. This is used in a proxy form to yield an object that can be used
 * to run GSSM from Java.
 * 
 * @author Ferdinando Villa
 *
 */
public interface SPANProxy {

	/**
	 * Run a GSSM simulation from four observations of source, sink, use and flow. Choose the flow model 
	 * according to the observable of the flow observation.
	 * 
	 * @param source
	 * @param use
	 * @param sink
	 * @param flow
	 * @return a flow map, from which results can be obtained.
	 */
	public abstract Map<?,?> runSPAN(
			IObservationContext sourceCtx, 
			IConcept sourceConcept, IConcept useConcept, IConcept sinkConcept, Collection<IConcept> flowConcepts,
			Map<?,?> flowParams);
	
}
