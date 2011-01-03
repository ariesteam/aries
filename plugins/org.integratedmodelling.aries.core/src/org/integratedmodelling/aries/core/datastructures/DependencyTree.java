/**
 * DependencyGraph.java
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
package org.integratedmodelling.aries.core.datastructures;

import org.integratedmodelling.aries.core.ARIESNamespace;

import org.integratedmodelling.thinklab.dolce.DOLCE;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.graph.KnowledgeGraph;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IKnowledgeSubject;
import org.integratedmodelling.thinklab.interfaces.knowledge.IProperty;
import org.integratedmodelling.thinklab.interfaces.knowledge.IRelationship;

//public class DependencyTree extends KnowledgeGraph {
//
//	private static final long serialVersionUID = -6151570001288627653L;
//
//	public DependencyTree(IConcept rootObservable) throws ThinklabException {
//		
//		forceTreeGeometry(true);
//		buildGraph(rootObservable);
//	}
//	
//	@Override
//	protected boolean followRelationship(IKnowledgeSubject source,
//			IRelationship relationship, IConcept target) {
//		/* for now */
//		return false;
//	}
//
//	@Override
//	protected boolean followProperty(IConcept source, IConcept target,
//			IProperty property) {
//		
//		return 
//			property == null || 
//			property.is(ARIESNamespace.HAS_PROVISION) ||
//			property.is(DOLCE.IMMEDIATE_RELATION) ||
//			property.is(DOLCE.IMMEDIATE_RELATION_I);
//	}
//
//}
