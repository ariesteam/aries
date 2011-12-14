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

package org.integratedmodelling.aries.webapp.model.integrated;

import java.util.ArrayList;

import org.integratedmodelling.aries.webapp.interfaces.IMonitoringModule;
import org.integratedmodelling.aries.webapp.model.AriesModule;
import org.integratedmodelling.corescience.interfaces.IState;
import org.integratedmodelling.mca.MCA;
import org.integratedmodelling.modelling.storyline.ModelStoryline;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.utils.Pair;

/**
 * Supports a regular MCA, mapping the observable of each computed storyline
 * to a criterion. In addition, it allows alternative views of each criterion, 
 * based on the aggregated value of their states. Each state incarnating 
 * mca:Utility is suitable to be the value for the criterion, and the storyline
 * can define further concepts that are used to provide a choice between 
 * different views of multiple utilities in the same model. Each alternative
 * must pick one of those views per computed model when creating an alternative.
 * 
 * @author ferdinando.villa
 *
 */
public class MCAModule extends AriesModule implements IMonitoringModule {

	MCA _mca = new MCA();
	
	IConcept utilityConcept = null;
	ArrayList<IConcept> utilityViews = new ArrayList<IConcept>();
	
	class AlternativeView {
		
		IConcept criterion;
		ArrayList<Pair<IConcept, Double>> values = 
			new ArrayList<Pair<IConcept,Double>>();
		
	}
	
	public MCAModule(Storyline s) {

		super(s);
		
		utilityConcept = 
			KnowledgeManager.getConcept(
					s.getTemplate().getDefault("utility", "mca:Utility"));
		
		/*
		 * define utility and view concepts
		 */
		for (String sc : s.getTemplate().getAll("utility-view")) {
			utilityViews.add(KnowledgeManager.getConcept(sc));
		}
	}

	@Override
	public synchronized void onStorylineComputed(ModelStoryline storyline) {

		IConcept criterion = storyline.getObservable();
		
		/*
		 * find all the values we can use in the context
		 */
		for (IState state : storyline.getContext().getStates()) {
			if (state.getObservableClass().is(utilityConcept)) {
				
			}
		}
		
		/*
		 * recompute MCA 
		 */
		compute();
		
	}

	private void compute() {
		
		/*
		 * reset MCA and add all chosen criteria and configured
		 * alternatives
		 */
		
		/*
		 * compute MCA and store results
		 */
		
	}

}
