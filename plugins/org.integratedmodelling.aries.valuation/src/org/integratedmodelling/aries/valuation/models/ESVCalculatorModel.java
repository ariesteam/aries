package org.integratedmodelling.aries.valuation.models;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.integratedmodelling.corescience.CoreScience;
import org.integratedmodelling.corescience.interfaces.internal.Topology;
import org.integratedmodelling.modelling.DefaultAbstractModel;
import org.integratedmodelling.modelling.interfaces.IModel;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabValidationException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;
import org.integratedmodelling.utils.Polylist;

import clojure.lang.PersistentList;

public class ESVCalculatorModel extends DefaultAbstractModel {
	
	/**
	 * Array of these is passed to the transformer to modify the values if required. 
	 * 
	 * @author Ferdinando
	 *
	 */
	public static class Influence {
		
		// observable of influencing dependency. Double values will be used.
		public IConcept observable;
		// total influence of factor (0 to 1), positive or negative
		public double   influenceFactor;
		// minimum level that has influence 0 (clips value)
		public double   min;
		// level of influence 1 (clips value)
		public double   max;
	}

	private boolean normalize = false;
	private ArrayList<Influence> influences = new ArrayList<Influence>();
	
	@Override
	protected void validateMediatedModel(IModel model)
			throws ThinklabValidationException {
	}

	@Override
	public IConcept getCompatibleObservationType(ISession session) {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public void applyClause(String keyword, Object argument)
			throws ThinklabException {

		if (keyword.equals(":influence"))
			parseInfluences(argument);
		else if (keyword.equals(":normalize"))
			this.normalize = (Boolean)argument;
		else
			super.applyClause(keyword, argument);
	}

	private void parseInfluences(Object argument) throws ThinklabException {
		PersistentList ls = (PersistentList) argument;

		for (Iterator<?> it = ls.iterator(); it.hasNext(); ) {
			PersistentList l = (PersistentList) it.next();
			
			int i = 0;
			Influence inf = new Influence();
			for (Iterator<?> ot = l.iterator(); ot.hasNext(); ) {
				
				/**
				 * FIXME this assumes a specific format, should be changed asap.
				 */
				Object o = ot.next();
				switch(i) {
				case 0: inf.observable = KnowledgeManager.get().requireConcept(o.toString()); break;
				case 1: inf.influenceFactor = ((Number)o).doubleValue(); break;
				case 3: inf.min = ((Number)o).doubleValue(); break;
				case 5: inf.max = ((Number)o).doubleValue(); break;
				}
				i++;
			}
			influences.add(inf);
		}
	}

	@Override
	protected void copy(DefaultAbstractModel model) {
		// TODO Auto-generated method stub
		super.copy(model);
	}

	@Override
	public Polylist buildDefinition(IKBox kbox, ISession session, Collection<Topology> extents, int flags) throws ThinklabException {

		return Polylist.list(
			"esvaluation:ESVCalculator",
			Polylist.list(":normalize", new Boolean(this.normalize)),
			Polylist.list(":influences", this.influences),
			Polylist.list(
				CoreScience.HAS_OBSERVABLE,
				Polylist.list(getObservable())));
		
	}

	@Override
	public Polylist conceptualize() throws ThinklabException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void validateSemantics(ISession session) throws ThinklabException {
		// TODO Auto-generated method stub
	}

	@Override
	public IModel getConfigurableClone() {
		
		ESVCalculatorModel ret = new ESVCalculatorModel();
		ret.copy(this);
		return ret;
	}
}
