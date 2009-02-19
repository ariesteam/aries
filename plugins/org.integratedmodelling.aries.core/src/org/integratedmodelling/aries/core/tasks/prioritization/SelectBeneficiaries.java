package org.integratedmodelling.aries.core.tasks.prioritization;

import java.util.ArrayList;
import java.util.Collection;

import org.integratedmodelling.aries.core.ARIESNamespace;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.interfaces.knowledge.IProperty;

/**
 * Return the benefits of interest given the passed ecosystem service.
 * 
 * TODO will need to add context and reasoning for goal-directed prioritization.
 * 
 * TODO see if we should allow passing more ecosystem services and return a comprehensive list of
 * benefits.
 * 
 * @author Ferdinando Villa
 *
 */
@TaskNamespace(ns = "aries")
public class SelectBeneficiaries implements ITask {

	private ArrayList<IConcept> beneficiaries = new ArrayList<IConcept>();
	private IConcept benefit;

	public void setBenefit(IConcept benefit) {
		this.benefit = benefit;
	}

	@Override
	public void run(ISession session)  throws ThinklabException  {
		
		IProperty p = KnowledgeManager.get().requireProperty(ARIESNamespace.HAS_BENEFIT);
		for (IConcept r : benefit.getPropertyRange(p)) {
				beneficiaries.add(r);				
			}

	}
	
	public Collection<IConcept> getBeneficiaries() {
		return beneficiaries;
	}
}
