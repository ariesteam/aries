package org.integratedmodelling.aries.core.tasks.prioritization;

import java.util.ArrayList;
import java.util.Collection;

import org.integratedmodelling.aries.core.ARIESCorePlugin;
import org.integratedmodelling.aries.core.ARIESNamespace;
import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.applications.annotations.TaskNamespace;
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
public class SelectBenefits implements ITask {

	private ArrayList<IConcept> benefits = new ArrayList<IConcept>();
	private IConcept es;

	public void setEcosystemService(IConcept ecosystemService) {
		this.es = ecosystemService;
	}

	@Override
	public void run(ISession session)  throws ThinklabException  {
		
		IProperty p = KnowledgeManager.get().requireProperty(ARIESNamespace.HAS_BENEFIT);
		for (IConcept r : es.getPropertyRange(p)) {
				
				// FIXME temporary; should not have the parent benefit in range,
				// but it does :)
				//if (r.getConceptSpace().equals("eserv"))
				//	continue;
				
				benefits.add(r);				
			}

	}
	
	public Collection<IConcept> getBenefits() {
		return benefits;
	}
}
