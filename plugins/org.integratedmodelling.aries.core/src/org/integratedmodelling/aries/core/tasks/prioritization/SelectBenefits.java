package org.integratedmodelling.aries.core.tasks.prioritization;

import java.util.Collection;

import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;

public class SelectBenefits implements ITask {

	public void setEcosystemService(IConcept ecosystemService) {
		
	}

	@Override
	public void run(ISession session)  throws ThinklabException  {
		// TODO Auto-generated method stub
		
	}
	
	public Collection<IConcept> getBenefits() {
		return null;
	}
}
