package org.integratedmodelling.aries.core.tasks.prioritization;

import java.util.Collection;

import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.IConcept;
import org.integratedmodelling.thinklab.interfaces.ISession;
import org.integratedmodelling.thinklab.interfaces.ITask;

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
