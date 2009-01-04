package org.integratedmodelling.aries.core.tasks.prioritization;

import java.util.ArrayList;
import java.util.Collection;

import org.integratedmodelling.thinklab.KnowledgeManager;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.IConcept;
import org.integratedmodelling.thinklab.interfaces.ISession;
import org.integratedmodelling.thinklab.interfaces.ITask;

public class SelectEcosystemServices implements ITask {
	
	ArrayList<IConcept> es = new ArrayList<IConcept>();

	public SelectEcosystemServices() throws ThinklabException {
		
		es.add(KnowledgeManager.get().requireConcept("carbonService:CarbonStorage"));
		es.add(KnowledgeManager.get().requireConcept("floodService:FloodPreventionService"));
		es.add(KnowledgeManager.get().requireConcept("soilretentionService:SoilRetentionService"));
		es.add(KnowledgeManager.get().requireConcept("rawMaterialsService:RawMaterials"));
	}
	
	public Collection<IConcept> getEcosystemServices() {
		return es;
	}

	@Override
	public void run(ISession session)  throws ThinklabException {
		// TODO Auto-generated method stub
	}
}
