package org.integratedmodelling.aries.core.tasks.configuration;

import org.integratedmodelling.aries.core.datastructures.demo.ARIESDemoKbox;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.interfaces.applications.ITask;
import org.integratedmodelling.thinklab.interfaces.applications.annotations.TaskNamespace;
import org.integratedmodelling.thinklab.interfaces.storage.IKBox;

/**
 * Return the demo kbox, creating it if necessary.
 * 
 * @author Ferdinando Villa
 *
 */
@TaskNamespace(ns = "aries")
public class GetDemoDataKbox implements ITask {

	static IKBox kbox = null;
	
	@Override
	public void run(ISession session) throws ThinklabException {
		
		if (kbox == null)
			kbox = new ARIESDemoKbox();
	}
	
	public IKBox getKbox() {
		return kbox;
	}

}
