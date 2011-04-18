package org.integratedmodelling.aries.webapp.scheduler;

import org.integratedmodelling.aries.webapp.ARIESWebappPlugin;
import org.integratedmodelling.utils.exec.TaskScheduler;

/**
 * The singleton that receives modeling tasks. Used throughout to synchronize modeling 
 * and prevent a model clusterfuck.
 * 
 * @author Ferdinando
 *
 */
public class ARIESScheduler extends TaskScheduler {

	static final String MAX_TASKS_PROPERTY = "aries.webapp.models.maxconcurrent";
	
	static ARIESScheduler _scheduler = null;
	
	public static ARIESScheduler get() {
		
		if (_scheduler == null) {
			int ntasks  = 
				Integer.parseInt(
						ARIESWebappPlugin.get().getProperties().getProperty(MAX_TASKS_PROPERTY, "1"));
			_scheduler = new ARIESScheduler(ntasks);
			_scheduler.start();
		}
		return _scheduler;
	}
	
	public ARIESScheduler(int maxConcurrentTasks) {
		super(maxConcurrentTasks);
	}

	public int getRunningModelsCount() {
		return executing();
	}

	public int getMaxRunningModelsCount() {
		return maxTaskCount();
	}

	public int getWaitingModelsCount() {
		return scheduled();
	}
	
}
