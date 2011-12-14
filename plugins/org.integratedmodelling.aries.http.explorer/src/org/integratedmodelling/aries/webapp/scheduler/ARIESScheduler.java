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
