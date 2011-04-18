package org.integratedmodelling.aries.webapp.interfaces;

import org.integratedmodelling.modelling.storyline.ModelStoryline;

public interface IMonitoringModule {

	/**
	 * NOTE: THIS MUST BE DEFINED AS SYNCHRONIZED TO
	 * WORK CORRECTLY WHEN MODEL RUN IS ASYNCHRONOUS.
	 * 
	 * @param storyline
	 */
	public abstract void onStorylineComputed(ModelStoryline storyline);
	
}
