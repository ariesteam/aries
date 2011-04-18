package org.integratedmodelling.aries.webapp.view.integrated;

import org.integratedmodelling.aries.webapp.model.AriesModule;
import org.integratedmodelling.aries.webapp.view.AriesBrowser;
import org.integratedmodelling.aries.webapp.view.AriesModuleView;

public class MCAView extends AriesModuleView {

	private static final long serialVersionUID = -4047988197767262968L;

	public MCAView(AriesModule module, AriesBrowser browser) {
		super(module, browser);
	}
	
	@Override
	public void display() {
		
		clear();
	}
}
