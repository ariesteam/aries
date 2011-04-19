package org.integratedmodelling.aries.webapp.webapps;

import org.integratedmodelling.thinklab.http.application.ThinklabWebApplication;
import org.integratedmodelling.thinklab.http.extensions.WebApplication;

@WebApplication(
		name="aries", 
		modelClass="org.integratedmodelling.aries.webapp.model.ARIESUserModel",
		shortDescription="ARIES Ecosystem Services Explorer")
public class ARIESWebApplication extends ThinklabWebApplication {


}
