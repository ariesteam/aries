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

package org.integratedmodelling.aries.webapp.webapps;

import org.integratedmodelling.thinklab.http.application.ThinklabWebApplication;
import org.integratedmodelling.thinklab.http.extensions.WebApplication;

@WebApplication(
		name="aries", 
		modelClass="org.integratedmodelling.aries.webapp.model.ARIESUserModel",
		shortDescription="ARIES Ecosystem Services Explorer")
public class ARIESWebApplication extends ThinklabWebApplication {


}
