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
