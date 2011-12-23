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

package org.integratedmodelling.aries.webapp.view;

import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.view.components.ThinkcapComponent;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;

public class AriesBanner extends ThinkcapComponent {

	private static final long serialVersionUID = 2811332230891342315L;
	Storyline storyline = null;
//	LoginBox loginbox = null;
	AriesBrowser browser;
	
	@Override
	public void initialize() {
		display();
	}

	public AriesBrowser getBrowser() {
		
		if (browser == null) {
			browser = (AriesBrowser) ZK.getComponentById(this.getParent(), "workspace");
		}
		return browser;
	}
	
	public void setStoryline(Storyline s) {
		this.storyline = s;
		display();
	}
	
	public void display() {
		
		clear();
		
		setContent(
				ZK.vbox(
					ZK.hbox(
						ZK.div(
							ZK.image("/aries/images/logos/arieslogo.gif")).width(236).align("left"),
						ZK.div(
							ZK.vbox(
								ZK.separator(false).height(60),
								ZK.bar(),
								ZK.div(
									ZK.hbox(
										ZK.label("Not logged in").
											sclass(STYLE.TEXT_VERYSMALL),
										storyline == null ?
											null : 
											ZK.imagebutton("/images/icons/home.png").
												tooltip("Show case study information page").
												listener("onClick", new EventListener() {
													@Override
													public void onEvent(Event arg0) throws Exception {
														getBrowser().showStoryline(storyline, null);
													}
											}),
											storyline == null ?
													null : 
													ZK.imagebutton("/images/icons/delete.png").
														tooltip("Delete this case study and start over").
														listener("onClick", new EventListener() {
															@Override
															public void onEvent(Event arg0) throws Exception {
																getBrowser().resetCaseStudy();
															}
													})
											)).
									align("right")
								/*ZKAUTH.loginbox().id("loginbox")*/).height(88).width(1040))),
						ZK.separator(true).width(1276)).width("100%"));
			

	}
	
	@Override
	public void postInitialize() {
		//loginbox.initialize();
		//loginbox.afterCompose();
	}
	
}
