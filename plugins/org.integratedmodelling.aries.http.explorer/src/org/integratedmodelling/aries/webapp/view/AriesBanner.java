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
											ZK.imagebutton("/aries/tc/images/icons/home.png").
												tooltip("Show case study information page").
												listener("onClick", new EventListener() {
													@Override
													public void onEvent(Event arg0) throws Exception {
														getBrowser().showStoryline(storyline, null);
													}
											}),
											storyline == null ?
													null : 
													ZK.imagebutton("/aries/tc/images/icons/delete.png").
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
