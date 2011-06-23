package org.integratedmodelling.aries.webapp.view;

import java.util.ArrayList;
import java.util.Collection;

import org.integratedmodelling.aries.webapp.model.AriesModule;
import org.integratedmodelling.aries.webapp.view.storyline.StorylineControlPanel;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.ZK.ZKComponent;
import org.integratedmodelling.thinklab.webapp.view.components.SidebarModule;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;
import org.zkoss.zul.Toolbarbutton;

public class AriesModuleView extends SidebarModule {

	private static final long serialVersionUID = 4692188438190024543L;
	private AriesModule module = null;
	protected AriesBrowser browser = null;
	private boolean covShown = false;
	
	private String label;

	protected String getModuleLabel() {
		return label;
	}
	
	public AriesModuleView(AriesModule module, AriesBrowser browser) {

		this.module = module;
		this.setHeaderLabel(this.label = module.getStoryline().getTemplate().getTitle());
		this.setMinimumHeight(280);
		setSclass(STYLE.MODULE_PANEL);
		this.setId(module.getModuleId());
		this.isEnabled = false;
		this.browser = browser;
	}

	@Override
	public void initialize() {
	}
	
	@Override
	public void display() {
		
		clear();
		
		// takes care of some uncoordination, but not all.
		if (!browser.shapeShown)
			covShown = false;
		
		/*
		 * home and coverage buttons only shown if the module is open, to avoid
		 * visual clutter.
		 */
		if (isOpen())
			header.setRightComponent(
				ZK.div(
					ZK.hbox(
							ZK.spacer(4),
							(browser.isContextSet() ? 
								ZK.imagebutton("/images/icons/home.png").
									tooltip("Show the storyline for this module in this region.").
										listener("onClick", new EventListener() {
											
											@Override
											public void onEvent(Event arg0) throws Exception {
												browser.showStoryline(module.getStoryline(), null);
											}
										}) :
									null),
								(browser.isContextSet() ? 
									null : 
										ZK.imagebutton(covShown ? 
												"/images/icons/redeye.png" : 
												"/images/icons/grayeye.png").
											tooltip(covShown ? 
												"Hide the spatial coverage of this storyline." : 
												"Show the spatial coverage of this storyline.").
											listener("onClick", new EventListener() {
									
												@Override
												public void onEvent(Event arg0) throws Exception {
													if (covShown) {
														browser.showCoverage(null);
														((Toolbarbutton)(arg0.getTarget())).setImage(ZK.fixUrl("/images/icons/grayeye.png"));
														((Toolbarbutton)(arg0.getTarget())).setTooltiptext("Show where this storyline can be computed.");
													} else {
														browser.showCoverage(module.getCoverage());
														((Toolbarbutton)(arg0.getTarget())).setImage(ZK.fixUrl("/images/icons/redeye.png"));
														((Toolbarbutton)(arg0.getTarget())).setTooltiptext("Hide the spatial coverage of this storyline.");
													}
													covShown = !covShown;
												}
											}))
						)
					).align("right").tmargin(3).rmargin(3));
		
		ArrayList<ZKComponent> badges = new ArrayList<ZK.ZKComponent>();

		badges.add(ZK.separator().height(4));

		Collection<Storyline> storylines = module.getStorylines();
		for (Storyline s : storylines) {
			badges.add(ZK.c(new StorylineControlPanel(s, browser, this)).width("98%").sclass(STYLE.BORDERED_SUNK));
		}
		
		if (storylines.size() == 0) {
			badges.add(ZK.text("No storylines available to compute in this region").
					fillx().align("center").sclass(STYLE.TEXT_LARGE_BOLD_GREY));
			header.setLabelClass(STYLE.MODULE_TITLE_DISABLED);
		} else {
			header.setLabelClass(STYLE.MODULE_TITLE);
		}
//		badges.add(ZK.bar().width("100%"));
		
		setContent(
				ZK.window(
						ZK.vbox((ZKComponent[])badges.toArray(new ZKComponent[badges.size()])).
							fillx())
					.fillx().scroll());
		
		header.redisplay();
	}

	
	protected AriesModule getModule() {
		return module;
	}
	
	public void statusComputed() {
		header.setLabelClass(STYLE.MODULE_TITLE_COMPUTED);
		header.setRightImage(null);
		header.redisplay();
	}
		
	public void resetToInitial() {
		display();
	}
	
}
