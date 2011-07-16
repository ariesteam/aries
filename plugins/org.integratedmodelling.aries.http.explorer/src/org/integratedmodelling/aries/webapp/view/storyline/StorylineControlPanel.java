package org.integratedmodelling.aries.webapp.view.storyline;


import java.util.ArrayList;

import org.integratedmodelling.aries.webapp.interfaces.IMonitoringModule;
import org.integratedmodelling.aries.webapp.model.AriesModule;
import org.integratedmodelling.aries.webapp.scheduler.ARIESScheduler;
import org.integratedmodelling.aries.webapp.view.AriesBrowser;
import org.integratedmodelling.aries.webapp.view.AriesModuleView;
import org.integratedmodelling.aries.webapp.view.STYLE;
import org.integratedmodelling.aries.webapp.visualization.WebZKVisualization;
import org.integratedmodelling.corescience.interfaces.IContext;
import org.integratedmodelling.modelling.interfaces.IModel;
import org.integratedmodelling.modelling.interfaces.IVisualization;
import org.integratedmodelling.modelling.storyline.ModelStoryline;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.applications.ISession;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.view.components.ThinkcapComponent;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.StringUtils;
import org.integratedmodelling.utils.exec.ITaskScheduler;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;
import org.zkoss.zul.Toolbarbutton;

/**
 * Component that serves as the gateway to a storyline view. Each configured model gets its
 * storyline; new "child" storylines can be added by creating scenarios using the buttons on
 * the badge or the info page. Computation of storylines can be started from the badge or from 
 * the info page that clicking on the badge brings up. This shoud be the only object that keeps
 * the link between the storyline and its view, or the MVC paradigm is broken.
 * 
 * @author Ferdinando
 *
 */
public class StorylineControlPanel extends ThinkcapComponent {

	private static final long serialVersionUID = 2245573842208185105L;
	
	ModelStoryline storyline = null;
	AriesBrowser browser = null;
	AriesModuleView mview;
	ModelStorylineView sview;
	
	public StorylineControlPanel(Storyline storyline, AriesBrowser browser, AriesModuleView view) {
		this.storyline = (ModelStoryline) storyline;
		this.browser = browser;
		this.mview = view;
		this.sview = 
			(ModelStorylineView) browser.createStorylineView(storyline, this);
		initialize();
	}
	
	class Listener implements ModelStoryline.Listener {
		
		private ISession session;
		private String directory;
		private String urlPrefix;
		private int vx;
		private int vy;
		private ArrayList<IMonitoringModule> notified =
			new ArrayList<IMonitoringModule>();

		public Listener(int vx, int vy, ISession session, String directory, String urlPrefix) {
			this.session = session;
			this.directory = directory;
			this.urlPrefix = urlPrefix;
			this.vx = vx;
			this.vy = vy;
			
			/*
			 * find all modules that want notification and add them to notify list when 
			 * status changes to computed.
			 */
			for (AriesModule m : browser.getUserModel().getAllModules()) {
				if (m instanceof IMonitoringModule) {
					notified.add((IMonitoringModule) m);
				}
			}
 		}
		
		@Override
		public void onStatusChange(Storyline storyline, IModel model, IContext context, int original, int newstatus) {

			// only refresh to computed after the visualization has been created, which takes a while
			if (! (original == Storyline.COMPUTING && newstatus == Storyline.COMPUTED)) {
				browser.send(browser, "refreshStoryline", storyline.getTemplateSignature());
			}
		}

		@Override
		public ITaskScheduler getScheduler() {
			return ARIESScheduler.get();
		}

		@Override
		public IVisualization createVisualization(IModel model, IContext context) {
			
			WebZKVisualization ret = null;
			try {
				ret = new WebZKVisualization(context, directory, urlPrefix);
				ret.setViewPort(vx, vy);
			} catch (ThinklabException e) {
				throw new ThinklabRuntimeException(e);
			}
			return ret;
		}

		@Override
		public ISession getSession() {
			return session;
		}

		@Override
		public void notifyVisualization(Storyline modelStoryline, IModel model,
				IContext context, IVisualization visualization) {
			((ModelStoryline)storyline).setVisualization(visualization);
			browser.send(browser, "refreshStoryline", storyline.getTemplateSignature());
			for (IMonitoringModule m : notified) {
				m.onStorylineComputed((ModelStoryline) storyline);
			}
		}

		@Override
		public void notifyError(ModelStoryline modelStoryline, IModel model,
				IContext context, Exception e) {
			// TODO Auto-generated method stub
			
		}
	}
	
	@Override
	public void initialize() {
		display();
	}
	
	public void display() {

		clear();
		
		/*
		 * position in hierarchy: either 
		 * 
		 * 0 = no hierarchy (we have no children)
		 * 1 = we are the root of a set of scenarios
		 * 2 = we are a scenario but not the last one
		 * 3 = we are the last scenario
		 * 
		 * used to choose the left-side bracket image to group
		 * scenarios together.
		 */
		int pos = 0;
		if (this.storyline.getParent() != null &&
			this.storyline.getParent() instanceof ModelStoryline) {
			if (this.storyline.getNextSibling() == null)
				pos = 3;
			else
				pos = 2;
		} else if (this.storyline.getChildren().size() > 0) {
			pos = 1;
		}
			
		String butImage = ZK.fixUrl("/images/icons/play48.png");
		String tooltip  = "Start computation of this storyline.";
		String mainttp  = storyline.getTemplate().get("description");
		if (mainttp == null)
			mainttp = "Click to show the storyline in its current status.";
		else 
			mainttp = StringUtils.pack(mainttp);
			
		if (storyline.getStatus() == ModelStoryline.COMPUTING) {
			butImage = ZK.fixUrl("/images/icons/spinner48.gif");
			tooltip = "The storyline is computing. Please wait for it to finish.";
		} else if (storyline.getStatus() == ModelStoryline.PENDING) {
			// TODO a different wait icon would be nice
			butImage = ZK.fixUrl("/images/icons/spinner48.gif");
			tooltip = "The storyline is waiting for its turn to be computed.";
		} else if (storyline.getStatus() == ModelStoryline.ERROR) {
			butImage = ZK.fixUrl("/images/icons/warning48.png");
			tooltip = "The computation completed with errors.";
		}  else if (storyline.getStatus() == ModelStoryline.COMPUTED) {
			butImage = ZK.fixUrl("/images/icons/check48_green.png");
			tooltip = "The storyline has been successfully computed.";
		} else if (storyline.getStatus() == ModelStoryline.DISABLED) {
			butImage = ZK.fixUrl("/images/icons/boh48.png");
			tooltip = "This storyline cannot be computed in this region.";
		}	
		
		setContent(
			ZK.hbox(
				ZK.div(
						
					/*
					 * narrow placeholder to show tree alignment. Fortunately we can only have a two level hierarchy.
					 */
					ZK.div(
						ZK.image(/*"aries/images/hs" + pos + ".png"*/null)
					).align("left").width(4),
					
					/*
					 * image: compute, idle, error or done. Click on compute to compute, any other state
					 * click brings up info page.
					 */
					ZK.vbox(
						ZK.separator().height(10),
						ZK.imagebutton(butImage).
							tooltip(tooltip).
							enable(storyline.getStatus() == Storyline.IDLE).
						 	listener("onClick", new EventListener() {
						 		@Override
						 		public void onEvent(Event arg0) throws Exception {
						 			
									Pair<String, String> zr =
										browser.getApplication().getNewDirectoryUrl(browser.getSession());
									
						 			storyline.compute(
											new Listener(
													ModelStorylineView.vx, ModelStorylineView.vy,
													browser.getSession(), zr.getFirst(), zr.getSecond()));
						 			
						 			((Toolbarbutton)arg0.getTarget()).setImage(
						 					ZK.fixUrl("/images/icons/spinner48.gif"));
						 			browser.showStoryline(storyline, null);
						 		}
						 	})).align("center").width(48)),
					
					/*
					 * main area with title and brief description
					 */
					ZK.div(
						ZK.vbox(
								ZK.separator().height(3),
								ZK.label(storyline.getTemplate().getTitle()).sclass(
										storyline.getStatus() == Storyline.DISABLED ? STYLE.TEXT_SMALL : STYLE.TEXT_SMALL_BRIGHT),
								ZK.bar(),
								ZK.text(storyline.getTemplate().getRunningHead()).sclass(STYLE.TEXT_VERYSMALL))
					).fillx().height(64).hand().
					  tooltip(mainttp).
					  listener("onClick", new EventListener() {
						
						@Override
						public void onEvent(Event arg0) throws Exception {
							browser.showStoryline(storyline, StorylineControlPanel.this);
						}
					}),
					
					/*
					 * menu: new storyline, edit (only enabled if derived), delete
					 */
					ZK.div(
						ZK.vbox(
								ZK.imagebutton("/images/icons/plus24.png").
									tooltip("Create and compute a scenario derived from this storyline\n\nAt the moment scenario definition is not accessible to unprivileged users.").
									listener("onClick", new EventListener() {
										@Override
										public void onEvent(Event arg0) throws Exception {
											browser.createScenario(storyline);
										}
									}),
								ZK.imagebutton("/images/icons/minus24.png").
									tooltip("Remove this storyline from the case study. \n\nAt the moment this feature is not accessible by unprivileged users."),
								ZK.div().height(30)
						).height(64)).height(64).align("right")).fillx());

	}

	@Override
	public void postInitialize() {

	}

}
