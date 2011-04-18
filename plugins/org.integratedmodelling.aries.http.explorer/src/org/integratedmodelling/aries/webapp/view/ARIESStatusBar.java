package org.integratedmodelling.aries.webapp.view;

import java.util.Hashtable;

import org.integratedmodelling.aries.webapp.scheduler.ARIESScheduler;
import org.integratedmodelling.thinklab.webapp.TC;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.utils.Pair;
import org.integratedmodelling.utils.exec.ITaskScheduler;
import org.zkoss.zul.Window;

public class ARIESStatusBar extends Window {

	// messages
	public final static String SET = "set";
	
	private static final long serialVersionUID = -6653953855887408421L;

	String statusIcon = null;
	String message = null;
	String statusCode = null;
		
	int waiting = -1;
	int executi = -1;
	
	/*
	 * contains a map of code -> (message, icon)
	 */
	Hashtable<String, Pair<String, String>> statusCodes = null;

	private AriesBrowser browser;
	
	class TaskListener implements ITaskScheduler.Listener {

		private void setTaskLabels(int executing, int scheduled) {
			waiting = scheduled;
			executi = executing;
			browser.send(ARIESStatusBar.this, "display", (Object[])null);
		}
		
		@Override
		public void notifyTaskEnqueued(Thread task, int currentlyExecuting,
				int currentlyScheduled) {
			 setTaskLabels(currentlyExecuting, currentlyScheduled);
		}

		@Override
		public void notifyTaskFinished(Thread task, int currentlyExecuting,
				int currentlyScheduled) {
			 setTaskLabels(currentlyExecuting, currentlyScheduled);
		}

		@Override
		public void notifyTaskStarted(Thread task, int currentlyExecuting,
				int currentlyScheduled) {
			 setTaskLabels(currentlyExecuting, currentlyScheduled);
		}
	}
	
	public ARIESStatusBar(Hashtable<String, Pair<String, String>> codes, 
				AriesBrowser browser) {
		statusCodes = codes;
		this.browser = browser;
		ARIESScheduler.get().addListener(new TaskListener());
	}
	
	public void initialize() {
		set("idle");
	}
	
	public void set(String state) {
		setState(state);
		display();
	}
	
	public void display() {
		
		int ex = (executi < 0 ?
					ARIESScheduler.get().getRunningModelsCount() :
					executi);

		int wa = (waiting < 0 ? 
					ARIESScheduler.get().getWaitingModelsCount() :
					waiting);

		
		ZK.resetComponent(this);
		
		appendChild(
			ZK.hbox(
				ZK.hbox(
					ZK.label(
						"Users: " +
						browser.getApplication().getCurrentUserCount()),
					ZK.separator().width(2),
					ZK.image(TC.url(browser.getApplication(), 
							(ex > 0 ? 
									"/images/icons/bullet_red.png" : 
									"/images/icons/bullet_green.png"))).tmargin(4),
					ZK.separator().width(2),
					ZK.label(
						"Running " +
						 ex +
						"/" + 
						ARIESScheduler.get().getMaxRunningModelsCount()),
					ZK.separator().width(2),
					ZK.image(TC.url(browser.getApplication(), 
								(wa > 0 ? 
										"/images/icons/bullet_red.png" : 
										"/images/icons/bullet_green.png"))).tmargin(4),
					ZK.separator().width(2),
					ZK.label("Waiting " + wa)
				).sclass(STYLE.BORDERED_TEXT)
				 .tooltip("Number of current users, models running/allowed concurrently, and models waiting in queue for computation."),
				ZK.separator(false).width(4),
				ZK.label(message)
			).sclass(STYLE.STATUS_BAR).get());
	}
	
	private void setState(String state) {
		
		Pair<String, String> p = statusCodes.get(state);
		
		message = p == null ? "" : p.getFirst();
		statusIcon = p == null ? 
				"" : // TODO put in a question mark thingy
				p.getSecond();
	}


}