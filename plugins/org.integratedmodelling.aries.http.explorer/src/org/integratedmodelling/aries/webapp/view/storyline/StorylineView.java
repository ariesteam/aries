package org.integratedmodelling.aries.webapp.view.storyline;

import java.util.ArrayList;
import java.util.Properties;

import org.integratedmodelling.aries.webapp.view.AriesBrowser;
import org.integratedmodelling.aries.webapp.view.STYLE;
import org.integratedmodelling.modelling.interfaces.IPresentation;
import org.integratedmodelling.modelling.storyline.Storyline;
import org.integratedmodelling.modelling.visualization.storyline.StorylineTemplate;
import org.integratedmodelling.modelling.visualization.storyline.StorylineTemplate.Page;
import org.integratedmodelling.thinklab.exception.ThinklabException;
import org.integratedmodelling.thinklab.exception.ThinklabRuntimeException;
import org.integratedmodelling.thinklab.interfaces.knowledge.IConcept;
import org.integratedmodelling.thinklab.webapp.ZK;
import org.integratedmodelling.thinklab.webapp.ZK.ZKComponent;
import org.integratedmodelling.thinklab.webapp.view.components.ThinkcapComponent;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.EventListener;

/**
 * These are initialized with a storyline and render the associated presentation layout as a 
 * ZK component in the main browser window.
 * 
 * @author Ferdinando
 *
 */
public abstract class StorylineView extends ThinkcapComponent implements IPresentation {

	private static final long serialVersionUID = 4285841459804790349L;
	protected Storyline storyline;
	protected AriesBrowser browser;
	// info page is default. Navigation is enabled if the storyline presentation has 
	// sequence pages too.
	protected int currentPage = -1;

	// pages are kept in a filtered sequence
	ArrayList<Page> sequence = new ArrayList<StorylineTemplate.Page>();
	
	// will not render sequenced pages unless this is true. 
	private boolean showSequence = false;
	
	protected static final int VIEWPORT_X = 600, VIEWPORT_Y = 600, LABEL_HEIGHT = 20;
	
	protected static int TOPBAR_H = 40, BOTBAR_H = 63;
	public  static int vx = VIEWPORT_X, vy = VIEWPORT_Y - TOPBAR_H - BOTBAR_H - 8;
	
	public StorylineView(Storyline s, AriesBrowser browser) {
		initialize(s, browser);
	}
	
	public void initialize(Storyline storyline, AriesBrowser browser) {
		this.storyline = storyline;
		this.browser = browser;
	}
	
	@Override
	public void initialize(Storyline storyline, Properties properties) {
	}

	@Override
	public void render(IConcept concept) throws ThinklabException {
	}

	/*
	 * produce a standard 180x64 badge with given attributes.
	 */
	public static ZKComponent getBadge(int width, String icon, String title, String description, String tooltip, EventListener listener) {
		return
			ZK.hbox(
					ZK.vbox(
							ZK.separator().height(4),
							ZK.imagebutton(icon).
								tooltip(tooltip).
							 	listener("onClick", listener).align("center").width(48)),
						
						/*
						 * main area with title and brief description
						 */
						ZK.div(
							ZK.vbox(
									ZK.separator().height(3),
									ZK.label(title).sclass(listener == null ? STYLE.TEXT_SMALL : STYLE.TEXT_SMALL_BRIGHT),
									ZK.bar(),
									ZK.text(description).sclass(STYLE.TEXT_VERYSMALL))
						).tooltip(tooltip).fillx().height(64)		
			).width(width).height(64).sclass(STYLE.DARK_BG);
	}
	
	/*
	 * create the sequence and set showsequence to true, then render.
	 */
	public void showSequence() throws ThinklabException {
		
		showSequence = true;
		sequence.clear();
		for (int i = 0; i < storyline.getTemplate().getPagesCount(); i++) {
			if (isPageEnabled(storyline.getTemplate().getPage(i))) {
				sequence.add(storyline.getTemplate().getPage(i));
			}
		}
	}
	
	protected boolean isPageEnabled(StorylineTemplate.Page page) {
		return !page.getBoolean("disabled");
	}

	@Override
	public void initialize() {
		try {
			render();
		} catch (ThinklabException e) {
			throw new ThinklabRuntimeException();
		}
	}
	
	protected ArrayList<Page> getPageSequence() {
		return sequence;
	}
	
	@Override
	public void render() throws ThinklabException {
		setupView();
	}
	
	public void switchPage(int page) throws ThinklabException {
		currentPage = page;
		render();
	}
	
	public void setupView() throws ThinklabException {
		
		clear();
		
		// TODO Auto-generated method stub
		StorylineTemplate t = storyline.getTemplate();
		Page info = currentPage == -1 ? t.getPage("info") : sequence.get(currentPage);
		
		int infoh = (int)((double)VIEWPORT_Y * .6);
		int linkh = (VIEWPORT_Y - infoh - (3*LABEL_HEIGHT))/2 - 1;
		int infow = browser.MAPWIDTH - VIEWPORT_X - 6;
		
		setContent(
			ZK.vbox(
					
				/*
				 * main area: display of image and info/credits display
				 */
				ZK.hbox(
					
					/*
					 * image area
					 */
					
					ZK.vbox(
						ZK.hbox(
								ZK.label(t.getTitle() + " :: " + info.getTitle()).sclass(STYLE.TEXT_MAP_TITLE),
								ZK.div(getNavigationControls()).align("right")
							).width(VIEWPORT_X),
						getImageArea(VIEWPORT_X, VIEWPORT_Y)
					),
					ZK.spacer(6),
					ZK.vbox(
							
						ZK.label(info.getDescriptionTitle()).sclass(STYLE.TEXT_MAP_TITLE).height(LABEL_HEIGHT).width(infow),
						
						/*
						 * info
						 */
						ZK.window(ZK.text(info.getHtml("description"))).scroll().sclass(STYLE.BORDERED_HTML_TEXT).width(infow).height(infoh),
							
						/*
						 * credits
						 */
						ZK.label(info.getSeeAlsoTitle()).width(infow).height(LABEL_HEIGHT).sclass(STYLE.TEXT_MAP_TITLE),
						ZK.text(info.getHtml("see-also")).sclass(STYLE.BORDERED_HTML_TEXT).width(infow).height(linkh),
							
						/*
						 * links
						 */
						ZK.label(info.getCreditsTitle()).width(infow).height(LABEL_HEIGHT).sclass(STYLE.TEXT_MAP_TITLE),
						ZK.text(info.getHtml("credits")).sclass(STYLE.BORDERED_HTML_TEXT).width(infow).height(linkh)
							
					).height(VIEWPORT_Y)
				),
				
				/*
				 * browser/chooser for the module storylines
				 */
				ZK.div(getRibbon(browser.MAPWIDTH, 86)).fillx().sclass(STYLE.BORDERED_SUNK_TOPDOWN).height(86)
			).spacing(4).fillx());
	}

	private ZKComponent getNavigationControls() {
		
		ZKComponent ret = null;
		if (storyline.getTemplate().getPagesCount() > 0) {
			ret = ZK.hbox(
					ZK.imagebutton("/images/icons/information_grey.png").
						tooltip("Show the information page").
						listener("onClick", new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								currentPage = -1;
								render();
							}
						}),
					ZK.bar(),
					ZK.imagebutton(
							(showSequence && currentPage > 0) ?
									"/images/icons/arrow_left.png" :
									"/images/icons/arrow_left_grey.png").
						tooltip("Back to previous slide").
						listener("onClick", new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								if (showSequence && currentPage > 0) {
									currentPage --;
									render();
								}
							}
						}),
					ZK.imagebutton((showSequence && currentPage < sequence.size() - 1) ?
								"/images/icons/arrow_right.png" :
								"/images/icons/arrow_right_grey.png").
						tooltip("Forward to next slide").
						listener("onClick", new EventListener() {
							
							@Override
							public void onEvent(Event arg0) throws Exception {
								if (showSequence && currentPage < (sequence.size() - 1)) {
									currentPage ++;
									render();
								}
							}
						})
				);
		}
		return ret;
	}

	/*
	 * create the ribbon area component
	 */
	protected abstract ZKComponent getRibbon(int width, int height) throws ThinklabException;

	/*
	 * create the image area component
	 */
	protected abstract ZKComponent getImageArea(int width, int height) throws ThinklabException;

}
